(ns clj-aql.core
  (:require [clj-aql.spec.fn :as fns]
            [clj-aql.spec.op :as ops]
            [clojure.string :as string]
            [cheshire.core :as json]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(def OR (symbol "OR"))

(defn coll-join [sep coll]
  (drop-last (mapcat vector coll (repeat sep))))

(defn expand-map [m value-fn quote-key?]
  (concat ["{"] (flatten (coll-join ","
                                (map (fn [[k v]]
                                       (let [key (if quote-key? (str "\"" (name k) "\"") k)]
                                        (concat [key] [":"] (value-fn v))))
                                     m)))
       ["}"]))

(declare expand-expression)

(defn expand-any [v]
  (cond
    (map? v) (expand-map v expand-any true)
    (symbol? v) (list (str v))
    (keyword? v) (list (name v))
    (string? v) (list "\"" v "\"")
    ; hack for fn args
    (and (vector? v) (= (count v) 2) (keyword? (first v))) (expand-expression v)
    (sequential? v) v
    :else (list v)))

(defn expand-fn [fn]
  (let [name (:name fn)
        args (vals (dissoc fn :name))]
    (concat [(str name)] ["("] (flatten (coll-join "," (map expand-any args))) [")"])))

(declare expand-clause1)

(defn expand-expression [[type val]]
  (case type
    :string (list (str "\"" val "\""))
    :symbol (list (str val))
    :quoted [(str "@" (:val val))]
    :map (expand-map val expand-expression true)
    :map-s (expand-map val expand-expression false)
    :fn (expand-fn val)
    :for-op (list "(" (expand-clause1 val) ")")
    :varargs (list (clojure.string/join "," (map (comp (partial clojure.string/join "") expand-any) val)))
    :array (list "[" (clojure.string/join "," (map (comp (partial clojure.string/join "") expand-any) val)) "]")
    (list val)))

(defmulti expand-clause :name)
(defmethod expand-clause 'FOR [{:keys [fields collection clauses]}]
  (let [coll (case (first collection)
               :string (list (second collection))
               :symbol (list (second collection))
               :fn (expand-fn (second collection))
               (concat ["("] (expand-clause1 (second collection)) [")"]))]
    (concat ["FOR "] (coll-join "," (map str fields)) [" IN "] coll ["\n"]
           (mapcat expand-clause1 clauses))))

(defmethod expand-clause 'RETURN [{:keys [expression]}]
  (cons "RETURN " (expand-expression expression)))

(defn expand-operand [[type val]]
  (case type
    :quoted [(str "@" (:val val))]
    :fn (expand-fn val)
    [(str val)]))

(defn expand-primitive-condition [{:keys [op-first op op-second]}]
  (concat (expand-operand op-first) [" "] (expand-any op) [" "] (expand-operand op-second)))

(defn expand-n-ary-condition [{:keys [op-logical op-cond]}]
  (concat [" "] (expand-any op-logical) [" "] (expand-primitive-condition op-cond)))

(defn expand-condition [[condition-type condition]]
  (case condition-type
    :symbol (list condition)
    :n-ary-op (concat
               (expand-primitive-condition (:op-first condition))
               (flatten (coll-join " " (map expand-n-ary-condition (:op-n-ary condition)))))
    (expand-primitive-condition condition)))

(defmethod expand-clause 'FILTER [{:keys [condition]}]
  (concat ["FILTER "] (expand-condition condition) ["\n"]))

(defmethod expand-clause 'SORT [{:keys [expression direction]}]
  (concat ["SORT "] (flatten (coll-join "," (map expand-any expression))) [" " (name direction) "\n"]))

(defmethod expand-clause 'LIMIT [{:keys [offset count]}]
  (concat ["LIMIT "] (flatten (coll-join "," (->> [offset count]
                                              (filter (complement nil?))
                                              (map expand-operand)))) ["\n"]))

(defmethod expand-clause 'LET [{:keys [bindings]}]
  (concat
    (flatten
      (coll-join "\n"
                         (for [{:keys [binding expression]} bindings
                               :let [[type val] expression
                                     result (case type
                                              :for-op (concat ["(\n"] (expand-clause1 val) ["\n)"])
                                              :fn (expand-fn val)
                                              :string (list val)
                                              (list val))]]
                           (concat ["LET "] (expand-any binding) [" = "] result))))
    ["\n"]))

(defmethod expand-clause 'COLLECT [{:keys [vars into-clause keep-clause]}]
  (concat ["COLLECT "]
          (flatten (coll-join "," (map #(concat
                                          (expand-any (:variable-name %))
                                          [" = "]
                                          (expand-expression (:expression %))) vars)))
       (if into-clause
         (cons " INTO " (expand-any (:groups-variable into-clause)))
         [""])
       (if keep-clause
         (cons " KEEP " (expand-any (:keep-variable keep-clause)))
         [""])
       ["\n"]))

(defmethod expand-clause 'INSERT [{:keys [document into collection options-clause let-clauses return-clause]}]
  (concat ["INSERT "]
          (case (first document)
            :string (list (second document))
            :symbol (list (second document)))
          (list " " (name into) " ")
          (case (first collection)
            :string (list (second collection))
            :symbol (list (second collection)))
          (if-not (nil? options-clause)
            [" OPTIONS " (json/generate-string (:options options-clause))]
            [])
          (if-not (empty? let-clauses)
            (mapcat expand-clause1 let-clauses)
            [])
          (if-not (nil? return-clause)
            [" RETURN " (:op return-clause)]
            [])
          #_["\n"]))

(defmethod expand-clause :default [clause]
  (if (map? clause)
    (cond
      ;; expand function with FOR inside
      (not (nil? (:name clause))) (expand-fn clause)
      :else "")
    ""))

(defn expand-clause1 [{:keys [name args]}]
  (expand-clause (merge {:name name} args)))

(defn- expand-with-symbol [spec sym args]
  (let [form (s/conform spec (cons sym args))
        _ (when (= form ::s/invalid) (prn (s/explain-str spec (cons sym args))))
        _ (prn "FORM: " form)]


    {:query (list 'apply 'str
                  (cons 'list (expand-clause1 form)))
     :args (into {} (for [n (tree-seq coll? seq args)
                          :when (and (coll? n) (= (first n) `unquote))]
                      [(str (second n)) (second n)]))}))

(defn is-op-node? [node-key]
  (some #(= (keyword "clj-aql.spec.op" (name node-key)) (key %)) (s/registry)))

(defn is-fn-node? [node-key]
  (some #(= (keyword "clj-aql.spec.fn" (name node-key)) (key %)) (s/registry)))

(declare emit)

(defn expand-op [op-map]
  ;(prn "op-map: " op-map)
  (let [emitted (vec (map emit (seq op-map)))]
    (list 'str "(" (list 'clojure.string/join " " emitted) ")")))

(defn expand-fn [fn-map]
  ;(prn fn-map)
  (let [fn-name (str (:str-symbol fn-map))
        fn-args-map (dissoc fn-map :str-symbol)
        emitted (vec (walk/postwalk emit (seq fn-args-map)))]
    (list 'str fn-name "(" (list 'clojure.string/join "," emitted) ")")))

(defn emit [node]
  ;(prn node)

  (if (and (vector? node) (= (count node) 2) (keyword? (first node)))
    (cond
      (= (first node) :str-symbol) (str (second node))
      (= (first node) :ref-symbol-number) (second node)
      (= (first node) :ref-string) (second node)
      (= (first node) :literal-string) (str "\"" (second node) "\"")
      (= (first node) :literal-array) (json/generate-string (second node))
      (= (first node) :literal-map) (str/replace (json/generate-string (second node)) "\"" "")
      (= (first node) :literal-tuple) (str/join "," (second node))
      (= (first node) :ref-symbol-tuple) (list 'clojure.string/join "," (second node))
      (= (first node) :kw) (name (second node))
      (= (first node) :ref-symbol-sequential) (list 'cheshire.core/generate-string (second node))
      (= (first node) :binary-expression) (let [{:keys [operator rvalue lvalue]} (second node)]
                                            (str lvalue operator rvalue))
      (= (first node) :multi-expression) (let [{:keys [operator rvalue lvalue]} (second node)]
                                           (str/join (str " " operator " ")
                                               (cons (emit lvalue) (map emit rvalue))))
      (= (first node) :assignment-expression) (let [{:keys [rvalue lvalue]} (second node)]
                                                (list 'str lvalue "=" (emit rvalue)))
      (is-op-node? (first node)) (expand-op (second node))
      (is-fn-node? (first node)) (expand-fn (second node))
      :else (emit (second node)))
    node))

(defn op-expansion [spec form]
  (if (s/valid? spec form)
    (let [exp (s/conform spec (seq form))
          _ (prn "op-exp: " exp)
          emitted (map emit (seq exp))]
      (list 'clojure.string/join " " (vec emitted)))
    (s/explain spec form)))


(defn fn-expansion [spec form fn]
  (if (s/valid? spec (cons fn form))
    (let [exp (s/conform spec (seq (cons fn form)))
          ;_ (prn exp)
          emitted (map emit (seq exp))]
      (list 'str (str fn) "("
        (list 'clojure.string/join "," (vec emitted)) ")"))
    (s/explain spec (cons fn form))))

(defn hlo-expansion [form]
  (let [exp (s/conform ::ops/high-level-op form)
        emitted (map emit (seq (second exp)))]
    emitted))

(defn get-args-spec [spec]
  (eval (cons 'clojure.spec.alpha/cat (drop 2 (rest (s/form spec))))))

(defmacro FOR [& args] (op-expansion ::ops/for-op (cons 'FOR args)))
;(s/fdef FOR :args (get-args-spec ::ops/for-op) :ret string?)

(defmacro RETURN [& args] (op-expansion ::ops/return-op (cons 'RETURN args)))
;(s/fdef RETURN :args (get-args-spec ::ops/return-op) :ret string?)

(defmacro FILTER [& args] (op-expansion ::ops/filter-op (cons 'FILTER args)))
;(s/fdef FILTER :args (get-args-spec ::ops/filter-op) :ret string?)

(defmacro SORT [& args] (op-expansion ::ops/sort-op (cons 'SORT args)))
;(s/fdef SORT :args (get-args-spec ::ops/sort-op) :ret string?)

(defmacro LIMIT [& args] (op-expansion ::ops/limit-op (cons 'LIMIT args)))
;(s/fdef LIMIT :args (get-args-spec ::ops/limit-op) :ret string?)

(defmacro LET [& args] (op-expansion ::ops/let-op (cons 'LET args)))
;(s/fdef LET :args (get-args-spec ::ops/let-op) :ret string?)

(defmacro FLATTEN [& args] (fn-expansion ::fns/flatten-fn args 'FLATTEN))
;(s/fdef FLATTEN :args (get-args-spec ::fns/flatten-fn) :ret string?)

(defmacro DOCUMENT [& args] (fn-expansion ::fns/document-fn args 'DOCUMENT))
;(s/fdef DOCUMENT :args (get-args-spec ::fns/document-fn) :ret string?)

;(defmacro RETURN [& args] (expand-with-symbol :clj-aql.spec.op/return-op 'RETURN args))
;(defmacro INSERT [& args] (expand-with-symbol :clj-aql.spec.op/insert-op 'INSERT args))



;(s/fdef RETURN :args :clj-aql.spec.op/return-op-args
;               :ret string?)
;
;(s/fdef INSERT :args :clj-aql.spec.op/insert-op-args
;               :ret string?)

#_(stest/instrument [`FOR `RETURN `FILTER `SORT `LIMIT `LET `FLATTEN `DOCUMENT])

