(ns clj-aql.core
  (:require [clj-aql.spec.fn]
            [clj-aql.spec.op]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [cheshire.core :as json]))

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
    :else (list v)))

(defn expand-fn [fn]
  (let [name (:name fn)
        args (vals (dissoc fn :name))]
    (concat [(str name)] ["("] (flatten (coll-join "," (map expand-any args))) [")"])))

(declare expand-clause1)

(defn expand-expression [[type val]]
  (case type
    :string (list val)
    :symbol (list (str val))
    :quoted [(str "@" (:val val))]
    :map (expand-map val expand-expression true)
    :map-s (expand-map val expand-expression false)
    :fn (expand-fn val)
    :for-op (list "(" (expand-clause1 val) ")")
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
            (expand-clause1 return-clause)
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
        ;_ (prn "FORM: " form)
        ]
    {:query (list 'apply 'str
                  (cons 'list (expand-clause1 form)))
     :args (into {} (for [n (tree-seq coll? seq args)
                          :when (and (coll? n) (= (first n) `unquote))]
                      [(str (second n)) (second n)]))}))

(defmacro FOR [& args] (expand-with-symbol :clj-aql.spec.op/for-op 'FOR args))
(defmacro RETURN [& args] (expand-with-symbol :clj-aql.spec.op/return-op 'RETURN args))
(defmacro INSERT [& args] (expand-with-symbol :clj-aql.spec.op/insert-op 'INSERT args))

(s/fdef FOR :args :clj-aql.spec.op/for-op-args
            :ret string?)

(s/fdef RETURN :args :clj-aql.spec.op/return-op-args
               :ret string?)

(s/fdef INSERT :args :clj-aql.spec.op/insert-op-args
               :ret string?)

(stest/instrument [`FOR `RETURN `INSERT])
