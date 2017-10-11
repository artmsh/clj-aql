(ns clj-aql.core
  (:require [clj-aql.spec.fn]
            [clj-aql.spec.op]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

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

(declare expand-clause)

(defn expand-expression [[type val]]
  (case type
    :string (list val)
    :symbol (list (str val))
    :map (expand-map val expand-expression true)
    :map-s (expand-map val expand-expression false)
    :fn (expand-fn val)
    :for-op (list "(" (expand-clause val) ")")
    (list val)))

(defmulti expand-clause :name)
(defmethod expand-clause 'FOR [{:keys [fields collection clauses]}]
  (let [coll (if (= :string (first collection))
               (list (second collection))
               (concat ["("] (expand-clause (second collection)) [")"]))]
    (concat ["FOR "] (coll-join "," (map str fields)) [" IN "] coll ["\n"]
           (mapcat expand-clause clauses))))

(defmethod expand-clause 'RETURN [{:keys [expression]}]
  (cons "RETURN " (expand-expression expression)))

(defn expand-operand [[type val]]
  (if (= type :quoted)
    (str "@" (:val val))
    (str val)))

(defn expand-primitive-condition [{:keys [op-first op op-second]}]
  (concat [(expand-operand op-first) " "] (expand-any op) [" " (expand-operand op-second)]))

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
                                     result (cond
                                              (= type :for-op) (concat ["(\n"] (expand-clause val) ["\n)"])
                                              (= type :fn) (expand-fn val)
                                              (= type :string) (list val)
                                              :else (list val))]]
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

(defmethod expand-clause :default [_] "")

(defmacro FOR [& args]
  (let [form (s/conform :clj-aql.spec.op/for-op (cons 'FOR args))]
    {:query (list 'apply 'str
                  (cons 'list (expand-clause form)))
     :args (into {} (for [n (tree-seq coll? seq args)
                          :when (and (coll? n) (= (first n) `unquote))]
                      [(str (second n)) (second n)]))}))

(defmacro RETURN [& args]
  (let [form (expand-clause (s/conform :clj-aql.spec.op/return-op (cons 'RETURN args)))
        ;_ (prn (s/conform :clj-aql.spec.op/return-op (cons 'RETURN args)))
        ;_ (prn form)
        ]
    {:query (list 'apply 'str
                  (cons 'list form))
     :args (into {} (for [n (tree-seq coll? seq args)
                      :when (and (coll? n) (= (first n) `unquote))]
                  [(str (second n)) (second n)]))}))

(s/fdef FOR
        :args (s/cat :fields (s/coll-of symbol? :kind vector?)
                     :in #{:IN}
                     :collection (s/or :for-op :clj-aql.spec.op/for-op
                                       :string string?)
                     :clauses (s/* :clj-aql.spec.op/high-level-op)))

(s/fdef RETURN
        :args (s/cat :expression :clj-aql.spec.op/return-expr)
        :ret string?)

(stest/instrument [`FOR `RETURN])
