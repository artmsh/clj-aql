(ns clj-aql.core
  (:require [clj-aql.spec.fn]
            [clj-aql.spec.op]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(defn expand-map [m value-fn quote-key?]
  (str "{" (clojure.string/join ","
                                (map (fn [[k v]]
                                       (let [key (if quote-key? (str "\"" (name k) "\"") k)]
                                        (str key ":" (value-fn v))))
                                     m))
       "}"))

(declare expand-expression)

(defn expand-any [v]
  (cond
    (map? v) (expand-map v expand-any true)
    (symbol? v) v
    (keyword? v) (name v)
    (string? v) (str "\"" v "\"")
    ; hack for fn args
    (and (vector? v) (= (count v) 2) (keyword? (first v))) (expand-expression v)
    :else v))

(defn expand-fn [fn]
  (let [name (:name fn)
        args (vals (dissoc fn :name))]
    (str name "(" (clojure.string/join "," (map expand-any args)) ")")))

(declare expand-clause)

(defn expand-expression [[type val]]
  (case type
    ;:string (str "\"" val "\"")
    :string (symbol val)
    :symbol val
    :map (expand-map val expand-expression true)
    :map-s (expand-map val expand-expression false)
    :fn (expand-fn val)
    :for-op (str "(" (expand-clause val) ")")
    val))

(defmulti expand-clause :name)
(defmethod expand-clause 'FOR [{:keys [fields collection clauses]}]
  (let [coll (if (= :string (first collection))
               (second collection)
               (str "(" (expand-clause (second collection)) ")"))]
    (apply str "FOR " (clojure.string/join "," fields) " IN " coll "\n"
           (map expand-clause clauses))))

(defmethod expand-clause 'RETURN [{:keys [expression]}]
  (str "RETURN " (expand-expression expression)))

(defn expand-operand [[type val]]
  (if (= type :quoted)
    (str "@" (:val val))
    (str val)))
(defn expand-primitive-condition [{:keys [op-first op op-second]}]
  (str (expand-operand op-first) " " (expand-any op) " " (expand-operand op-second)))

(defn expand-condition [[condition-type condition]]
  (if (= condition-type :binary-op)
    (str (expand-primitive-condition (:op-first condition)) " "
         (expand-any (:op condition)) " "
         (expand-primitive-condition (:op-second condition)))
    (expand-primitive-condition condition)))

(defmethod expand-clause 'FILTER [{:keys [condition]}]
  (str "FILTER " (expand-condition condition) "\n"))

(defmethod expand-clause 'SORT [{:keys [expression direction]}]
  (str "SORT " (clojure.string/join "," expression) " " (name direction) "\n"))

(defmethod expand-clause 'LIMIT [{:keys [offset count]}]
  (str "LIMIT " (clojure.string/join "," (filter (complement nil?) [offset count])) "\n"))

(defmethod expand-clause 'LET [{:keys [bindings]}]
  (str
    (clojure.string/join "\n"
                         (for [{:keys [binding expression]} bindings
                               :let [[type val] expression
                                     result (cond
                                              (= type :for-op) (expand-clause val)
                                              (= type :fn) (expand-fn val)
                                              (= type :string) (symbol val)
                                              :else val)]]
                           (str "LET " binding " = (\n" result "\n)")))
    "\n"))

(defmethod expand-clause 'COLLECT [{:keys [vars into-clause keep-clause]}]
  (str "COLLECT " (clojure.string/join ","
                                       (map #(str (:variable-name %) " = " (expand-expression (:expression %))) vars))
       (if into-clause
         (str " INTO " (:groups-variable into-clause))
         "")
       (if keep-clause
         (str " KEEP " (:keep-variable keep-clause))
         "")
       "\n"))

(defmethod expand-clause :default [_] "")

(defmacro FOR [& args]
  (let [form (s/conform :clj-aql.spec.op/for-op (cons 'FOR args))
        _ (prn form)]
    {:query (expand-clause form)
     :args (into {} (for [n (tree-seq coll? seq args)
                    :when (and (coll? n) (= (first n) `unquote))]
                  [(str (second n)) (second n)]))}))

(defmacro RETURN [& args]
  {:query (expand-clause (s/conform :clj-aql.spec.op/return-op (cons 'RETURN args)))
   :args (into {} (for [n (tree-seq coll? seq args)
                    :when (and (coll? n) (= (first n) `unquote))]
                [(str (second n)) (second n)]))})

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