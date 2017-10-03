(ns clj-aql.core
  (:require [clj-aql.spec.fn]
            [clj-aql.spec.op]
            [clojure.spec.alpha :as s]))

(defn expand-map [m value-fn]
  (str "{" (clojure.string/join ","
                                (map (fn [[k v]] (str "\"" (name k) "\":" (value-fn v))) m))
       "}"))

(defn expand-any [v]
  (cond
    (map? v) (expand-map v expand-any)
    (symbol? v) v
    (keyword? v) (name v)
    (string? v) (str "\"" v "\"")
    :else v))

(defn expand-fn [fn]
  (let [name (:name fn)
        args (vals (dissoc fn :name))]
    (str name "(" (clojure.string/join "," (map expand-any args)) ")")))

(declare expand-clause)

(defn expand-expression [[type val]]
  (case type
    :string (str "\"" val "\"")
    :symbol val
    :map (expand-map val expand-expression)
    :fn (expand-fn val)
    :for-op (str "(" (expand-clause val) ")")
    val))

(defmulti expand-clause :name)
(defmethod expand-clause 'FOR [{:keys [fields collection clauses]}]
  (apply str "FOR " (clojure.string/join "," fields) " IN " collection "\n"
         (map expand-clause clauses)))
(defmethod expand-clause 'RETURN [{:keys [expression]}]
  (str "RETURN " (expand-expression expression)))

(defn expand-operand [[type val]] (str val))
(defn expand-primitive-condition [{:keys [op-first op op-second]}]
  (str (expand-operand op-first) " " op " " (expand-operand op-second)))

(defn expand-condition [[condition-type condition]]
  (if (= condition-type :binary-op)
    (str (expand-primitive-condition (:op-first condition)) " "
         (:op condition) " "
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
                                     result (if (= type :for-op)
                                           (expand-clause val)
                                           (expand-fn val))]]
                           (str "LET " binding " = (\n" result "\n)")))
    "\n"))

(defmethod expand-clause :default [_] "")

(defmacro FOR [& args]
  (let [form (s/conform :clj-aql.spec.op/for-op (cons 'FOR args))]
    (expand-clause form)))

(defmacro RETURN [& args]
  (expand-clause (s/conform :clj-aql.spec.op/return-op (cons 'RETURN args))))