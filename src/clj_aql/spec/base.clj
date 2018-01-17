(ns clj-aql.spec.base
  (:require [clojure.spec :as s]))

(s/def ::document (s/or :map map?
                        :fn :clj-aql.spec.fn/function))
(s/def ::_id string?)
(s/def ::indexed-document (s/and map? (s/keys :req-un [::_id])))

(s/def ::binary-operator #{'== '!=})
(s/def ::n-ary-operator #{'OR 'AND})

(s/def ::filter-expression
  (s/or :multi-expression (s/cat :operator ::n-ary-operator
                                 :lvalue ::filter-expression
                                 :rvalue (s/+ ::filter-expression))
        :binary-expression (s/cat :operator ::binary-operator
                                  :lvalue any?
                                  :rvalue any?)
        :ref-string string?
        :bool-literal boolean?
        :number-literal number?))