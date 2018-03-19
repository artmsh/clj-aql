(ns clj-aql.spec.base
  (:require [clojure.spec.alpha :as s]))

(s/def ::document (s/or :map map?
                        :fn :clj-aql.spec.fn/function))
(s/def ::_id string?)
(s/def ::indexed-document (s/and map? (s/keys :req-un [::_id])))

(s/def ::binary-operator #{'== '!=})
(s/def ::n-ary-operator #{'OR 'AND})

(s/def ::bool-expression
  (s/or :in/n-ary-expression (s/cat :operator ::n-ary-operator
                                 :lvalue ::bool-expression
                                 :rvalue (s/+ ::bool-expression))
        :in/binary-expression (s/cat :operator ::binary-operator
                                  :lvalue ::any-expression
                                  :rvalue ::any-expression)
        :in/s string?
        :in/bool boolean?))

(s/def ::array-expression (s/or :in/bracket-op :clj-aql.spec.op/bracket-for-op
                                 :in/vec (s/coll-of any? :kind vector?)
                                 :in/s-var symbol?))

(s/def ::any-expression (s/or
                          :in/num number?
                           :in/s string?
                          :array-exp ::array-expression
                               :bool-exp ::bool-expression
                               :map-exp ::map-expression
                               :in/fn :clj-aql.spec.fn/inner-function))


(s/def ::map-expression (s/or :in/map (s/map-of (s/or :in/s string?
                                                       :kw keyword?)
                                                ::any-expression)))