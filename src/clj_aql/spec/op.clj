(ns clj-aql.spec.op
  (:require [clojure.spec.alpha :as s]
            [clj-aql.spec.fn]
            [clojure.spec.gen.alpha :as gen]))


(s/def ::primitive-num (s/or
                        :num number?
                        :quoted (s/cat :q #{`unquote} :val any?)))

(s/def ::primitive-operand (s/or :sym symbol?
                                 :num number?
                                 :bool boolean?
                                 :str string?
                                 :quoted (s/cat :q #{`unquote} :val any?)))

(s/def ::condition-op #{'== '!= '< '<= '> '>= :IN :NOT-IN :LIKE '&& '|| '!})

(s/def ::primitive-condition (s/cat :op-first ::primitive-operand
                                    :op ::condition-op
                                    :op-second ::primitive-operand))

(s/def ::condition
  (s/alt :binary-op (s/cat :op-first ::primitive-condition
                           :op ::condition-op
                           :op-second ::primitive-condition)
         :ternary-op (s/cat :op-first ::primitive-condition
                            :op ::condition-op
                            :op-second ::primitive-condition
                            :op ::condition-op
                            :op-third ::primitive-condition)
         :primitive ::primitive-condition))

(defmulti high-level-op first)
(s/def ::high-level-op (s/multi-spec high-level-op first))

(s/def ::for-op (s/cat :name #{'FOR}
                       :fields (s/coll-of symbol? :kind vector?)
                       :in #{:IN}
                       :collection (s/or :for-op ::for-op
                                         :string string?)
                       :clauses (s/* ::high-level-op)))

(s/def ::let-expr (s/or
                    :symbol symbol?
                    :string string?
                    :fn :clj-aql.spec.fn/function
                    :for-op ::for-op))

(s/def ::expression (s/or :string string?
                          :symbol symbol?
                          :map (s/map-of keyword? ::expression)
                          :map-s (s/map-of string? ::expression)
                          :fn :clj-aql.spec.fn/function
                          :for-op ::for-op))

(s/def ::return-expr ::expression)

(s/def ::return-op (s/cat :name #{'RETURN}
                          :expression ::return-expr))

(defmethod high-level-op 'FOR [_] ::for-op)
(defmethod high-level-op 'RETURN [_] ::return-op)
(defmethod high-level-op 'FILTER [_] (s/cat :name #{'FILTER}
                                            :condition ::condition))
(defmethod high-level-op 'SORT [_] (s/cat :name #{'SORT}
                                          :expression (s/coll-of symbol? :kind vector?)
                                          :direction (s/? #{:ASC :DESC})))
(defmethod high-level-op 'LIMIT [_] (s/cat :name #{'LIMIT}
                                           :offset (s/? ::primitive-num)
                                           :count ::primitive-num))
(defmethod high-level-op 'LET [_] (s/cat :name #{'LET}
                                         :bindings (s/and
                                                    vector?
                                                    (s/* (s/cat :binding symbol?
                                                                :expression ::let-expr)))))
(defmethod high-level-op 'COLLECT [_] (s/cat :name #{'COLLECT}
                                             :vars (s/and
                                                     vector?
                                                     (s/* (s/cat :variable-name symbol?
                                                                 :expression ::expression)))
                                             :into-clause (s/? (s/cat :into #{:INTO}
                                                                      :groups-variable symbol?))
                                             :keep-clause (s/? (s/cat :keep #{:KEEP}
                                                                      :keep-variable symbol?))))
(defmethod high-level-op 'REMOVE [_] (s/cat :name #{'REMOVE}))
(defmethod high-level-op 'UPDATE [_] (s/cat :name #{'UPDATE}))
(defmethod high-level-op 'REPLACE [_] (s/cat :name #{'REPLACE}))
(defmethod high-level-op 'INSERT [_] (s/cat :name #{'INSERT}))
(defmethod high-level-op 'UPSERT [_] (s/cat :name #{'UPSERT}))
