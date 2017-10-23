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
                                 :fn :clj-aql.spec.fn/function
                                 :quoted (s/cat :q #{`unquote} :val any?)))

(s/def ::condition-op #{'== '!= '< '<= '> '>= :IN :NOT-IN :LIKE '!})
(s/def ::logical-op #{'&& '|| '!})

(s/def ::primitive-condition (s/cat
                               :op-first ::primitive-operand
                               :op ::condition-op
                               :op-second ::primitive-operand))


(s/def ::condition
  (s/alt
    :symbol symbol?
    :n-ary-op (s/cat :op-first ::primitive-condition
                     :op-n-ary (s/+ (s/cat :op-logical ::logical-op :op-cond ::primitive-condition)))
    :primitive ::primitive-condition))

(defmulti high-level-op first)
(s/def ::high-level-op (s/multi-spec high-level-op first))

(s/def ::for-op-args (s/cat :fields (s/coll-of symbol? :kind vector?)
                            :in #{:IN}
                            :collection (s/or :symbol symbol?
                                              :for-op ::for-op
                                              :fn :clj-aql.spec.fn/function
                                              :string string?)
                            :clauses (s/* ::high-level-op)))
(s/def ::for-op (s/cat :name #{'FOR}
                       :args ::for-op-args))

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

(s/def ::return-op-args (s/cat :expression ::return-expr))
(s/def ::return-op (s/cat :name #{'RETURN}
                          :args ::return-op-args))

(s/def ::let-op (s/cat :name #{'LET}
                       :args (s/cat
                               :bindings (s/and
                                           vector?
                                           (s/* (s/cat :binding symbol?
                                                       :expression ::let-expr))))))

(s/def ::insert-op-args (s/cat :document (s/or :string string?
                                               :symbol symbol?)
                               :into #{:INTO :IN}
                               :collection (s/or :string string?
                                                 :symbol symbol?)
                               :options-clause
                               (s/? (s/cat :options-marker #{:OPTIONS}
                                           :options (s/map-of keyword? any?)))
                               ; TODO ::let-op with NEW ::return-op with NEW
                               :let-clauses (s/* ::let-op)
                               :return-clause (s/? ::return-op)))

(s/def ::insert-op (s/cat :name #{'INSERT}
                          :args ::insert-op-args))

(defmethod high-level-op 'FOR [_] ::for-op)
(defmethod high-level-op 'RETURN [_] ::return-op)
(defmethod high-level-op 'INSERT [_] ::insert-op)
(defmethod high-level-op 'FILTER [_] (s/cat :name #{'FILTER}
                                            :args (s/cat :condition ::condition)))
(defmethod high-level-op 'SORT [_] (s/cat :name #{'SORT}
                                          :args (s/cat
                                                  :expression (s/coll-of symbol? :kind vector?)
                                                  :direction (s/? #{:ASC :DESC}))))
(defmethod high-level-op 'LIMIT [_] (s/cat :name #{'LIMIT}
                                           :args (s/cat
                                                   :offset (s/? ::primitive-num)
                                                   :count ::primitive-num)))
(defmethod high-level-op 'LET [_] ::let-op)
(defmethod high-level-op 'COLLECT [_] (s/cat :name #{'COLLECT}
                                             :args (s/cat
                                                     :vars (s/and
                                                             vector?
                                                             (s/* (s/cat :variable-name symbol?
                                                                         :expression ::expression)))
                                                     :into-clause (s/? (s/cat :into #{:INTO}
                                                                              :groups-variable symbol?))
                                                     :keep-clause (s/? (s/cat :keep #{:KEEP}
                                                                              :keep-variable symbol?)))))
(defmethod high-level-op 'REMOVE [_] (s/cat :name #{'REMOVE}))
(defmethod high-level-op 'UPDATE [_] (s/cat :name #{'UPDATE}))
(defmethod high-level-op 'REPLACE [_] (s/cat :name #{'REPLACE}))
(defmethod high-level-op 'UPSERT [_] (s/cat :name #{'UPSERT}))
