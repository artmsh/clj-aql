(ns clj-aql.spec.op
  (:require [clj-aql.spec.base :as base]
            [clj-aql.spec.fn :as fns]
            [clojure.spec.alpha :as s]))

; literal: edn-array: [1 2 {:a 3}]
; literal: edn-object: {:a 1, :b [1 2 3]}
; ref: string: 'users' (collection in database), 'u' (binding from another FOR or LET)
; ref: symbol: will be resolved during macroexpansion: users => (let [users "users"] ...)


; literal array will be expanded to json representation
; literal map will be expanded to json representation

; comma-sep expands inner content to [symb1], [symb2], [symb3]
; str-symbol: '.. to ..

(s/def ::for-op (s/cat
                  :str-symbol #{'FOR}
                  :field (s/or :ref-string string?
                               :ref-symbol symbol?)
                  :kw #{:IN}
                  :collection (s/or :ref-string string?
                                    :for-op ::for-op
                                    :ref-symbol symbol?
                                    :fn ::fns/function
                                    :literal-array (s/coll-of any? :kind vector?))
                  :clauses (s/* ::high-level-op)))

(s/def ::return-op
  (s/cat :str-symbol #{'RETURN}
         :expression (s/or :ref-string string?
                           :literal-map (s/map-of any? any?)
                           :fn ::fns/function
                           :for-op ::for-op)))

(s/def ::filter-op
  (s/cat :str-symbol #{'FILTER}
         :condition (s/or :ref-symbol symbol?
                          :expression ::base/filter-expression)))

(s/def ::sort-op
  (s/cat :str-symbol #{'SORT}
         :literal-tuple (s/+ string?)
         :kw (s/? #{:ASC :DESC})))

(s/def ::limit-op
  (s/cat :str-symbol #{'LIMIT}
         :args (s/alt :literal-tuple (s/+ number?)
                      :ref-symbol-tuple (s/+ symbol?))))

(s/def ::let-op
  (s/cat :str-symbol #{'LET}
         :assignment-expression (s/cat :lvalue string?
                                       :rvalue (s/or
                                                 :ref-string string?
                                                 :fn ::fns/function
                                                 :for-op ::for-op))))

;(s/def ::primitive-num (s/or
;                         :num number?
;                         :quoted (s/cat :q #{`unquote} :val any?)))
;
;(s/def ::primitive-operand (s/or :sym symbol?
;                                 :num number?
;                                 :bool boolean?
;                                 :str string?
;                                 :fn :clj-aql.spec.fn/function
;                                 :quoted (s/cat :q #{`unquote} :val any?)))
;
;(s/def ::condition-op #{'== '!= '< '<= '> '>= :IN :NOT-IN :LIKE '!})
;(s/def ::logical-op #{'&& '|| '!})
;
;(s/def ::primitive-condition (s/cat
;                               :op-first ::primitive-operand
;                               :op ::condition-op
;                               :op-second ::primitive-operand))
;
;
;(s/def ::condition
;  (s/alt
;    :symbol symbol?
;    :n-ary-op (s/cat :op-first ::primitive-condition
;                     :op-n-ary (s/+ (s/cat :op-logical ::logical-op :op-cond ::primitive-condition)))
;    :primitive ::primitive-condition))
;
;(defmulti high-level-op first)
(s/def ::high-level-op (s/alt
                         :for-op ::for-op
                         :return-op ::return-op
                         :filter-op ::filter-op
                         :sort-op ::sort-op
                         :limit-op ::limit-op
                         :let-op ::let-op))
;
;(s/def ::for-op-doc-args
;  (s/cat
;    :str-symbol 'FOR
;    :field (s/or :ref-string string?
;                      :ref-symbol symbol?)
;                            :kw #{:IN}
;                            :collection (s/or :ref-string string?
;                                              :ref-symbol-sequential symbol?
;                                              :literal-array (s/coll-of any?)
;                                              :for-op ::for-op
;                                              :fn :clj-aql.spec.fn/function)
;                            :clauses (s/* ::high-level-op)))
;
;(s/def ::edge (s/cat
;                :any-cl (s/? (s/or :kw #{:ANY}))
;                :ref-string string?))
;
;(s/def ::for-op-graph-args
;  (s/cat :comma-sep (s/coll-of (s/or :ref-string string?) :kind vector? :min-count 1 :max-count 3)
;         :kw #{:IN}
;         :min (s/? (s/alt :num nat-int?))
;         :max (s/? (s/cat :str-symbol #{'..} :num nat-int?))
;         :dir (s/alt :kw #{:OUTBOUND :INBOUND :ANY})
;         ; TODO add support for document
;         :start-vertex (s/alt :ref-string string?)
;         :graph-or-edge-colls (s/alt :graph (s/cat :kw #{:GRAPH} :ref-string string?)
;                                     :edge-colls (s/coll-of ::edge
;                                                            :kind vector? :min-count 1))
;         :opts (s/? (s/cat :kw #{:OPTIONS} :literal-map map?))
;         :clauses (s/* ::high-level-op)
;         ))
;
;(s/def ::for-op-args (s/alt
;                       :for-op-graph-args ::for-op-graph-args
;                       :for-op-doc-args ::for-op-doc-args
;                       )
;)
;
;
;(s/def ::expression (s/or :string string?
;                          :symbol symbol?
;                          :map (s/map-of keyword? ::expression)
;                          :map-s (s/map-of string? ::expression)
;                          :fn :clj-aql.spec.fn/function
;                          :for-op ::for-op))
;
;(s/def ::return-expr ::expression)
;
;(s/def ::return-op-args (s/cat :expression ::return-expr))
;(s/def ::return-op (s/cat :name #{'RETURN}
;                          :args ::return-op-args))
;
;(s/def ::let-op (s/cat :name #{'LET}
;                       :args (s/cat
;                               :bindings (s/and
;                                           vector?
;                                           (s/* (s/cat :binding symbol?
;                                                       :expression ::let-expr))))))
;
;(s/def ::insert-op-args (s/cat :document (s/or :string string?
;                                               :symbol symbol?)
;                               :into #{:INTO :IN}
;                               :collection (s/or :string string?
;                                                 :symbol symbol?)
;                               :options-clause
;                               (s/? (s/cat :options-marker #{:OPTIONS}
;                                           :options (s/map-of keyword? any?)))
;                               ; TODO ::let-op with NEW ::return-op with NEW
;                               ;:let-clauses (s/? (s/* ::let-op))
;                               :return-clause (s/? (s/cat :return-marker #{:RETURN}
;                                                          :op string?))))
;
;(s/def ::insert-op (s/cat :name #{'INSERT}
;                          :args ::insert-op-args))
;
;(defmethod high-level-op 'FOR [_] ::for-op)
;(defmethod high-level-op 'RETURN [_] ::return-op)
;(defmethod high-level-op 'INSERT [_] ::insert-op)
;(defmethod high-level-op 'FILTER [_] (s/cat :name #{'FILTER}
;                                            :args (s/cat :condition ::condition)))
;(defmethod high-level-op 'SORT [_] (s/cat :name #{'SORT}
;                                          :args (s/cat
;                                                  :expression (s/coll-of symbol? :kind vector?)
;                                                  :direction (s/? #{:ASC :DESC}))))
;(defmethod high-level-op 'LIMIT [_] (s/cat :name #{'LIMIT}
;                                           :args (s/cat
;                                                   :offset (s/? ::primitive-num)
;                                                   :count ::primitive-num)))
;(defmethod high-level-op 'LET [_] ::let-op)
;(defmethod high-level-op 'COLLECT [_] (s/cat :name #{'COLLECT}
;                                             :args (s/cat
;                                                     :vars (s/and
;                                                             vector?
;                                                             (s/* (s/cat :variable-name symbol?
;                                                                         :expression ::expression)))
;                                                     :into-clause (s/? (s/cat :into #{:INTO}
;                                                                              :groups-variable symbol?))
;                                                     :keep-clause (s/? (s/cat :keep #{:KEEP}
;                                                                              :keep-variable symbol?)))))
;(defmethod high-level-op 'REMOVE [_] (s/cat :name #{'REMOVE}))
;(defmethod high-level-op 'UPDATE [_] (s/cat :name #{'UPDATE}))
;(defmethod high-level-op 'REPLACE [_] (s/cat :name #{'REPLACE}))
;(defmethod high-level-op 'UPSERT [_] (s/cat :name #{'UPSERT}))
