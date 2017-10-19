(ns clj-aql.spec.fn
  (:require [clojure.spec.alpha :as s]))

(s/def ::document map?)
(s/def ::_id string?)
(s/def ::indexed-document (s/and map? (s/keys :req-un [::_id])))

(defmulti document-function first)
(defmethod document-function 'ATTRIBUTES [_] (s/cat :name #{'ATTRIBUTES}
                                                    :document ::document
                                                    :remove-internal (s/? boolean?)
                                                    :sort (s/? boolean?)))
(defmethod document-function 'COUNT [_] (s/cat :name #{'COUNT}
                                               :doc symbol?))
(defmethod document-function 'HAS [_] (s/cat :name #{'HAS}
                                             :document ::document
                                             :attribute-name string?))
(defmethod document-function 'IS_SAME_COLLECTION [_] (s/cat :name #{'IS_SAME_COLLECTION}
                                                            :collection-name string?
                                                            :document-handle (s/or :string string?
                                                                                   :kw ::indexed-document)))
(defmethod document-function 'KEEP [_] (s/cat :name #{'KEEP}
                                              :attribute-names (s/or :varargs (s/* string?)
                                                                     :array (s/coll-of string? :kind vector?))))

(defmethod document-function 'LENGTH [_] (s/cat :name #{'LENGTH}
                                                :doc symbol?))
(defmethod document-function 'FLATTEN [_] (s/cat :name #{'FLATTEN}
                                                 :doc (s/or :symbol symbol?
                                                            :for-op  :clj-aql.spec.op/for-op)
                                                 ))
(defmethod document-function 'APPEND [_] (s/cat :name #{'APPEND}
                                                :doc symbol?))
(defmethod document-function 'FIRST [_] (s/cat :name #{'FIRST}
                                               :doc symbol?))
(defmethod document-function 'LAST [_] (s/cat :name #{'LAST}
                                              :doc symbol?))
(defmethod document-function 'INTERSECTION [_] (s/cat :name #{'INTERSECTION}
                                                      :doc symbol?))
(defmethod document-function 'MINUS [_] (s/cat :name #{'MINUS}
                                               :doc symbol?))
(defmethod document-function 'NTH [_] (s/cat :name #{'NTH}
                                             :doc symbol?))
(defmethod document-function 'OUTERSECTION [_] (s/cat :name #{'OUTERSECTION}
                                                      :doc symbol?))
(defmethod document-function 'POSITION [_] (s/cat :name #{'POSITION}
                                                  :doc symbol?))
(defmethod document-function 'PUSH [_] (s/cat :name #{'PUSH}
                                              :doc symbol?))
(defmethod document-function 'REMOVE_NTH [_] (s/cat :name #{'REMOVE_NTH}
                                                    :doc symbol?))
(defmethod document-function 'REMOVE_VALUE [_] (s/cat :name #{'REMOVE_VALUE}
                                                      :doc symbol?))
(defmethod document-function 'REMOVE_VALUES [_] (s/cat :name #{'REMOVE_VALUES}
                                                       :doc symbol?))
(defmethod document-function 'REVERSE [_] (s/cat :name #{'REVERSE}
                                                 :doc symbol?))
(defmethod document-function 'SHIFT [_] (s/cat :name #{'SHIFT}
                                               :doc symbol?))
(defmethod document-function 'UNSHIFT [_] (s/cat :name #{'UNSHIFT}
                                                 :doc symbol?))
(defmethod document-function 'SLICE [_] (s/cat :name #{'SLICE}
                                               :doc symbol?))
(defmethod document-function 'UNION [_] (s/cat :name #{'UNION}
                                               :doc symbol?))
(defmethod document-function 'UNION_DISTINCT [_] (s/cat :name #{'UNION_DISTINCT}
                                               :doc symbol?))
(defmethod document-function 'UNIQUE [_] (s/cat :name #{'UNIQUE}
                                                :doc symbol?))


(defmethod document-function 'MATCHES [_] (s/cat :name #{'MATCHES}
                                                 :doc ::document
                                                 :examples (s/or :doc ::document
                                                                 :docs (s/and (s/coll-of ::document :kind vector?)
                                                                              #(not-empty %)))
                                                 :return-index (s/? boolean?)))
(defmethod document-function 'MERGE [_] (s/cat :name #{'MERGE}
                                               :doc ::document))

(defmethod document-function 'DOCUMENT [_] (s/cat :name #{'DOCUMENT}
                                                  :collection (s/? string?)
                                                  :id (s/or :string string?
                                                            :symbol symbol?
                                                            :array (s/* string?))))

(s/def ::function (s/multi-spec document-function first))
