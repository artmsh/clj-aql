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
                                                            :document-handle (s/or :s string?
                                                                                   :kw ::indexed-document)))
(defmethod document-function 'KEEP [_] (s/cat :name #{'KEEP}
                                              :attribute-names (s/or :varargs (s/* string?)
                                                                     :array (s/coll-of string? :kind vector?))))

(defmethod document-function 'LENGTH [_] (s/cat :name #{'LENGTH}
                                                :doc symbol?))
(defmethod document-function 'MATCHES [_] (s/cat :name #{'MATCHES}
                                                 :doc ::document
                                                 :examples (s/or :doc ::document
                                                                 :docs (s/and (s/coll-of ::document :kind vector?)
                                                                              #(not-empty %)))
                                                 :return-index (s/? boolean?)))
(defmethod document-function 'MERGE [_] (s/cat :name #{'MERGE}
                                               :doc ::document))

(s/def ::function (s/multi-spec document-function first))
