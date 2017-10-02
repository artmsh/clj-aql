(ns clj-aql.spec.macros
  (:require [clojure.spec.alpha :as s]
            [clj-aql.core :as core]))

(s/fdef core/FOR
        :args (s/cat :fields (s/coll-of symbol? :kind vector?)
                     :in #{:IN}
                     :collection string?
                     :clauses (s/* :clj-aql.spec.op/high-level-op)))

(s/fdef core/RETURN
        :args (s/cat :expression :clj-aql.spec.op/return-expr)
        :ret string?)