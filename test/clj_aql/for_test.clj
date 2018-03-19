(ns clj-aql.for-test
  (:require [clojure.test :refer :all]
            [clj-aql.core :refer :all]))

(deftest for-traversal-test
  (is (= (FOR ["v" "e" "p"] :IN 1 .. 5 :OUTBOUND "circles/A" :GRAPH "traversalGraph"
           (FILTER "p.edges[0].theTruth" == true)
           (RETURN p))
       {:query "FOR v, e, p IN 1..5 OUTBOUND 'circles/A' GRAPH 'traversalGraph'\nFILTER p.edges[0].theTruth == true\nRETURN p"
        :args {}})))