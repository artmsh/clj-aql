(ns clj-aql.ops-test
  (:require [clojure.test :refer :all]
            [clj-aql.core :refer :all]))

(deftest for-test
  (let [field-variable "u"
        coll-variable "users"]
    (is (= (FOR "item" :IN "collection")
           "FOR item IN collection"))
    (is (= (FOR field-variable :IN [1 2 3])
           "FOR u IN [1,2,3]"))
    (is (= (FOR "item" :IN coll-variable)
           "FOR item IN users"))
    (is (= (FOR "item" :IN (FOR "item2" :IN "items"))
           "FOR item IN (FOR item2 IN items)"))
    (is (= (FOR "item" :IN (FLATTEN [1 2 [3 4]]))
           "FOR item IN FLATTEN([1,2,[3,4]])"))))

(deftest return-test
  (is (= (RETURN "expression")
         "RETURN expression"))
  (is (= (RETURN {:name "u.name" :age "u.age"})
         "RETURN {name:u.name,age:u.age}"))
  (is (= (RETURN {"[u._id]" "u.age"})
         "RETURN {[u._id]:u.age}"))
  (is (= (RETURN (FLATTEN [1 [2 3]]))
         "RETURN FLATTEN([1,[2,3]])"))
  (is (= (RETURN (FOR "i" :IN [1 2]))
         "RETURN (FOR i IN [1,2])")))

(deftest filter-test
  (is (= (FILTER "b.id == 1")
         "FILTER b.id == 1"))
  (is (= (FILTER (OR (== "b.id" 1) (== "b.id" 2)))
         "FILTER b.id==1 OR b.id==2")))

(deftest sort-test
  (is (= (SORT "u.lastName" "u.id")
         "SORT u.lastName,u.id"))
  (is (= (SORT "u.id" :DESC)
         "SORT u.id DESC")))

(deftest limit-test
  (let [offset 2
        count 5]
    (is (= (LIMIT 5)
           "LIMIT 5"))
    (is (= (LIMIT 2 5)
           "LIMIT 2,5"))
    (is (= (LIMIT offset count)
           "LIMIT 2,5"))))

(deftest let-test
  (is (= (LET "body" "t.body")
         "LET body=t.body"))
  (is (= (LET "taxon" (DOCUMENT "taxon" "tx.taxon_id"))
         "LET taxon=DOCUMENT(taxon,\"tx.taxon_id\")"))
  (is (= (LET "taxa" (FOR "t" :IN "taxon"))
         "LET taxa=(FOR t IN taxon)")))