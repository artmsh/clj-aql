(ns clj-aql.fns-test
  (:require [clojure.test :refer :all]
            [clj-aql.core :refer :all]))

(deftest flatten-test
  (is (= (FLATTEN [1 [2 3]])
         "FLATTEN([1,[2,3]])")))

(deftest document-test
  (let [ids (list "john" "mary")
        users "users"]
    (is (= (DOCUMENT "users" "john")
           "DOCUMENT(users,\"john\")"))
    (is (= (DOCUMENT users ids)
           "DOCUMENT(users,[\"john\",\"mary\"])"))))
