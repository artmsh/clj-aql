(ns clj-aql.fns-test
  (:require [clojure.test :refer :all]
            [clj-aql.new.core :refer :all]))

(deftest flatten-test
  (is (= (RETURN (FLATTEN [1 [2 3]]))
         "RETURN FLATTEN([1,[2,3]])")))

(deftest document-test
  (let [ids (list "john" "mary")
        id "john"
        users "users"]
    (is (= (RETURN (DOCUMENT "users" id))
           "RETURN DOCUMENT(users,\"john\")"))
    (is (= (RETURN (DOCUMENT users ids))
           "RETURN DOCUMENT(users,[\"john\",\"mary\"])"))))
