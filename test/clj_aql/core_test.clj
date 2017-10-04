(ns clj-aql.core-test
  (:require [clojure.test :refer :all]
            [clj-aql.core :refer :all]))

(deftest return-test
  (testing "simple return expression"
    (is (= (:query (RETURN expression)) "RETURN expression")))
  (testing "return with function"
    (is (= (:query (RETURN (ATTRIBUTES { :foo "bar" :_key "123" :_custom "yes" } false true)))
           "RETURN ATTRIBUTES({\"foo\":\"bar\",\"_key\":\"123\",\"_custom\":\"yes\"},false,true)"))))

(deftest for-in-return-test
  (testing "for-in-return expression"
    (is (= (:query
             (FOR [variableName] :IN "expression"
              (RETURN variableName)))
           "FOR variableName IN expression\nRETURN variableName"))
    (is (= (:query
             (FOR [u] :IN "users"
              (RETURN u.name)))
           "FOR u IN users\nRETURN u.name"))))

(defn query [vall]
  (FOR [u] :IN "users"
    (FILTER u.name == ~vall)
    (RETURN u.name)))

(deftest filter-test
  (testing "filter expression"
    (is (=  (:query
              (FOR [u] :IN "users"
               (FILTER u.active == true && u.age < 39)
               (RETURN u)))
          "FOR u IN users\nFILTER u.active == true && u.age < 39\nRETURN u"))
      (is (= (query "someVal")
             {:query "FOR u IN users\nFILTER u.name == @vall\nRETURN u.name"
              :args {"vall" "someVal"}}))))

(deftest sort-test
  (testing "sort expression"
    (is (= (:query
             (FOR [u] :IN "users"
               (SORT [u.lastName u.firstName u.id] :DESC)
               (RETURN u)))
           "FOR u IN users\nSORT u.lastName,u.firstName,u.id DESC\nRETURN u"))))

(deftest limit-test
  (testing "limit expression"
    (is (= (:query
             (FOR [u] :IN "users"
              (LIMIT 2 5)
              (RETURN u)))
           "FOR u IN users\nLIMIT 2,5\nRETURN u"))))

; FOR u IN users
; LET friends = (
; FOR f IN friends
; FILTER u.id == f.userId
; RETURN f
; )
; LET memberships = (
; FOR m IN memberships
; FILTER u.id == m.userId
; RETURN m
; )
; RETURN {"user":u,"friends":friends,"numFriends":LENGTH(friends),"memberShips":memberships}

(deftest let-test
  (testing "let expression"
    (is (=
          (:query (FOR [u] :IN "users"
             (LET [friends (FOR [f] :IN "friends"
                             (FILTER u.id == f.userId)
                             (RETURN f))
                   memberships (FOR [m] :IN "memberships"
                                 (FILTER u.id == m.userId)
                                 (RETURN m))])
             (RETURN {:user u :friends friends :numFriends (LENGTH friends) :memberShips memberships})))
          (:query (FOR [u] :IN "users"
             (LET [friends (FOR [f] :IN "friends"
                            (FILTER u.id == f.userId)
                            (RETURN f))])
             (LET [memberships (FOR [m] :IN "memberships"
                            (FILTER u.id == m.userId)
                            (RETURN m))])
             (RETURN {:user u :friends friends :numFriends (LENGTH friends) :memberShips memberships})))
          "FOR u IN users\nLET friends = (\nFOR f IN friends\nFILTER u.id == f.userId\nRETURN f\n)\nLET memberships = (\nFOR m IN memberships\nFILTER u.id == m.userId\nRETURN m\n)\nRETURN {\"user\":u,\"friends\":friends,\"numFriends\":LENGTH(friends),\"memberShips\":memberships}"))))

(deftest collect-test
  (testing "collect expression"
    (is (= (:query (FOR [u] :IN "users"
                     (COLLECT [city u.city])
                     (RETURN { :city city })))
           "FOR u IN users\nCOLLECT city = u.city\nRETURN {\"city\":city}"))))