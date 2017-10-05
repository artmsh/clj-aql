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
           "FOR u IN users\nRETURN u.name"))
    (is (= (:query
             (FOR [u] :IN (FOR [v] :IN "vendors" (RETURN v))
               (RETURN u.name)))
           "FOR u IN (FOR v IN vendors\nRETURN v)\nRETURN u.name"))))

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


(defn limited-query [o c]
  (FOR [u] :IN "users"
       (LIMIT ~o ~c)
       (RETURN u)))

(defn limited-query-no-offset [c]
  (FOR [u] :IN "users"
       (LIMIT ~c)
       (RETURN u)))

(deftest limit-test
  (testing "limit expression"
    (is (= (:query
             (FOR [u] :IN "users"
              (LIMIT 2 5)
              (RETURN u)))
           "FOR u IN users\nLIMIT 2,5\nRETURN u")))
  (testing "limit expression with variables"
    (is (= (limited-query 10 100)
           {:query "FOR u IN users\nLIMIT @o,@c\nRETURN u"
            :args {"o" 10 "c" 100}
            })))
  (testing "limit expression with count only (no offset)"
    (is (= (limited-query-no-offset 10)
           {:query "FOR u IN users\nLIMIT @c\nRETURN u"
            :args {"c" 10}
            })))
  )


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

(def taxon-type-id "weight")

(deftest let-test
  (testing "let expression"
    #_(is (=
          (FOR [t] :IN (FOR [b] :IN "body"
                         (LET [taxa (FOR [tx] :IN "b.taxa"
                                      (LET [taxon (DOCUMENT "taxon" tx.taxon_id)])
                                      (FILTER taxon.taxon_type_id == ~taxon-type-id)
                                      (RETURN taxon))])
                         (RETURN {:key "taxa[0].id" :body b.id}))
            (FILTER t.key != null)
            (LET [body "t.body"])
            (COLLECT [vals t.key] :INTO objs :KEEP body)
            (RETURN "objs[*].body"))
          {:query ""
           :args {"taxon-type-id" "weight"}}))
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
                     (COLLECT [city u.city] :INTO objs :KEEP body)
                     (RETURN { :city city })))
           "FOR u IN users\nCOLLECT city = u.city INTO objs KEEP body\nRETURN {\"city\":city}"))))
