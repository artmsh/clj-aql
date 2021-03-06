(ns clj-aql.core-test
  (:require [clojure.test :refer :all]
            [clj-aql.core :refer :all]
            [cheshire.core :as json]
            [spectrum.check :as check]))

(def db "body")
(def id "000000000000")

(deftest spectrum-check
  (is (empty? (check/check 'clj-aql.core))))

(deftest return-test
  (testing "simple return expression"
    (is (= (:query (RETURN expression)) "RETURN expression")))
  (testing "return with function"
    (is (= (:query (RETURN (ATTRIBUTES {:foo "bar" :_key "123" :_custom "yes"} false true)))
           "RETURN ATTRIBUTES({\"foo\":\"bar\",\"_key\":\"123\",\"_custom\":\"yes\"},false,true)"))
    (is (= (RETURN (DOCUMENT ~db ~id))
           {:query "RETURN DOCUMENT(@db,@id)"
            :args {"db" "body"
                   "id" "000000000000"}}))))

(def expression "expression")

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
           "FOR u IN (FOR v IN vendors\nRETURN v)\nRETURN u.name"))
    (is (= (FOR [variableName] :IN expression
             (RETURN variableName))
           {:query "FOR variableName IN expression\nRETURN variableName"
            :args {}}))))

(defn query [vall]
  (FOR [u] :IN "users"
    (FILTER u.name == ~vall)
    (RETURN u.name)))

(def local-db "local")
(def external-db "external")
(def object-id "object-id")

(def ids #{"1" "2"})
(def clause (clojure.string/join " OR " (map #(str "id = " %) ids)))

(FOR [b] :IN "body"
  (FILTER clause) ;; (POSITION ~ids "b._key" "false") = true)
  (RETURN {:body b
           :taxa (FOR [tx] :IN "b.taxa" (RETURN (DOCUMENT "taxon" tx.taxon_id)))}))

(deftest filter-test
  (testing "filter expression"
    (is (= (:query
             (FOR [u] :IN "users"
               (FILTER u.active == true)
               (RETURN u)))
           "FOR u IN users\nFILTER u.active == true\nRETURN u"))

    (is (= (:query
             (FOR [u] :IN "users"
               (FILTER u.active == true && u.age < 39)
               (RETURN u)))
           "FOR u IN users\nFILTER u.active == true && u.age < 39\nRETURN u"))
    (is (= (query "someVal")
           {:query "FOR u IN users\nFILTER u.name == @vall\nRETURN u.name"
            :args  {"vall" "someVal"}}))
    (is (= (FOR [m] :IN "mapping"
             (FILTER m.m_db == ~local-db &&
                     m.e_db == ~external-db &&
                     m.m_id == ~object-id)
             (RETURN m))
           {:query "FOR m IN mapping\nFILTER m.m_db == @local-db && m.e_db == @external-db  && m.m_id == @object-id\nRETURN m"
            :args  {"local-db" "local", "external-db" "external", "object-id" "object-id"}}))
    (is (= (FOR [u] :IN "users"
             (FILTER clause)
             (RETURN u))
           {:query "FOR u IN users\nFILTER id = 1 OR id = 2\nRETURN u"
            :args  {}}))))

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
            :args  {"o" 10 "c" 100}
            })))
  (testing "limit expression with count only (no offset)"
    (is (= (limited-query-no-offset 10)
           {:query "FOR u IN users\nLIMIT @c\nRETURN u"
            :args  {"c" 10}
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
             :args  {"taxon-type-id" "weight"}}))
    (is (= (FOR [u] :IN "users"
             (LET [body "u.body"])
             (RETURN body))
           {:query "FOR u IN users\nLET body = u.body\nRETURN body"
            :args  {}}))

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
                     (RETURN {:city city})))
           "FOR u IN users\nCOLLECT city = u.city INTO objs KEEP body\nRETURN {\"city\":city}"))))

(deftest keep-test
  (is (= (:query (RETURN (KEEP (DOCUMENT "location" "t1p") ["id" "locationType"])))
         "RETURN KEEP(DOCUMENT(\"location\",\"t1p\"),[\"id\",\"locationType\"])")))

(deftest values-test
  (is (= (:query (RETURN (VALUES (KEEP (DOCUMENT "location" "t1p") ["id" "locationType"]))))
         "RETURN VALUES(KEEP(DOCUMENT(\"location\",\"t1p\"),[\"id\",\"locationType\"]))")))

(deftest flatten-test
  (testing "flatten expression"
    (is (= (:query (FOR [u] :IN "users"
                     (LET [name (FLATTEN u.names)])
                     (RETURN {:name name})))
           "FOR u IN users\nLET name = FLATTEN(u.names)\nRETURN {\"name\":name}")))
  (testing "flatten expression with inner FOR"
    (is (= (:query (FOR [u] :IN (FLATTEN (FOR [x] :IN "users"
                                           (RETURN x)))
                     (RETURN u)))
           "FOR u IN FLATTEN((FOR x IN users\nRETURN x))\nRETURN u"))))

(deftest fn-test
  (testing "POSITION with external array expression"
    (let [ids #{"00000518200053"}]
      (is (= (FOR [b] :IN "body"
                       (FILTER (POSITION ~ids b._key false) == true)
                       (RETURN b)))
          {:query "FOR b IN body\nFILTER POSITION([\"00000518200053\"],b._key,false) == true\nRETURN b"
           :args [["00000518200053"]]}))))

(deftest insert-test
  (testing "insert string literal"
    (is (= (INSERT "{ value : 1 }" :IN "numbers")
           {:query "INSERT { value : 1 } IN numbers"
            :args {}})))
  (testing "insert string literal with options"
    (is (= (INSERT "{ value : 1 }" :IN "numbers" :OPTIONS {:waitForSync true})
           {:query "INSERT { value : 1 } IN numbers OPTIONS {\"waitForSync\":true}"
            :args {}})))
  (testing "insert into variable collection"
    (let [coll "collection"]
      (is (= (INSERT "{ value : 1 }" :IN coll)
             {:query "INSERT { value : 1 } IN collection"
              :args {}}))))
  (testing "insert with return new"
    (is (= (INSERT "{ value : 1 }" :IN "coll" :RETURN "NEW")
           {:query "INSERT { value : 1 } IN coll RETURN NEW"
            :args {}})))
  (testing "insert multiple from string binding"
    (let [edn-objs [{:id 1 :data1 100 :data2 "100" :data3 {:data4 [] :data5 "100-200"}}
                    {:id 2 :data1 200 :data2 "200" :data3 {:data4 [] :data5 "200-300"}}]
          json-objs (json/generate-string edn-objs {:key-fn name})]
      (is (= (FOR [v] :IN json-objs
               (INSERT "v" :IN "coll" :RETURN "NEW"))
             {:query (str "FOR v IN [{\"id\":1,\"data1\":100,\"data2\":\"100\",\"data3\":{\"data4\":[],"
                          "\"data5\":\"100-200\"}},{\"id\":2,\"data1\":200,\"data2\":\"200\",\"data3\":{\"data4\":[],"
                          "\"data5\":\"200-300\"}}]\nINSERT v IN coll RETURN NEW")
              :args {}})))))
