(defproject clj-aql "0.1.1-SNAPSHOT"
  :description "Clojure DSL for ArangoDB query language"
  :url "https://github.com/artmsh/clj-aql"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha19"]
                 [cheshire "5.8.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [circleci/circleci.test "0.3.1"]]}}
  :aliases {"test" ["run" "-m" "circleci.test/dir" :project/test-paths]
            "tests" ["run" "-m" "circleci.test"]
            "retest" ["run" "-m" "circleci.test.retest"]})
