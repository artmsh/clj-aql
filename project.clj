(defproject clj-aql "0.2.0-SNAPSHOT"
  :description "Clojure DSL for ArangoDB query language"
  :url "https://github.com/artmsh/clj-aql"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[cheshire "5.8.0"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/test.check "0.9.0"]
                                  [circleci/circleci.test "0.3.1"]
                                  [spectrum "0.1.4"]
                                  [org.clojure/core.match "0.3.0-alpha5"]]
                   :plugins [[lein-nvd "0.4.2"]
                             [lein-kibit "0.1.5"]]}}
  :source-paths ["src"]
  :aliases {"test" ["run" "-m" "circleci.test/dir" :project/test-paths]
            "tests" ["run" "-m" "circleci.test"]
            "retest" ["run" "-m" "circleci.test.retest"]
            "docs" ["run" "-m" "clj-aql.docs"]})
