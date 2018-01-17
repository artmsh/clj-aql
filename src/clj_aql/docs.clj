(ns clj-aql.docs
  (:require [clj-aql.spec.fn]
            [clj-aql.spec.op]
            [clojure.string :as str]
            [clojure.spec :as s]))

(declare spec->doc)

(defn pair->doc [[kw spec-form]]
  (case kw
    :kw (str/join " | " spec-form)
    :ref-string "string-literal"
    :ref-symbol "string-variable"
    :ref-symbol-sequential "sequential-variable"
    :str-symbol (second (first spec-form))
    :literal-array "collection-literal"
    (spec->doc spec-form)))

(defn cat->doc [pairs]
  (str "(" (str/join " " (map pair->doc pairs)) ")"))

(defn or->doc [pairs]
  (str "[ " (str/join " | " (map pair->doc pairs)) " ]"))

(defn star->doc [form]
  (str (spec->doc form) "*"))

(defn alt->doc [pairs]
  (str (str/join " | " (map pair->doc pairs))))

(defn spec->doc [form]
  (cond
    (keyword? form) (str "[" (name form) "](#" (name form) ")")
    (= (first form) `clojure.spec/alt) (alt->doc (partition 2 (rest form)))
    (= (first form) `clojure.spec/cat) (cat->doc (partition 2 (rest form)))
    (= (first form) `clojure.spec/or) (or->doc (partition 2 (rest form)))
    (= (first form) `clojure.spec/*) (star->doc (second form))))

(defn emit-spec-doc [spec-kw]
  (str "### " (name spec-kw) "\n"
       (spec->doc (s/form spec-kw))))

(defn get-ns [kw]
  (second (re-find #":(.+)[/](.+)" (str kw))))

(defn included-ns? [ns]
  (contains? #{"clj-aql.spec.fn" "clj-aql.spec.op"} ns))

(defn get-registered-specs []
  (map first (filter #(included-ns? (get-ns (key %))) (s/registry))))

(defn -main [& args]
  (spit "doc/syntax.md"
        (str
          "## Syntax\n"
          (str/join "\n" (map emit-spec-doc (get-registered-specs))))))