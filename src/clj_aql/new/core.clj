(ns clj-aql.new.core
  (:require [clj-aql.spec.op :as ops]
            [clojure.spec.alpha :as s]
            [clj-aql.core :as old-core]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [cheshire.core :as json]
            [clojure.core.match :as m]
            [cheshire.factory :as factory]
            [cheshire.generate :as gen])
  (:import (com.fasterxml.jackson.core JsonGenerator)))

(defn reducer [{:keys [s a]} [ss aa]]
  {:s (conj s ss)
   :a (concat a aa)})

(defn conform-wrapped [spec form]
  (let [v (s/conform spec form)]
    (if (= v :clojure.spec.alpha/invalid)
      (do
        (pprint/pprint form)
        (s/explain spec form))
      v)))

(declare emit)

(defn s-no-a [s] [s []])
(defn fmt-a [a] ["%s" [a]])
(defn many-sa-sep [coll sep] (let [r (reduce reducer {:s [] :a []} (map emit coll))]
                               [(str/join sep (:s r)) (:a r)]))
(defn many-sa [coll] (many-sa-sep coll " "))

(defn get-kw-ns [kw]
  (-> kw
      (str)
      (str/split #"/")
      (first)))

(defn aql-kw? [kw]
  (= ":in" (get-kw-ns kw)))

(defn skip-vec? [v]
  (and
    (vector? v)
    (= 2 (count v))
    (not (aql-kw? (first v)))))

; return ["FOR %s %s..." ["u" "IN"]]
(defn emit [form]
  (m/match [form]
           [[(_ :guard #(not (aql-kw? %))) (m :guard map?)]] (do
                                                               #_(prn "map")
                                                               (many-sa m))
           [[:in/s s]] (s-no-a s)
           [[:in/quoted-s s]] (s-no-a (str "'" s "'"))
           [[:in/s-var s]] (fmt-a s)
           [[:in/quoted-s-var s]] [(str "'%s'") [s]]
           [[:in/vec-var v]] (fmt-a (list json/generate-string v))
           [[:in/vec v]] (s-no-a (json/generate-string v))
           [[:in/map m]] (let [r (reduce reducer {:s [] :a []} (map emit (vals m)))]
                           [(str "{" (str/join "," (map #(str (name (key %)) ":" (val %)) (zipmap (keys m) (:s r)))) "}") (:a r)])
           [[:in/num n]] (s-no-a (str n))
           [[:in/tuple t]] (s-no-a (str/join "," t))
           [[:in/tuple-vars t]] [(str/join "," (repeat (count t) "%s")) t]
           [[:in/assignment-expression expr]] (many-sa-sep expr "=")
           [[:in/n-ary-expression e]] (many-sa-sep (cons (:lvalue e) (:rvalue e)) (str " " (:operator e) " "))
           [[:in/binary-expression e]] (many-sa-sep [(:lvalue e) (:rvalue e)] (str " " (:operator e) " "))
           [[:in/bracket-op op]] (let [[s a] (many-sa (vals op))]
                                       ;_ (prn "br" s a)

                                   [(str "(" s ")") a])
           [[:in/fn [_ fn]]] (let [[name & args] (vals fn)
                                   [s a] (many-sa-sep args ",")]
                               [(str (first (emit name)) "(" s ")") a])
           [(_ :guard symbol?)] (s-no-a (str form))
           [[_ (kw :guard keyword?)]] (s-no-a (name kw))
           [(_ :guard keyword?)] (s-no-a (name form))
           [(v :guard skip-vec?)] (emit (second v))
           [(v :guard vector?)] (many-sa v)
           :else (throw (ex-info "Cannot recognize form" {:form form}))))

(defn parse [form]
  (let [conf-form (conform-wrapped ::ops/high-level-op form)
        _ (prn form)
        _ (prn)
        _ (pprint/pprint conf-form)
        _ (prn "---------")]

    (emit conf-form)))

(defn build-macro-form [form]
  (let [[s a] (parse form)]
    (if (empty? a)
      s
      (concat (list 'format s) a))))

(defmacro FOR [& args] (build-macro-form (cons 'FOR args)))
(defmacro RETURN [& args] (build-macro-form (cons 'RETURN args)))
(defmacro FILTER [& args] (build-macro-form (cons 'FILTER args)))
(defmacro SORT [& args] (build-macro-form (cons 'SORT args)))
(defmacro LIMIT [& args] (build-macro-form (cons 'LIMIT args)))
(defmacro LET [& args] (build-macro-form (cons 'LET args)))