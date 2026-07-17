(ns culture.facts-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [culture.facts :as facts]))

(deftest grc-has-culture-basis
  (let [sb (facts/spec-basis "GRC")]
    (is (= 10 (count sb)))
    (is (= (count sb) (count (set (map :culture/id sb)))))
    (is (every? #(str/starts-with? (:culture/url %) "https://") sb))
    (is (every? #(= "GRC" (:culture/country %)) sb))
    (is (every? #(nil? (:culture/municipality %)) sb))
    (is (every? #(seq (:culture/summary %)) sb))
    (is (every? #(string? (:culture/retrieved-at %)) sb))))

(deftest unknown-jurisdiction-has-no-basis
  (is (nil? (facts/spec-basis "TUR")))
  (is (nil? (facts/spec-basis "zzz"))))

(deftest coverage-is-honest
  (let [c (facts/coverage ["GRC" "TUR"])]
    (is (= 2 (:requested c)))
    (is (= 1 (:covered c)))
    (is (= ["TUR"] (:missing-jurisdictions c)))))

(deftest by-kind-filters
  (is (= 5 (count (facts/by-kind "GRC" :dish))))
  (is (= ["grc.beverage.ouzo"]
         (mapv :culture/id (facts/by-kind "GRC" :beverage))))
  (is (empty? (facts/by-kind "GRC" :other)))
  (is (empty? (facts/by-kind "TUR" :dish))))

(deftest tx-file-matches-catalog
  (let [tx (edn/read-string (slurp "data/culture-tx.edn"))
        flat (mapcat val (sort-by key facts/catalog))]
    (is (= (vec flat) (vec tx)))))
