(ns immutable-bitset-test
  (:use
    [clojure test]
    [immutable-bitset])
  (:require
    [criterium.core :as c])
  (:import
    [java.util BitSet]))

(deftest test-bitset-operations
  (let [s (sparse-bitset [1 10 100 1000])]
    (is (= #{1 10 100 1000} s))
    (is (= #{1 10 1000}
          (disj s 100)
          (-> s (disj 100) (conj 100) (disj 100))
          ))
    (is (= #{1 10 100 1000}
          (conj s 1 100)
          (-> s (disj 20) (conj 1 100))
          (-> s transient (disj! 20) (conj! 100) persistent!)
          ))
    (is (= #{1 10}
          (-> s transient (disj! 100 1000) persistent!)
          (-> s transient (disj! 10 100 100) (conj! 10) persistent!)
          ))))

(deftest ^:benchmark benchmark-modify-set
  (println "sparse bitset into 1e3")
  (c/quick-bench
    (into (sparse-bitset) (range 1e3)))
  (println "dense bitset into 1e3")
    (c/quick-bench
    (into (dense-bitset) (range 1e3)))
  (println "normal set into 1e3")
  (c/quick-bench
    (into #{} (range 1e3)))
  (println "mutable bitset add 1e3")
  (c/quick-bench
    (let [^BitSet bitset (BitSet. 1e3)]
      (dotimes [idx 1e3]
        (.set bitset idx true)))))

(deftest ^:benchmark benchmark-check-set
  (println "check sparse bitset")
  (let [b (into (sparse-bitset) (range 1e3))]
    (c/quick-bench
      (contains? b 123)))
  (println "check dense bitset")
  (let [b (into (dense-bitset) (range 1e3))]
    (c/quick-bench
      (contains? b 123)))
  (println "check normal set")
  (let [s (into #{} (range 1e3))]
    (c/quick-bench
      (contains? s 123)))
  (println "mutable bitset add 1e3")
  (let [b (BitSet. 1e3)]
    (c/quick-bench
      (.get b 123))))
