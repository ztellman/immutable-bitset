(ns immutable-bitset-simple-check
  (:require
    [clojure.test :refer :all]
    [immutable-bitset :as ib]
    [simple-check.core :as sc]
    [simple-check.generators :as gen]
    [simple-check.properties :as prop]
    [simple-check.clojure-test :as ct :refer (defspec)]))

;; Equivalent to clojure.core set property helpers
;;

(defn set-equiv
  "Putting `is` into provided set `s` is equivalent
  to putting them into a clojure.core set?"
  [s is]
  (= (into #{} is)
     (into s is)))

(defn set-equiv-property-creator
  "For all list of integers `is`, conjoining them into a clojure.core set
  and `specific-set` should be equivalent"
  [specific-set]
  (prop/for-all [is (gen/vector gen/int)]
                (set-equiv specific-set is)))

;; Conj then disj property helpers
;;

(defn conj-then-disj-all-elements-empty?
  "Conj all of `is` into s, then disj them all. That should
  then be empty"
  [s is]
  (let [conjs (reduce conj s is)
        disjs (reduce disj conjs is)]
    (empty? disjs)))

(defn fill-then-empty-property-creator
  [s]
  ;; for all list of integers `is`, conjoining themm all into
  ;; `s` and then disjoining them all should be empty
  (prop/for-all [is (gen/vector gen/int)]
                (conj-then-disj-all-elements-empty? s is)))


;; Sparse
;;

(defspec prop-sparse-equiv-into-set 1000
  (set-equiv-property-creator (ib/sparse-bitset)))

(defspec prop-sparse-fill-then-empty 1000
  (fill-then-empty-property-creator (ib/sparse-bitset)))

;; Dense
;;

(defspec prop-dense-equiv-into-set 1000
  (set-equiv-property-creator (ib/dense-bitset)))

(defspec prop-dense-fill-then-empty 1000
  (fill-then-empty-property-creator (ib/dense-bitset)))
