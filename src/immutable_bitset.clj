(ns immutable-bitset
  (:require
    [primitive-math :as p])
  (:import
    [java.util
     BitSet]))

(deftype Chunk
  [^long generation
   ^BitSet bitset])

(def ^:const chunk-size 1024)
(def ^:const bit-shift (/ (Math/log chunk-size) (Math/log 2)))
(def ^:const bit-mask (dec chunk-size))

(defn- ^Chunk bit-set-chunk [^long generation]
  (Chunk. generation (BitSet. chunk-size)))

(defn- bit-seq [^java.util.BitSet bitset ^long offset]
  (let [cnt (long (.cardinality bitset))
        ^longs ary (long-array cnt)]
    (loop [ary-idx 0, set-idx 0]
      (when (p/< ary-idx cnt)
        (let [set-idx (.nextSetBit bitset set-idx)]
          (aset ary ary-idx (p/+ offset set-idx))
          (recur (p/inc ary-idx) (p/inc set-idx)))))
    (seq ary)))

(declare bitset ->persistent ->transient)

(deftype PersistentSparseBitSet
  [^long generation
   ^long count
   m
   meta]

  java.lang.Object
  (hashCode [this] (reduce + this))
  (equals [this x] (.equiv this x))

  java.util.Set
  (size [_] count)
  (isEmpty [_] (zero? count))
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
  (containsAll [this s] (every? #(contains? this %) s))

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [_ meta] (PersistentSparseBitSet. generation count m meta))

  clojure.lang.IEditableCollection
  (asTransient [this] (->transient this))

  clojure.lang.Seqable
  (seq [_]
    (mapcat
      (fn [[slot ^Chunk v]]
        (bit-seq (.bitset v) (p/<< (long slot) bit-shift)))
      m))

  clojure.lang.IPersistentSet
  (equiv [this x]
    (and (set? x)
      (every?
        #(contains? x %)
        (seq this))))
  (count [_] count)
  (empty [_] (bitset))
  (contains [_ n]
    (let [n (long n)
          slot (p/>> n bit-shift)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (p/bit-and n bit-mask)]
          (.get ^BitSet (.bitset chunk) idx))
        false)))
  (disjoin [this n]
    (let [n (long n)
          slot (p/>> n bit-shift)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (p/bit-and n bit-mask)]
          (if (.get ^BitSet (.bitset chunk) idx)
            (let [generation (p/inc generation)]
              (PersistentSparseBitSet.
                generation
                (p/dec count)
                (assoc m slot
                  (Chunk. generation
                    (doto ^BitSet (.clone ^BitSet (.bitset chunk))
                      (.set idx false))))
                meta))
            this))
        this)))
  (cons [this n]
    (let [n (long n)
          slot (p/>> n bit-shift)
          generation (p/inc generation)
          idx (p/bit-and n bit-mask)]
      (if-let [^Chunk chunk (get m slot)]
        (if-not (.get ^BitSet (.bitset chunk) idx)
          (PersistentSparseBitSet.
            generation
            (p/inc count)
            (assoc m slot
              (Chunk. generation
                (doto ^BitSet (.clone ^BitSet (.bitset chunk))
                  (.set idx true))))
            meta)
          this)
        (PersistentSparseBitSet.
          generation
          (p/inc count)
          (let [^Chunk chunk (bit-set-chunk generation)]
            (.set ^BitSet (.bitset chunk) idx true)
            (assoc m slot chunk))
          meta)))))

(deftype TransientSparseBitSet
  [^long generation
   ^long count
   m
   meta]

  java.util.Set
  (size [_] count)
  (isEmpty [_] (zero? count))
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
  (containsAll [this s] (every? #(contains? this %) s))

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [_ meta] (TransientSparseBitSet. generation count m meta))

  clojure.lang.Seqable
  (seq [_]
    (mapcat
      (fn [[slot ^Chunk v]]
        (bit-seq (.bitset v) (p/<< (long slot) bit-shift)))
      m))

  clojure.lang.ITransientSet
  (count [_] count)
  (persistent [this] (->persistent this))
  (contains [_ n]
    (let [n (long n)
          slot (p/>> n bit-shift)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (p/bit-and n bit-mask)]
          (.get ^BitSet (.bitset chunk) idx))
        false)))
  (disjoin [this n]
    (let [n (long n)
          slot (p/>> n bit-shift)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (p/bit-and n bit-mask)]
          (if (.get ^BitSet (.bitset chunk) idx)
            (if (p/== (.generation chunk) generation)
              (do
                (.set ^BitSet (.bitset chunk) idx false)
                (TransientSparseBitSet.
                  generation
                  (p/dec count)
                  m
                  meta))
              (TransientSparseBitSet.
                generation
                (p/dec count)
                (let [^BitSet bitset (.clone ^BitSet (.bitset chunk))]
                  (.set bitset idx false)
                  (assoc m slot (Chunk. generation bitset)))
                meta))
            this))
        this)))
  (conj [this n]
    (let [n (long n)
          slot (p/>> n bit-shift)
          idx (p/bit-and n bit-mask)]
      (if-let [^Chunk chunk (get m slot)]
        (if-not (.get ^BitSet (.bitset chunk) idx)
          (if (p/== (.generation chunk) generation)
            (do
              (.set ^BitSet (.bitset chunk) idx true)
              (TransientSparseBitSet.
                generation
                (p/inc count)
                m
                meta))
            (TransientSparseBitSet.
              generation
              (p/inc count)
              (let [^BitSet bitset (.clone ^BitSet (.bitset chunk))]
                (.set bitset idx true)
                (assoc m slot bitset))
              meta))
          this)
        (TransientSparseBitSet.
          generation
          (p/inc count)
          (let [^Chunk chunk (bit-set-chunk generation)]
            (.set ^BitSet (.bitset chunk) idx true)
            (assoc m slot chunk))
          meta)))))

(defn- ->persistent [^TransientSparseBitSet bitset]
  (PersistentSparseBitSet.
    (p/inc (.generation bitset))
    (.count bitset)
    (.m bitset)
    (.meta bitset)))

(defn- ->transient [^PersistentSparseBitSet bitset]
  (TransientSparseBitSet.
    (p/inc (.generation bitset))
    (.count bitset)
    (.m bitset)
    (.meta bitset)))

(defn bitset
  "Creates a space-efficient integer set."
  ([]
     (PersistentSparseBitSet. 0 0 (sorted-map) nil))
  ([s]
     (into (bitset) s)))
