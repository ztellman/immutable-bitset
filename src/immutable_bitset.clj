(ns immutable-bitset
  (:require
    [primitive-math :as p])
  (:import
    [java.util
     BitSet]))

(deftype Chunk
  [^long generation
   ^BitSet bitset])

(defn- ^Chunk bit-set-chunk [^long generation log2-chunk-size]
  (Chunk. generation (BitSet. (p/<< 1 log2-chunk-size))))

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

(defmacro ^:private assoc-bitset [x & {:as fields}]
  (let [type (-> &env ^clojure.lang.Compiler$LocalBinding (get x) .getJavaClass)
        field-names [:log2-chunk-size :generation :count :m :meta]]
    `(new ~type
       ~@(map
           (fn [field-name]
             (get fields field-name
               `(~(symbol (str "." (name field-name))) ~x)))
           field-names))))

(definline ^:private chunk-idx [n bit-shift]
  `(p/>> ~n ~bit-shift))

(definline ^:private idx-within-chunk [n bit-shift]
  `(p/bit-and ~n (-> 1 (p/<< ~bit-shift) p/dec)))

(deftype PersistentBitSet
  [^long log2-chunk-size
   ^long generation
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
  (withMeta [this meta] (assoc-bitset this :meta meta))

  clojure.lang.IEditableCollection
  (asTransient [this] (->transient this))

  clojure.lang.Seqable
  (seq [_]
    (mapcat
      (fn [[slot ^Chunk v]]
        (bit-seq (.bitset v) (p/<< (long slot) log2-chunk-size)))
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
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (.get ^BitSet (.bitset chunk) idx))
        false)))
  (disjoin [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (if (.get ^BitSet (.bitset chunk) idx)
            (let [generation (p/inc generation)]
              (assoc-bitset this
                :count (p/dec count)
                :m (assoc m slot
                     (Chunk. generation
                       (doto ^BitSet (.clone ^BitSet (.bitset chunk))
                         (.set idx false))))))
            this))
        this)))
  (cons [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)
          generation (p/inc generation)
          idx (idx-within-chunk n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (if-not (.get ^BitSet (.bitset chunk) idx)
          (assoc-bitset this
            :count (p/inc count)
            :m (assoc m slot
                 (Chunk. generation
                   (doto ^BitSet (.clone ^BitSet (.bitset chunk))
                     (.set idx true)))))
          this)
        (assoc-bitset this
          :count (p/inc count)
          :m (let [^Chunk chunk (bit-set-chunk generation log2-chunk-size)]
               (.set ^BitSet (.bitset chunk) idx true)
               (assoc m slot chunk)))))))

(deftype TransientBitSet
  [^long log2-chunk-size
   ^long generation
   ^long count
   m
   meta]

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [this meta] (assoc-bitset this :meta meta))

  clojure.lang.ITransientSet
  (count [_] count)
  (persistent [this] (->persistent this))
  (contains [_ n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (.get ^BitSet (.bitset chunk) idx))
        false)))
  (disjoin [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (let [idx (idx-within-chunk n log2-chunk-size)]
          (if (.get ^BitSet (.bitset chunk) idx)
            (if (p/== (.generation chunk) generation)
              (do
                (.set ^BitSet (.bitset chunk) idx false)
                (assoc-bitset this :count (p/dec count)))
              (assoc-bitset this
                :count (p/dec count)
                :m (let [^BitSet bitset (.clone ^BitSet (.bitset chunk))]
                     (.set bitset idx false)
                     (assoc! m slot (Chunk. generation bitset)))))
            this))
        this)))
  (conj [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)
          idx (idx-within-chunk n log2-chunk-size)]
      (if-let [^Chunk chunk (get m slot)]
        (if-not (.get ^BitSet (.bitset chunk) idx)
          (if (p/== (.generation chunk) generation)
            (do
              (.set ^BitSet (.bitset chunk) idx true)
              (assoc-bitset this :count (p/inc count)))
            (assoc-bitset this
              :count (p/inc count)
              :m (let [^BitSet bitset (.clone ^BitSet (.bitset chunk))]
                   (.set bitset idx true)
                   (assoc! m slot bitset))))
          this)
        (assoc-bitset this
          :count (p/inc count)
          :m (let [^Chunk chunk (bit-set-chunk generation log2-chunk-size)]
               (.set ^BitSet (.bitset chunk) idx true)
               (assoc! m slot chunk)))))))

(defn- ->persistent [^TransientBitSet bitset]
  (PersistentBitSet.
    (.log2-chunk-size bitset)
    (p/inc (.generation bitset))
    (.count bitset)
    (persistent! (.m bitset))
    (.meta bitset)))

(defn- ->transient [^PersistentBitSet bitset]
  (TransientBitSet.
    (.log2-chunk-size bitset)
    (p/inc (.generation bitset))
    (.count bitset)
    (transient (.m bitset))
    (.meta bitset)))

;;;

(defn sparse-bitset
  "Creates an immutable set which can only store integral values."
  ([]
     ;; 128 bits per chunk
     (PersistentBitSet. 7 0 0 {} nil))
  ([s]
     (into (sparse-bitset) s)))

(defn dense-bitset
  "Creates an immutable set which can only store integral values."
  ([]
     ;; 4096 bits per chunk
     (PersistentBitSet. 12 0 0 {} nil))
  ([s]
     (into (dense-bitset) s)))
