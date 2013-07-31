(ns immutable-bitset
  (:require
    [primitive-math :as p])
  (:import
    [java.util
     BitSet]))

(deftype Chunk
  [^int generation
   ^BitSet bitset])

(defn- ^Chunk bitset-chunk [^long generation log2-chunk-size]
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
  [^byte log2-chunk-size
   ^int generation
   ^int count
   m
   meta]

  java.lang.Object
  (hashCode [this]
    (->> this
      (map #(p/bit-xor (long %) (p/>>> (long %) 32)))
      (reduce #(p/+ (long %1) (long %2)))))
  (equals [this x] (.equiv this x))

  java.util.Set
  (size [this] count)
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
    (when-not (zero? count)
      (mapcat
        (fn [[slot ^Chunk v]]
          (bit-seq (.bitset v) (p/<< (long slot) log2-chunk-size)))
        m)))

  clojure.lang.IPersistentSet
  (equiv [this x]
    (and
      (set? x)
      (= count (clojure.core/count x))
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
            (assoc-bitset this
              :count (p/dec count)
              :m (assoc m slot
                   (Chunk. generation
                     (doto ^BitSet (.clone ^BitSet (.bitset chunk))
                       (.set idx false)))))
            this))
        this)))
  (cons [this n]
    (let [n (long n)
          slot (chunk-idx n log2-chunk-size)
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
          :m (let [^Chunk chunk (bitset-chunk generation log2-chunk-size)]
               (.set ^BitSet (.bitset chunk) idx true)
               (assoc m slot chunk)))))))

(deftype TransientBitSet
  [^byte log2-chunk-size
   ^int generation
   ^int count
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
          :m (let [^Chunk chunk (bitset-chunk generation log2-chunk-size)]
               (.set ^BitSet (.bitset chunk) idx true)
               (assoc! m slot chunk)))))))

(defn- ->persistent [^TransientBitSet bitset]
  (PersistentBitSet.
    (.log2-chunk-size bitset)
    (.generation bitset)
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
  "Creates an immutable set which can only store integral values.  This should be used unless elements are densely
   clustered (each element has multiple elements within +/- 1000)."
  ([]
     ;; 128 bits per chunk
     (PersistentBitSet. 7 0 0 {} nil))
  ([s]
     (into (sparse-bitset) s)))

(defn dense-bitset
  "Creates an immutable set which can only store integral values.  This should be used only if elements are densely
   clustered (each element has multiple elements within +/- 1000)."
  ([]
     ;; 4096 bits per chunk
     (PersistentBitSet. 12 0 0 {} nil))
  ([s]
     (into (dense-bitset) s)))

;;;

(defn- merge-bit-op [bit-set-fn keys-fn ^PersistentBitSet a ^PersistentBitSet b]
  (assert (= (.log2-chunk-size a) (.log2-chunk-size b)))
  (let [log2-chunk-size (.log2-chunk-size a)
        generation (p/inc (long (max (.generation a) (.generation b))))
        cnt (atom 0)
        m-a (.m a)
        m-b (.m b)
        ks (keys-fn m-a m-b)
        m (zipmap
            ks
            (map
              (fn [k]
                (let [^Chunk a (get m-a k)
                      ^Chunk b (get m-b k)]
                  (cond

                    (and a b)
                    (let [^Chunk chunk (Chunk. generation (.clone ^BitSet (.bitset a)))
                          ^BitSet b-a (.bitset chunk)
                          ^BitSet b-b (.bitset b)]
                      (bit-set-fn b-a b-b)
                      (swap! cnt + (.cardinality b-a))
                      chunk)

                    a
                    (let [^BitSet b-a (.bitset a)]
                      (swap! cnt + (.cardinality b-a))
                      a)

                    b
                    (let [^BitSet b-b (.bitset b)]
                      (swap! cnt + (.cardinality b-b))
                      b)

                    :else
                    (throw (IllegalStateException.)))))
              ks))]
    (PersistentBitSet.
      log2-chunk-size
      generation
      (long @cnt)
      m
      nil)))

(defn union
  "Returns the union of two bitsets."
  [a b]
  (merge-bit-op
    #(.or ^BitSet %1 %2)
    (fn [a b]
      (concat
        (keys a)
        (remove #(contains? a %) (keys b))))
    a
    b))

(defn intersection
  "Returns the intersection of two bitsets."
  [a b]
  (merge-bit-op
    #(.and ^BitSet %1 %2)
    (fn [a b]
      (filter #(contains? b %) (keys a)))
    a
    b))

(defn difference
  "Returns the difference between two bitsets."
  [a b]
  (merge-bit-op
    #(.andNot ^BitSet %1 %2)
    (fn [a b] (keys a))
    a
    b))
