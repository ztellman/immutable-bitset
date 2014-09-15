# Project Moved to Contrib!

ztellman/immutable-bitset is now [clojure/data.int-map](https://github.com/clojure/data.int-map)

## Immutable Integer Sets

Clojure's immutable sets are great, but they can be hugely inefficient when we're only trying to store integers.  This library contains an implementation for immutable sets which are both faster and smaller for this special case.

### usage

```clj
[immutable-bitset "0.1.6"]
```

All functions are in the `immutable-bitset` namespace.  There are two constructors, `sparse-bitset` and `dense-bitset`, and three operators, `union`, `intersection`, and `difference`.

```clj
immutable-bitset> (sparse-bitset)
#{}
immutable-bitset> (conj *1 1 2 3)
#{1 2 3}
immutable-bitset> (disj *1 2)
#{1 3}
immutable-bitset> (sparse-bitset [1 2 3])
#{1 2 3}
immutable-bitset> (transient *1)
#<TransientBitSet immutable_bitset.TransientBitSet@bee743d>
immutable-bitset> (disj! *1 2)
#<TransientBitSet immutable_bitset.TransientBitSet@2aa42d42>
immutable-bitset> (conj! *1 5)
#<TransientBitSet immutable_bitset.TransientBitSet@7c079ab0>
immutable-bitset> (persistent! *1)
#{1 3 5}
immutable-bitset> (union *1 (sparse-bitset [5 7 9]))
#{1 3 5 7 9}
```

The set algebra operators take two bitsets, and return a bitset.  Use `clojure.set` for general set operations.

`dense-bitset` behaves the same as `sparse-bitset`, the difference is only in their memory efficiency.  Consider a case where we create a set of all numbers between one and one million:

```clj
(def s (range 1e6))

(into #{} s)              ; ~100mb
(into (sparse-bitset) s)  ; ~1mb
(into (dense-bitset) s)   ; ~150kb
```

Both of these are significantly smaller than the standard set, but the dense bitset is almost an order of magnitude smaller than the sparse variant.  This is because the dense bitset allocates larger contiguous chunks, which is great if the numbers are densely clustered.  However, if the numbers are sparse:

```clj
(def s (map (partial * 1e6) (range 1e6)))

(into #{} s)              ; ~100mb
(into (sparse-bitset) s)  ; ~130mb
(into (dense-bitset) s)   ; ~670mb
```

In this case, the dense bitset is much less efficient than the standard set, while the sparse bitset is about equally large.  So as a rule of thumb, use `dense-bitset` where the elements are densely clustered (each element has multiple elements within +/- 1000), and `sparse-bitset` for everything else.

The bitsets are both somewhat faster than the standard set, as well; adding elements to the transient bitset can be ~50-100% faster than a normal set.

### license

Copyright © 2013 Zachary Tellman

Distributed under the [MIT License](http://opensource.org/licenses/MIT)
