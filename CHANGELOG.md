# 0.1.1
* Define `attached` destructor for `Product` datatype
* Define `empty` destructor for `Stack` structure
* Replace `flip` on `?` type operator
* Remove type parameter from `Stack` type synonymous
* Split `structure` and `value` in `Nonempty` type family
* Define `Invariant` instance for `Constant` datatype
* Remove all `Invariant` methods except `invmap`
* Extract `Junction` module from `Basis`

# 0.1.2
* Define `Pipeline` control flow paradigm
* Split `Structure` modules on `Property` and `Concrete`
* Define `Hollow` type class for handling empty structures
* Extract `Nonempty` into a separated module
* Define `Graph` concrete structure
* Define infix `:-.` type operator for `Lens`
* Define `Object` instances for `Product` datatype

# 0.1.3
* Define `Object` instances for `Cofree` datatype
* Define all `Object` pattern instances for `Constant` datatype
* Define `reset` and `shift` methods for `Continuation` datatype
* Define `Endo` datatype in `Basis` module
* Define `Object` instances for transformer schemes
* Define `Binary` tree concrete structure
* Define some `Object` instances for `Jack` datatype
* Remove `Hollow` ad-hoc typeclass
* Merge `Property` and `Concrete` modules back

# 0.1.4
* Define `Jet` datatype in `Basis` module
* Add `fail` method for `Conclusion` datatype
* Define `find` method in terms of stateful traversing
* Define `filter` method for `Stack` datastructure
* Define `loeb` method for `Covariant` type class
* Define `Variation` datatype in `Basis` module
* Define infix versions of `comap` with various nesting levels
* Define infix versions of `contramap` with various nesting levels
* Rename `Product` constructor from `:*` to `:*:`
* Define `Has` and `Injective` type families for `Product` proofs

# 0.1.5
* Add `<&>` and `>&<` methods for `Covariant` and `Contravariant` functors accordingly
* Define `Traversable` instance for `Product` datatype
* Rename `Cofree` to `Twister` datatype (we will use the first name later)
* Define fixity for `Jet`'s and `Twister`'s constructors
