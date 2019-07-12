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
* Rename `Exclusive` to `Avoidable` typeclass and `exclusive` to `idle` method
* Define `Tagged` datatype for attaching type information to the value
* Define `Proxy` datatype for holding no data, but having a phantom parameter
* Define `Validation` datatype (similar to `Conclusion`, but can collect errors)

# 0.1.6
* Define `->>>`, `->>>>`, `->>>>>` methods for `Traversable` to compact expressions
* Move `Natural` transformation from `Functor` to `Transformation` module
* Define infix and infix flipped versions of methods of `Functor` typeclasses
* Define `>>>-`, `>>>>-`, `>>>>>-` methods for `Distributive` to compact expressions
* Rename `<>` method of `Semigroup` typeclass to `+`
* Rename `><` method of `Ringoid` typeclass to `*`
* Rename `unit` to `zero` method of `Monoid` typeclass
* Define `Quasiring` typeclass

# 0.1.7
* Define `ifelse` function for `Boolean` values
* Change `Stack` and `Nonempty` definitions, temporarily remove `filter`
* Define `<**>`, `<***>`, `<****>` methods for `Applicative` to compact expressions
* Change `Graph` definition
* Rename `ask` to `env` method of `Environmental` datatype
* Introduce `><` type operator to separate functors from its arguments
* Define `Determinable` typeclass and define its instance for `Predicate`
* Define `curry` and `uncurry` for `Product` datatype
* Flip arguments of `statefully` method of `Stateful` datatype
* Exclude inner effects from `Environmental`, `Storage` and `Stateful` datatypes

# 0.1.8
* Rename `T` junction scheme to `UT` and move it to `Schemes` submodule and remove `up` method
* Rename `Y` junction scheme to `UTU` and move it to `Schemes` submodule and remove `:>:` type operator
* Add variance type arguments to `UT`, `UTU` and `TUT` schemes
* Rename `U` to `TU`, `UU` to `TUV`, `UUU` to `TUVW` and put them into `Schemes` module
* Define `Composition` typeclass and define its instances for `TU`, `TUV`, `TUVW`, `UT` and `UTU`
* Define `Transformer` typeclass and define its instance for `Stateful` datatype
* Replace `transform` on `lay` and add `equip` method in `Transformer` typeclass
* Define `Covariant`, `Applicative`, `Pointable`, `Bindable` and `Monad` instances for `Stateful` transformer
* Remove `:!:` type operator
* Define `Composition` and `Transformer` instances for `Maybe` and `Conclusion`
* Define `Core`, `Paradigm` and `Pattern` umbrella modules

# 0.1.9
* Change `Stack` definition: from type synonymous to newtype, change operations accordingly
* Define `.:.` composition: the same thing but with reverse order
