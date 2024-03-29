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
* Change `Nonempty` type family, family instances should be defined for each data structure
* Define `:>` type operator for transformers
* Define `Distributive` instance for `->` datatype
* Rename `idle` method of `Avoidable` typeclass to `empty`
* Remove `a` parameter from `Layout` to be able to use natural transformations in methods
* Return `filter` method to `Stack` data structure
* Rename `unwrap` to `untwist` method in `Twister` module
* Rename `composition` to `unwrap` and `Outline` to `Primary` in `Composition` type class
* Rename `equip` to `wrap` and `Layout` to `Schema` in `Transformer` typeclass
* Make `Composition` a superclass for `Transformer` typeclass
* Move all `Junction` modules to `Pattern` submodule except `Kan`

# 0.2.0
* Define `Representable` functor typeclass
* Define `Pointable`, `Applicative`, `Bindable` and `Representable` instances for `->`
* Define infix `Adjoint` operators - `-|` and `|-`
* Define `Adjoint` instance for `Stateful` and `Storage` datatypes
* Change `Graph` definition: from type synonymous to newtype
* Rename `><` to `>`, `:.:` to `.:`, `.:.` to `:.`
* Remove all instances for `Junction` schemes
* Define type operators for profunctorish types: `::|:.`, `::|.:` and `::|::`
* Define `Divariant` (also known as `Profunctor`) `Functor` typeclass

# 0.2.1
* Generalize `$` up to a method of `Divariant` typeclass
* Put concrete data structures to `Specific` submodule
* Move `Nonempty` type family to separated module
* Define `Cartesian` type class
* Rename `?` to `%` to use `?` as boolean multi-if
* Replace `ifelse` method from `Setoid` module
* Convert `:>` to a newtype
* Rename `Composition` class to `Interpreted`
* Rename `Junction` machinery to `Joint` and move it to `Controlflow` module
* Rename `>` type operator to `:=`
* Create `:#` type synonymous for `Tagged` datatype
* Remove `untag` in favor of `extract` method
* Rename `Tagged` constructor of `Tagged` to `Tag`

# 0.2.2
* Change types of `lay` and `wrap` methods of `Transformer` class
* Define `Adaptable` type class for fitting effects in schemas
* Define `Failable` and `failure` for adaptable `Conclusion`
* Define `Optional` and `nothing` for adaptable `Maybe`
* Rename `Stateful` to `State`, `get` to `current`, `put` to `replace`
* Define `Stateful` adaptable constraint and adapt `get`, `modify` and `put`
* Rename `Environmental` to `Environment`
* Make `env` adaptable effect and remove `local`
* Define `Accumulator` effect and it's adaptable `gather` method
* Define `|->` (Coalgebra) and `<-|` (Algebra) type synonyms
* Remove `oblige` in favor of `Liftable` instance of `Continuation`
* Remove `environmentally` in favor of `Interpreted` instance for `Environment`
* Remove `statefully` in favor of `Interpreted` instance for `State`

# 0.2.3
* Define `Category` type class with `identity` and `.` methods
* Change `>->` signature: `a -> b` to `v a d` and `c -> d` to `v c d`
* Extract `Schema` from `Transformer` type class as `Schematic` type family
* Rename `Transformer` type class to `Monadic`
* Define `Comonadic` type class for comonad transformers
* Define `Transformer` umbrella module and type family
* Rename `:<` data constructor to `Twister` to not confuse it with comonad transformer type operator
* Use `UT` joint scheme for `Stack` and `Graph` data structures
* Remove `Variant` type in favor of `Covariant` and `Contravariant` constraints in joint schemes
* Add `Covariant` constraint on schema parameter in `Adaptable` type class
* Rename `Storage` type to `Store`
* Define `Interpreted`, `Schematic` and `Comonadic` instances for `Store` type

# 0.2.4
* Remove `Pandora.Core.Transformation` module and move `~>` to `Functor` module
* Define `Adaptable` instances for comonad transformer schemes
* Make `position` and `access` methods of `Store` adaptable
* Rename `unwrap` method of `Interpreted` type class to `run`
* Define `Bivariant` functor type class
* Define `zoom` optical operator - apply lens within some part of `State`
* Make `top` method of `Stack` data structure a lens
* Make `zoom` method of `Optics` adaptable for bigger state
* Define `Traced` for adaptable `Environment` comonad
* Define `cata`, `ana` and `hylo` methods of `Fix`
* Define `Equipment` datatype to use `Comonad` `Product` transformer
* Define adaptable `retrieve` method of `Equipment`
* Extract `Imprint` from `Environment` module

# 0.2.5
* Define `Semiring` typeclass
* Define `Stream` module and type
* Move `Liftable` and `Lowerable` modules from `Functor` to `Transformer` submodule
* Define `Hoistable` module and typeclass
* Rename `TUV` joint schema to `TUT`
* Modify `UTU` joint schema internals
* Remove `&&` and `||` `Boolean` operators (use `*` and `+` instead)
* Define precedence for `*` and `+`
* Change superclass of `Group` class - `Quasiring` instead of `Monoid`
* Rename `Jet`'s constructor to `Jet` (previously - `:-`)
* Rename `Group`'s method from `inverse` to `invert`
* Remove `not` method of `Boolean`, (use `invert` instead)
* Remove `Injective` and `Has` type families
* Change superclasses for `Liftable` and `Lowerable` classes to provide a law

# 0.2.6
* Change order of arguments for `TUT` joint schema
* `Binary` trees can be empty
* Define `left_sub_tree` and `right_sub_tree` lenses of `Binary` tree
* Fix `insert` function: `Binary` tree should not contain duplicate values
* Remove `Cartesian` type class
* Move `Nonempty` module to `Variation` submodule
* Remove `Specific` module
* Add functional dependency to `Nonempty` type family and distribute instances
* Define `left` and `right` natural transformations from `Wye` to `Maybe`
* Define experimental `Substructure` type class
* Remove `left_sub_tree` and `right_sub_tree` lenses of `Binary` tree
* Define `this` and `that` methods from `Variation` to `Maybe`
* Define `Backwards` type to run `Applicative` actions in reverse order

# 0.2.7
* Replace `Lan` and `Ran` with `Kan` data family
* Rename `Free` to `Instruction` and its constructors: `Pure` to `Enter` and `Impure` to `Instruct`
* Define `Ring` typeclass with `Group` superclass
* Define `Weighted` type family and define instance for `Graph`
* Add `Leap` to `Edges` to indicate a connection with only the first vertex
* Remove `Graph` structure and `Weighted` type family
* Define `Reverse` type to run `Traversable` in reverse order with `Backwards`
* Rename `Twister` to `Construction` (in contrast of `Instruction`)
* Split `Basis` on `Functor` and `Transformer` modules in `Primary`
* Rename `untwist` to `deconstruct` in `Construction`
* Define `Rose` tree structure
* Replace `UT` joint scheme with `TU` in data structures
* Change `sub` method of `Substructure` - it always returns `Tagged` value
* Rename `Construction` constructor of `Construction` to `Construct`

# 0.2.8
* Define `Outline` free applicative transformer
* Define `Delta` datatype
* Define `Wedge` datatype
* Rename `Variation` to `These`
* Define experimental methods in `Bindable` class - `$>>=` and `>>=$`
* Define `here` and `there` methods from `Wedge` to `Maybe`
* Move `Boolean` definition to its own module
* Move `Ordering` definition to its own module
* Rename `Variation` submodule to `Ability` and create umbrella-module
* Define `Focusable` typeclass for getting root and creating singleton
* Replace `top` method of `Stack` structure with `Focusable` instance
* Add `Covariant` constraint on `natural transformation` in `Hoistable` typeclass

# 0.2.9
* Define `Zipper` type family to walk datastructures with context
* Define `Tap` datatype for context dependent values
* Define type synonyms for `TU`, `UT`, `TUT` and `UTU` joint schemes
* Move `Schemes` module to `Paradigm` submodule
* Move `$` definition from `Divariant` to `Category`
* Define generalized point free combinators in `Covariant` module
* Rename `Joint` module To `Effect`
* Define left and right `zig`, `zig-zig` and `zig-zag` `Splay` rotations for `Binary` tree
* Define experimental methods in `Extendable` class - `$=>>` and `<<=$`
* Define `Rotatable` typeclass to rotate trees
* Rename `sub` method of `Substructure` typeclass to `substructure`
* Define `sub` method which doesn't involve `Tagged`
* Change `Focusable` typeclass - now it's possible to point not only top/root

# 0.3.0
* Rename `Root` datatype to `Location` with `Head` (stack) and `Root` (tree) constructors
* Define `represent` as lens in `Representable` containers
* Define `Equivalence` datatype and define `Contravariant` instance for it
* Define `swop` for `Wye` (useful if you want to invert binary tree with `hoist swap`)
* Add experimental `Monotonic` typeclass
* Implement delete method for Stack
* Define `Comprehension` wrapper for data structures that can behave like list comprehensions
* Define experimental `Insertable` typeclass
* Define `Day convolution` datatype in `Transformer` module
* Define experimental methods: `<*+>`, `<**+>`, `<***+>`
* Rename Bindable method `>>=$` to `<>>=`
* Move `Comprehension` wrapper from `Construction` to `Structure.Ability` module

# 0.3.1
* Define `Set` interface for data structures with `member` method
* Move `zoom` function to `Inventory` module
* Move `Adjoint` instance for `(:*:)` and `(->)` to `Paradigm.Primary.Functor` module
* Remove `Schematic` module and move `Schematic` type family to `Interpreted` module
* Change arity of `Schematic` type family
* Remove `lay` method of `Monadic` type class, use `lift` instead
* Remove `flick` method of `Comonadic` type class, use `lower` instead
* Change `lift` constraint: from `Covariant` to `Traversable`

# 0.3.2
* Define experimental methods: `->>=`, `>>=-`, `-=<<`, `=<<-`
* Define `interruptable` method for `Continuation`
* Define `Catchable` typeclass to catch errors from `Conclusion`
* Define `repeat` method of `Stream` datatype
* Define `subset` method of `Set` interface
* Define `satisfy` method in `Predicate` module
* Make `fold` and `find` stateful expressions
* Define `equate` method of `Predicate` dataype
* Define `Zipper` instance of `Nonempty Stack`
* Define `forward'` and `backward'` methods for `Zipper` of `Nonempty Stack`
* Rename `iterate` method of `Monotonic` typeclass to `bypass`
* Rename `coiterate` method of `Construction` datatype to `iterate`

# 0.3.3
* Define inductive `Natural` number datatype in `Object` module
* Add `cardinality` method to `Set` interface
* Define new functor schemes: `T_`, `T_U`, `U_T`
* Define `Zipper` instance for `Nonempty Binary`
* Replace `forward`, `forward'`, `backward`, `backward'` methods with `Rotatable` instances
* Add `>-<` operator to `Invariant` typeclass
* Add `:~.` type synonymous for polymorphic lens
* Replace `maybe` with more generic `bypass`
* Rename `bypass` method of `Monotonic` to `reduce`
* Add `resolve` method of `Monotonic` typeclass
* Rename `iterate` method of `Construction` to `.-+`
* Define `via` method to use transformer as wrappers

# 0.3.4
* Define `branches` to create `Wye a` from two `Maybe a`
* Rename `<:.:>`,`>:.:>`,`<:.:<`,`>:.:<` to `<:*:>`,`>:*:>`,`<:*:<`,`>:*:<`
* Add `unite` method to `Interpreted` typeclass
* Rename `Direction` to `Vertical`
* Define `Nullable` typeclass for structures which can be null
* Define `>>=:>` and `<:=<<` methods to use `>>=` with `adapt`
* Define `not` method for `Predicate` inversion
* Make `Monoid` a superclass for `Group`
* Define `-` method in `Group` typeclass
* Rename `via` to `-=:` in Interpreted module
* Flip arguments for `Bindable` `$>>=` and `Extendable` `$=>>` methods
* Define experimental methods: `-|$`, `$|-`

# 0.3.5
* Define inductive `Denumerator` number datatype in `Object` module
* Rename `Natural` datatype to `Numerator`
* Define `Measurable` typeclass to measure datastructures
* Define `Convertible` typeclass for structure transformation
* Define `binary` method to construct `Binary` trees from other structures
* Remove `pop` method of `Stack`, use `Substructure` `Tail` instance instead
* Appear additional type parameter in `Substructure` typeclass
* Define `Divisible` typeclass
* Define `Function` module to contain instances for function
* Move `!`, `%`, `&` to `Function` module
* Remove `Pandora.Core.Morphism` module
* Define `$$|-`, `$$$|-`, `$$$$|-` methods of `Adjoint` typeclass
* Define `match` method to chain predicates

# 0.3.6
* Define `Walk` datatype to define different tree/graph traversals
* Make `a` paremeter in `:~.` to be existentially qualified
* Rename type operators: `|->` to `:=>` and `<-|` to `<:=`
* Define experimental `||=` method to apply a function inside transformer
* Define `subview`, `subplace` and `substitute` methods in `Substructure` typeclass
* Put `sub` method into `Substructure` typeclass
* Remove `^.`, `.~` and `%~` infix optics methods
* Generalize `T_U` and `U_T` schemes over some `p` (`Bifunctor`/`Profunctor`)
* Define experimental `Deletable` typeclass
* Define experimental `adjust`, `magnify`, `=<>`, `~<>`
* Define experimental `Accessible` type class
* Rename `access` method to `look` in `Optics` module
* Rename `Convertible` type class to `Morphable`
* Remove `Rotatable` type class in favor of `Morphable`
* Define `<:.:>`, `>:.:>`, `<:.:<`, `>:.:<` type synonyms
* Delete `Delta` datatype in favor of `(:*:) <:.:> t`

# 0.3.7
* Rename `insert` method to `+=` in `Insertable` typeclass
* Rename `delete` method to `-=` in `Deletable` typeclass
* Create `Some` umbrella module for certain data structures
* Define experimental `~~>` type synonyms
* Rename `/=` method of `Setoid` typeclass to `!=`
* Define `rotate` and `into` method of `Morphable` typeclass
* Remove `T_` joint scheme in favor of `T_U`
* Define `Prefixed` wrapper for data structures whose values can be prefixed
* Move `Comprehension` and `Prefixed` modules from `Ability` to `Modification`
* Define experimental `Dictionary` typeclass
* Add `=||` method to `Interpreted` typeclass
* Remove `hush`, `left`, `right`, `this`, `there`, `here`, `note` methods in favor of `Morphable` instance
* Define `Flip` transformer for flipping type arguments
* Replace two parementers in `>->` method of `Divariant` with usual functions

# 0.3.8
* Define `twosome` method to lift two parts to `<:.:> (:*:)` scheme
* Remove `branches` method in favor of `Morphable` instance
* Change order of type arguments in `T_U` scheme
* Define `!!` and `!!!` infix operators
* Define `premorph` to replace `extract . run` expressions for `Morphable` instances
* Define experimental `/` for providing arguments to function
* `modify` and `replaces` methods of `State` effect returns result of an applied function
* Change precedence of `->>` method of `Traversable` - from 3 to 5
* Rename `Stack` datastructure to `List`
* Define new type synonymous: `:=:=>`
* Define experimental `Insert`, `Push`, `Pop` verbs for `Morphable` ability
* Remove `Insertable` ability for datastructures
* Change order of arguments in `<:.:>`, `>:.:>`, `<:.:<`, `>:.:<` type synonyms
* Define fixity and precedence for `TU`, `UT`, `TUT` and `T_U` type synonyms
* Define `Stack` typeclass with no methods but with `Push` and `Pop` constraints

# 0.3.9
* Remove `Deletable` ability for datastructures in favor of `Morphable Delete First` instance
* Generalize `filter` and move it from `List` to `Morphable` module
* Define `Set` interface as constraint
* Generalize `Equivalence` and rename it to `Convergence`
* Define `Equivalence` and `Comparison` as type synonyms for `Convergence`
* Define version of `<*>` with flipped arguments - `<%>`
* Generalize `||=` and `=||` methods of `Interpreted` on another `Interpeted` functor
* Define `pass` method in `Pointable` typeclass
* Define experimental `reconcile` method in `State` module
* Remove `Maybe` from `Prefixed` type synonymous
* Define `discover` for `Morphable Find` instances with composite keys
* Change `subset` method of `Set` interface: returns `Convergence Boolean`
* Remove `Dictonary` typeclass
* Define `Lookup` verb for finding elements in datastructures by key
* Define `Cycle` typeclass with `Chain` superclass
* Move `lookup` and `discover` exressions to `Dictionary` module

# 0.4.0
* Define size typed `Vector` data structure
* Move `Vertical` datatype from `Binary` to `Morphable` module
* Remove `substitute`, `subplace`, `subview` methods from `Substructure` typeclass
* Remove `>>=:>` and `<:=<<` infix operators
* Move `Vector` datatype to `Linear` umbrella-module
* Rename `/` infix function application operator to `#`
* Define `$:` and `$::` infix function application operators
* Decrease precedence for `:*:` infix operator
* Change arguments order in `order` expression
* Rename `$:` to `#`, `$::` to `#:`, `$:::` to ` #::` and define `#:::`
* Remove `#:`, `#::`, `#:::` infix operators
* Change precedence for `.` infix operator
* Change precedence for `*`, `+`, `-` infix operators
* Change precedence for `#` infix operator
* Move `Linear` module to `Paradigm.Primary`
* Define experimental `Vectorize` typeclass
* Define `Matrix` datatype as combination of `Vector`'s
* Define `|>` datatype to use it for combining `Substructure` instances
* Define `Substructured` type synonymous as constraint kind

# 0.4.1
* Generalize `$` and `#` `Category`'s infix operators
* Add `Covariant` constraint in `Bivariant` and `Divariant` typeclasses definition
* Add infix version of `hoist` method in `Hoistable` typeclass - `/|\`
* Define `PQ_` joint schema exclusively for `Lens` type
* Change `Lens` representation - wrap it in `PQ_` joint schema
* Remove `|>` lens composition operator in favor of `.` `Category` method
* Rename `.|..`, `.|...`, `.|....` to `.#..`, `.#...`, `.#....` and move them to `Covariant` typeclass
* Change precedence for `||=` and `=||` infix operators
* Define precedence for `>-<` infix operator
* Move `|>` existential polymorphic type to `Core.Functor` module
* Remove `|>` existential polymorphic type and its Substructure instance
* Remove `lookup` and `discover` methods from `Dictionary` interface
* Define `lookup` method in `Morphable` ability module
* Remove `binary` expression from `Binary` module
* Define `Morphed` type synonymous as constraint kind
* Define `Combinative` type family
* Remove `Location` datatype in `Focusable` module
* Remove `Focusable` ability if favor of `Structure` typeclass
* Define `<$$`, `<$$$`, `<$$$$`, `$$>`, `$$$>`, `$$$$>` infix operators in `Covariant` typeclass
* Define `<$||=`, `<$$||=`, `<$$$||=`, `<$$$$||=` infix operators in `Interpreted` typeclass
* Define `=||$>`, `=||$$>`, `=||$$$>`, `=||$$$$>` infix operators in `Interpreted` typeclass

# 0.4.2
* Rename `>-<` method of `Invariant` typeclass to `<$<`
* Define `P_T` joint schema exclusively for `Prism` type
* Define `Prism` type and its `preview` method
* Redefine `Lens` type as `Optics` on `Identity`
* Define `#=@` type operator as synonymous for `Optics`
* Redefine `Substructure` typeclass with polymorphic `Optics`
* Generalize optical `view`, `set`, `over` methods
* Change `Substructured` constraint type according to new `Substructure` definition
* Define `:::` - type operator for almost anything higher kinded
* Redefine `Zipper` - add possible moves to type definition
* Define experimental `Fastenable` type family to have it as `Constraint`
* Remove `:~.` type operator in `Optics` module
* Rename `Optics` type synonymous to `Lens`
* Define `PTU` joint schema
* Define `P_Q_T` joint schema
* Redefine `Lens` type synonymous and use `P_Q_T` joint schema
* Define `Convex` and `Obscure` type families for `Lens`
* Change `Substructured` constraint - add paramater for `Available`
* Define experimental `Impliable` typeclass
* Remove `:-.` type operator in `Optics` module

# 0.4.3
* Rename `!` to `!.` in `Function` module
* Rename `!!` to `!..` in `Function` module
* Rename `!!!` to `!...` in `Function` module
* Define experimental `Covariant_` typeclass
* Define experimental `-<$$>-` method of `Covariant_` typeclass
* Define experimental `-.#..-` method in `Function` module
* Define experimental `Contravariant_` typeclass
* Define experimental `Pointable_` typeclass
* Define experimental `Bindable_` typeclass
* Define experimental `Bivariant_` typeclass
* Define experimental `Divariant_` typeclass
* Define experimental `Extendable_` typeclass
* Use experimental `Covariant_` as superclass of `Extractable`
* Define `Covariant_` superclasses for `Bivariant_`
* Define `Covariant_` and `Contravariant_` superclasses for `Divariant_`
* Define experimental `-<<$$>-` and `-<$$>>-` methods of `Covariant_` typeclass
* Use experimental `Covariant_` as superclass of `Pointable`
* Define experimental `Adjoint_` typeclass
* Define experimental `Applicative_` typeclass
* Define experimental `Divisible_` typeclass
* Define experimental `-<*>-` method
* Use experimental `Covariant_` as superclass of `Lowerable`
* Remove `Covariant` superclass of `Liftable`
* Remove `Covariant` superclass of `Hoistable`

# 0.4.4
* Remove experimental `Pointable_` typeclass
* Use experimental `Covariant_` as superclass of `Distributive`
* Use experimental `Covariant_` as superclass of `Traversable`
* Define experimental `Traversable_` typeclass
* Define experimental `Distributibe_` typeclass
* Define experimental `-<<-<<-` method of `Traversable_` typeclass
* Remove `Adjoint` typeclass
* Rename experimental `Adjoint_` to `Adjoint`
* Remove `Bivariant` typeclass
* Rename experimental `Bivariant_` to `Bivariant`
* Remove `Distributive` typeclass
* Rename experimental `Distributive_` to `Distributive`
* Remove `Divariant` typeclass
* Rename experimental `Divariant_` to `Divariant`
* Rename `Product` type to `:*:`
* Move `Paradigm.Primary.Functor.Function` module to `Paradigm.Primary.Algebraic.Exponential`
* Move `Paradigm.Primary.Functor.Product` module to `Paradigm.Primary.Algebraic.Product`
* Add `Covariant_`, `Pointable` and `Applicative_` constraints to `Traversable_` typeclass
* Remove `join_` method from `Bindable_` typeclass
* Remove `Traversable` typeclass
* Rename experimental `Traversable_` to `Traversable`
* Remove `Bindable` typeclass
* Rename experimental `Bindable_` to `Bindable`
* Use experimental `Extendable_` as superclass for `Comonad`
* Remove `Extendable` typeclass
* Rename experimental `Extendable_` to `Extendable`
* Rename experimental `Applicative_` to `Semimonoidal`

# 0.4.5
* Define `Endofunctor` type family
* Define `Sum` module in `Algebraic` with `:+:` type
* Define experimental `Day_` transformer
* Define experimental `Semimonoidal_` typeclass
* Define `Semigroupoid` typeclass as generalization of `Category`
* Define `Applicative_` and `Alternative_` type class aliases of `Semimonoidal_`
* Define experimental `-*-` method of `Semonoidal_` typeclass
* Define experimental `-+-` method of `Semonoidal_` typeclass
* Use experimental `Semimonoidal_` as superclass of `Traversable`
* Remove `-<*>-` method of `Semimonoidal` typeclass
* Remove `Semimonoidal` typeclass
* Rename `Semimonoidal_` typeclass to `Semimonoidal`
* Rename `-*-` method of `Semimonoidal` to `-<*>-`
* Use `Semigroupoid` as superclass for `Semimonoidal`
* Define `*>-` and `forever_` methods of `Applicative_` typeclass
* Remove `<+*>`, `<**+>`, `<***+>` methods
* Remove `Applicative` typeclass
* Remove `satisfy` method of `Predicate`
* Remove `Alternative` typeclass
* Remove `Avoidable` typeclass
* Change `-<$$$>-`: works with endofunctors
* Define `-<$$$$>-` operator of `Covariant_` typeclass
* Use experimental `Covariant_` in `Interpreted`
* Define `$>-`, `$$>-`, `$$$>-` methods of `Covariant_` typeclass
* Remove `Covariant` typeclass
* Rename experimental `Covariant_` to `Covariant`
* Remove `Determinable` typeclass
* Remove `Divisible_` typeclass
* Remove `Contravariant` typeclass
* Rename experimental `Contravariant_` to `Contravariant`

# 0.4.6
* Define `void` method in `Algebraic` module
* Define `<--` type synonymous for reverted arrows
* Define `Zero` module in `Algebraic` directory
* Define `One` module in `Algebraic` directory
* Rename `One` constructor in `Denumerator` to `Single`
* Define `Unit` type family
* Define `Monoidal` typeclass
* Define `absurd` method
* Define `empty` method
* Define `sum` method
* Add `Monoidal` constraint to `Applicative` and `Alternative` constraints
* Define `point_` method
* Use `Monoidal` superclass in `Traversable` instead of `Pointable`
* Use `Monoidal` superclass in `Monad` instead of `Pointable`
* Remove `Pointable` typeclass
* Rename `point_` to `point`
* Define `extract_` method
* Remove `Extractable` typeclass
* Rename `extract_` to `extract`
* Use `Monoidal` as superclass of `Comonad`
* Rename `multiply_` to `multiply`
* Rearrange type parameters in `Covariant` typeclass
* Rearrange type parameters in `Contravariant` typeclass
* Rearrange type parameters in `Semimonoidal` typeclass
* Rearrange type parameters in `Adjoint` typeclass
* Rearrange type parameters in `Bivariant` typeclass
* Rearrange type parameters in `Traversable` typeclass
* Rearrange type parameters in `Distributive` typeclass
* Rearrange type parameters in `Bindaple` typeclass
* Rearrange type parameters in `Extendable` typeclass
* Rearrange type parameters in `Monoidal` typeclass
* Rearrange type parameters in `Divariant` typeclass
* Generalize `Liftable` typeclass
* Generalize `Lowerable` typeclass

# 0.4.7
* Remove `hoist` method from `Hoistable` typeclass
* Rename `-<$>-` method to `<$>` in `Covariant` typeclasss
* Rename `->$<-` method to `>$<` in `Contravariant` typeclass
* Define `Straight` transformer
* Define `-->` type synonymous
* Define `Opposite` type family
* Define experimental `Appliable` typeclass
* Rename `multiply` method to `mult` in `Semimonoidal` typeclass
* Generalize `Interpreted` typeclass
* Define `Groupoid` typeclass
* Remove `Measurable` typeclass
* Rename `Applicative_` typeclass to `Applicative`
* Rename `Alternative_` typeclass to `Alternative`
* Rename `-<*>-` method of Applicative to `<-*-`
* Add functional dependencies to `Appliable` typeclass
* Remove `-<$$>-`, `-<<$>-`, `-<$$>>-` in favor of `<$$>`
* Remove `-<$$$>-` in favor of `<$$$>`
* Remove `-<$$$$>-` in favor of `<$$$$>`

# 0.4.8
* Define `Zippable` type family and remove moves from `Zipper` type alias
* Add `!` infix operator as synonymous for `run`
* Remove `Appliable` module
* Generalize `Adaptable` typeclass
* Generalize `Hoistable` typeclass
* Define experimental notation for map methods of `Covariant` typeclass
* Define experimental notation for map methods of `Contravariant` typeclass
* Define `Tape` type synonymous for List and Stream zippers
* Define `only` method of `Substructure` typeclass
* Change fixity and precedense of `!` method of `Interpreted` typeclass
* Rename `*>-` `Applicative` operator to `-*-`
* Rename `-+-` `Alternative` operator to `<-+-`
* Define `-+-` method of `Alternative` typeclass
* Define experimental `intensify` method as replacement of `zoom`
* Define `#` type operator for consuming arguments
* Define experimental `Possible` type class
* Remove `magnify` and `adjust` methods
* Remove `zoom` method in favour `intensify`
* Rename `intensify` method to `zoom`
* Define `Betwixt` type family to avoid specifying intermediate categories
* Define experimental `overlook` method
* Remove `<$$$$>`, `<$$$$||=` and `=||$$$$>` operators
* Define `Effectful` typeclass to delegate instance resolution for `Adaptable`
* Change order of type parameters in `Adaptable` typeclass

# 0.4.9
* Define `<-*-*-` method of `Applicative` typeclass
* Define `-*-*-` method of `Applicative` typeclass
* Define `TT` scheme where two effects are known
* Use `TT` instead of `TU` in `List` and `Binary`
* Define `Lensic` typeclass to combine different lenses
* Use `TT` instead of `TU` in `Tape` instances
* Rename Environment module and type to `Provision`
* Rename `Configured` to `Provided`
* Rename `env` to `provided`
* Rename `-<<-<<-` operator of `Traversable` to `<<-<<-`
* Rename `-*-` and `-*-*-` to `.-*-` and `.-*-*-`
* Define experimental `Kernel` typeclass
* Remove `!.`, `!..`, `!..` operators in `Exponential` module
* Move `<-|-|-` and `<-|-|-|-` under `Covariant` typeclass
* Change priority of `:*:` operator
* Change priority of `!` operator
* Remove `$` operator in favor of `!`

# 0.5.0
* Define experimental `<-|-<-|-`, `<-|->-|-`, `>-|->-|-`, `>-|-<-|-` methods
* Define `>-|-|-` method in `Contravariant` typeclass
* Remove `Bivariant` typeclass
* Remove `Divariant` typeclass
* Define `Turnover` wrapper for data structures that can that can be rotated indefinitely
* Wrap left part of `Tape` into `Reverse`
* Rename `-+-` operator to `.-+-` in Algebraic module
* Define `Ability` umbrella module (like in `Structure`) in `Inventory`
* Define experimenal `Gettable`, `Settable` and `Modifiable` typeclasses
* Define `Some` umbrella module (like in `Structure`) in `Inventory`
* Remove `current`, `replace`, `modify` methods of `State` effect
* Remove `view` method of `Lens`
* Remove `set` method of `Lens`
* Define experimental `Conditional` typeclass
* Remove `?` method of `Boolean` datatype
* Remove `over` method of `Lens`

# 0.5.1
* Define `Simplification` type family to extract type parameter from Identity
* Redefine `Stack` structure interface
* Define experimental from `<--` to `<----------`, operators in `Category` typeclass
* Define experimental from `-->` to `---------->`, operators in `Category` typeclass
* Define experimental `-->` operator in `Category` typeclass
* Remove `zoom` method in `Inventory` module
* Define `Zoomable` typeclass with `zoom` method
* Rename `-.#..-` operator to `.:..`
* Rename `Identity` to `Exactly`
* Define length encoding operators in `Covariant` class
* Define length encoding operators in `Contravariant` class
* Define length encoding operators in `Traversable` class
* Define length encoding operators in `Distributive` class
* Define length encoding operators in `Extendable` class
* Define length encoding operators in `Bindable` class
* Define length encoding operators in `Adjoint` class
* Change precedence for `<-*-*-` and `.-*-*-`
* Change precedence for `<-|-<-|-`, `<-|->-|-`, `>-|->-|-`, `>-|-<-|-`
* Define length encoding operators from `<-*--` to `<-*-------`
* Define `<-||-` and `>-||-` operators in `Algebraic` module
* Change precedence for `:*:`
* Change precedence for `:+:`
* Rename 'forever_' to 'loop'
* Remove `Nullable` typeclass

# 0.5.2
* Remove `#` operator from `Category` typeclass
* Change precedence for `==` and `!=`
* Remove `Conditional` typeclass ant its `?` operator
* Define `Conditional` typeclass with `iff` method
* Define `<~` length encoding operators in `Interpreted` class
* Define `<-|-` length encoding operators in `Covariant` class
* Remove `!` operator from `Interpreted` class
* Change precedence fro `:*:` operator - from 6 to 8
* Move `Simplification` type family to `Primary` module
* Refactor `Substructure` class: remove `Available` type family
* Rename `#` type operator to `<` and lower its precedence
* Define `>` operator for type application
* Define `<` operator for type parameters passing
* Remove `:=` type operator
* Change precedence fro `+`, `-`, `*` operators - to 9
* Define `<-||-` length encoding operators in `Algebraic` module
* Define `>-||-` length encoding operators in `Algebraic` module
* Define `Horizontal` type and use it in binary tree zipper
* Define `?=` operator as replacemet for if statement
* Define `<:*:>`, `>:*:>`, `<:*:<`, `>:*:<` type synonyms
* Define `<:+:>`, `>:+:>`, `<:+:<`, `>:+:<` type synonyms
* Define `Functor` module in `Algebraic`
* Change definition of binary tree
* Change `Zipper` definition - tag it

# 0.5.3
* Move `Algebraic` module from `Primary` to `Paradigm`
* Define `<:*:>` infix operator
* Rename `Tail` constructor of `Segment` to `Rest`
* Remove 'twosome' in favor `<:*:>`
* Rename `?==` operator to `?=`
* Remove `Conditional` module
* Change operators' precedence, length of operators define precedence
* Remove `leaf`, `left`, `right`, `both` in `Binary` module
* Define `Zipper` instance of `Nonempty Rose` tree
* Change `Zipper` definition - add `structure` in tag
* Define `Roses` type synonymous
* Move `Tape` to `Structure.Modification.Tape` module

# 0.5.4
* Define `<` length encoding type operators in `Functor` module
* Change precedence for `::|:.`, `::|.:`, `::|::`, `.:`, `:.`, `:::`
* Define `>` length encoding type operators in `Functor` module
* Change precedence for `<:.>`, `>:.>`, `<:.<`, `>:.<`,  `<::>`, `>::>`, `<::<`, `>::<`
* Rename `.-+-` `Alternative` operator to `-+-`
* Change `Prefixed` definition - from newtype to type alias
* Remove `Breadcrumbed` type synonymous
* Change `Zipper` definition - remove tagging
* Rename `.-+` `Construction` operator to `constitute`
* Define `Slidable` typeclass for data structures
* Define `current` and `change` methods in `State`
* Remove `Zoomable` typeclass, leave `zoom` method
* Change `zoom` definition - not an adaptable effect
* Change `change` method of `State` - return an old value
* Define `transwrap` lens for underlying effect in transformer
* Change `pop` method in `Stack` typeclass: returns not `Maybe` but `Topping t`
* Remove `Pushing` and `Popping` type familes from `Stack` typeclass
* Move `Interpreted` module to `Core`
* Move `Schematic` type family to `Schemes` module
* Move `Vertical` and `Horizontal` datatypes to `Auxiliary` module
* Rename `Horizontal` constructors to `Left` and `Right`
* Move `Nonempty` from `Ability` to `Modification` module

# 0.5.5
* Change `Nonempty` type family: close it
* Change `Zipper` of `Binary` tree definition
* Define `reconstruct` method of `Construction` for schemes
* Define `Trip` datatype as `Flip` extension
* Define `<-|||-` length encoding operators in `Algebraic` module
* Rename `<<-` method of `Traversable` to `<-/-`
* Define experimental `<<-/-` method
* Define `primary` lens for `Interpreted` values
* Rename `.-*-` length encoding operators to `-*-`
* Define `Location` datatype
* Remove `Fastenable` type family
* Define `fasten` method in `Zippable` typeclass
* Change `Zipper` of `Rose` tree definition
* Define precedence and fixity for `@>>>` operator
* Define `unfasten` method in `Zippable` typeclass
* Define `probably` method of `State` effect
* Rename `<-/-<-/-` `Traversable` method to `<-/-/-`
* Decrease precedence for `:*:` infix operator
* Rename `-*-` length encoding operators to `--*`
* Change definitions of `--*` length encoding operators: arguments order
* Define another `-*` length encoding operator
* Change definition of `-*-*-` operator: arguments order
* Change definition of `-+-` operator: arguments order
* Rename `-+-` operator to `-+`
* Define  `-+` length encoding operators
* Remove `reconstruct` method of `Construction`
* Define `Cardinality` type family
* Define `:+*+:` type operator
* Define `:*+*:` type operator

# 0.5.6
* Define `until` method for repeating actons until failure
* Define `definitely` method to simplify `State` transformer
* Define `magnify` method to `zoom` states in `State` transformers
* Rename `<-||-` length encoding operators to `<<-|-`
* Define  priority and fixity for `-+` length encoding operators
* Define `<<-|-|-` length encoding operators
* Define `:++*:` type operator
* Rename `<--` type synonymous of `Flip (->)` to `--<`
* Define generalised `Functor` typeclass
* Define `Covariant_` type synonymous for `Functor` typeclass
* Define `Contravariant_` type synonymous for `Functor` typeclass
* Define `Kleisli` morphism
* Define `Bindable_` type synonymous for `Functor` typeclass
* Rename `Pattern.Transformer` module to `Pattern.Transformation`
* Define `Component` experimental typeclass
* Define `Transformation` experimental typeclass
* Define experimental `Liftable_` type alias
* Define experimental `lift_` method
* Define experimental `Lowerable_` type alias
* Define experimental `lower_` method
* Define `Tensor` product in `Morphism` module
* Define experimental `Semimonoidal_` type alias
* Define `Operation` umbrella module in `Pattern`
* Move `One` module from `Algebraic` to `Operation`
* Move `Zero` module from `Algebraic` to `Operation`
* Move `Exponential` module from `Algebraic` to `Operation`
* Move `<:*:>`, `<:*:<`, `>:*:>`, `>:*:<` to `Algebraic` module
* Move `Product` module from `Algebraic` to `Operation`
* Move `Unit` type family to `Unit` module

# 0.5.7
* Define `Liftable` morphism in `Morphism` module
* Define `Traversable_` type synonymous for `Functor` typeclass
