{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Binary where

import Pandora.Core.Functor (type (~>), type (>), type (>>>), type (>>>>>), type (>>>>>>), type (<), type (:=>))
import Pandora.Core.Interpreted (run, unite, (<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (-->), (--->))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|---), (<-|-|-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<), (====<<), (=====<<)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>), attached)
import Pandora.Paradigm.Algebraic.Exponential ((%), (&), (.:..))
import Pandora.Paradigm.Algebraic.Functor ((<-*-), (<-*--), (<-*---), (<-*-*-), (-*------), extract, point, empty, void)
import Pandora.Paradigm.Primary.Auxiliary (Vertical (Up, Down), Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Object.Ordering (order)
import Pandora.Paradigm.Primary.Auxiliary (Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Functor (Comparison)
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), reconstruct)
import Pandora.Paradigm.Controlflow.Effect.Transformer ((:>), wrap)
import Pandora.Paradigm.Inventory.Ability.Gettable (get)
import Pandora.Paradigm.Inventory.Ability.Settable (set)
import Pandora.Paradigm.Inventory.Ability.Modifiable (modify)
import Pandora.Paradigm.Inventory.Some.State (State, change, current)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Obscure, view, replace, mutate, primary)
import Pandora.Paradigm.Inventory (zoom, overlook)
import Pandora.Paradigm.Structure.Modification.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), morph, premorph, Morph (Rotate, Into, Insert, Lookup, Key), lookup)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure), Segment (Tree, Root, Branch, Ancestors, Children, Medium), sub)
import Pandora.Paradigm.Structure.Ability.Slidable (Slidable (Sliding, slide))
import Pandora.Paradigm.Structure.Interface.Stack (push, pop)
import Pandora.Paradigm.Structure.Interface.Zipper (Zipper, Zippable (Breadcrumbs))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed)
import Pandora.Paradigm.Structure.Some.List (List)
import Pandora.Paradigm.Schemes (TT (TT), T_U (T_U), P_Q_T (P_Q_T), type (<::>), type (<:.:>))

type Binary = Maybe <::> Construction (Maybe <:*:> Maybe)

--rebalance :: Chain a => (Wye :. Construction Wye > a) -> Nonempty Binary a
--rebalance (Both x y) = extract x <=> extract y & order
--	# Construct (extract x) (Both # rebalance (deconstruct x) # rebalance (deconstruct y))
--	# Construct (extract y) (Both # x # rebalance (deconstruct y))
--	# Construct (extract x) (Both # rebalance (deconstruct x) # y)

-- instance Morphable Insert Binary where
	-- type Morphing Insert Binary = (Exactly <:.:> Comparison > (:*:)) <:.:> Binary > (->)
	-- morphing struct = case run ---> premorph struct of
		-- Nothing -> T_U <-- \(T_U (Exactly x :*: _)) -> lift <-- point x
		-- Just binary -> T_U <-- \(T_U (Exactly x :*: Convergence f)) ->
			-- let continue xs = run <-- morph @Insert @(Nonempty Binary) xs <--- twosome <-- Exactly x <-- Convergence f in
			-- let step = iff @Just <-|-|- get @(Obscure Lens) <-*-*- modify @(Obscure Lens) continue <-*-*- set @(Obscure Lens) <-- point x in
			-- lift <---- order binary
				-- <--- step <-- sub @Left <-- binary
				-- <--- step <-- sub @Right <-- binary
				-- <--- f x <-- extract binary

-------------------------------------- Non-empty binary tree ---------------------------------------

instance Morphable (Into Binary) (Construction (Maybe <:*:> Maybe)) where
	type Morphing (Into Binary) (Construction (Maybe <:*:> Maybe)) = Binary
	morphing = lift . premorph

-- instance Morphable Insert (Construction Wye) where
	-- type Morphing Insert (Construction Wye) = (Exactly <:.:> Comparison > (:*:)) <:.:> Construction Wye > (->)
	-- morphing (premorph -> struct) = T_U <-- \(T_U (Exactly x :*: Convergence f)) ->
		-- let continue xs = run <--- morph @Insert @(Nonempty Binary) xs <---- twosome <--- Exactly x <--- Convergence f in
		-- let step = iff @Just <-|-|- (run .:.. view) <-*-*- mutate continue <-*-*- replace (point x) in
		-- order struct
			-- <---- step <--- sub @Left <--- struct
			-- <---- step <--- sub @Right <--- struct
			-- <---- f x <--- extract struct

-------------------------------------- Prefixed binary tree ----------------------------------------

instance Chain k => Morphable (Lookup Key) (Prefixed Binary k) where
	type Morphing (Lookup Key) (Prefixed Binary k) = (->) k <::> Maybe
	morphing struct = case run . run . premorph <-- struct of
		Nothing -> TT <-- \_ -> Nothing
		Just tree -> TT <-- \key ->
			key <=> attached (extract tree) & order
				<--- Just --> extract --> extract tree
				<--- lookup @Key key . TT @Covariant @Covariant =<< run (view <-- sub @(Left Branch) <-- tree)
				<--- lookup @Key key . TT @Covariant @Covariant =<< run (view <-- sub @(Right Branch) <-- tree)

-- instance Chain k => Morphable (Vary Element) (Prefixed Binary k) where
	-- type Morphing (Vary Element) (Prefixed Binary k) = ((:*:) k <::> Exactly) <:.:> Prefixed Binary k > (->)
	-- morphing struct = case run . run . premorph ! struct of
		-- Nothing -> T_U ! \(TT (key :*: Exactly value)) -> Prefixed . lift . point ! key :*: value
		-- Just tree -> T_U ! \(TT (key :*: Exactly value)) ->
			-- let continue = ((vary @Element @k @_ @(Prefixed Binary _) key value -#=) -#=) in
			-- Prefixed . lift ! key <=> attached (extract tree) & order
				-- # over (sub @Root) (!!!>- value) tree
				-- # over (sub @Left) continue tree
				-- # over (sub @Right) continue tree

---------------------------------- Prefixed non-empty binary tree ----------------------------------

instance Chain key => Morphable (Lookup Key) (Prefixed < Construction (Maybe <:*:> Maybe) < key) where
	type Morphing (Lookup Key) (Prefixed < Construction (Maybe <:*:> Maybe) < key) = (->) key <::> Maybe
	morphing (run . premorph -> Construct x xs) = TT <-- \key ->
		key <=> attached x & order
			<---- Just <-- extract x
			<---- lookup @Key key . TT @Covariant @Covariant ==<< get @(Obscure Lens) <-- sub @(Left Branch) <-- xs
			<---- lookup @Key key . TT @Covariant @Covariant ==<< get @(Obscure Lens) <-- sub @(Left Branch) <-- xs

-------------------------------------- Zipper of binary tree ---------------------------------------

instance Zippable Binary where
	type Breadcrumbs Binary = (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe)
		<:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)

instance Substructure Children (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Substance Children (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe)
	substructure = P_Q_T <-- \source -> case run @(->) <-|- run <-- lower source of
		focus :*: children :*: ancestors -> Store <--- children :*: lift . (focus <:*:>) . (<:*:> ancestors)

instance Substructure (Left Tree) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Substance (Left Tree) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	substructure = P_Q_T <-- \source -> case run @(->) <-|- run <-- lower source of
		focus :*: TT (T_U (left :*: right)) :*: ancestors ->
			Store <--- unite left :*: lift . (focus <:*:>) . (<:*:> ancestors) . TT . (<:*:> right) . run

instance Substructure (Right Tree) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Substance (Right Tree) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = Maybe <::> Construction (Maybe <:*:> Maybe)
	substructure = P_Q_T <-- \source -> case run @(->) <-|- run <-- lower source of
		focus :*: TT (T_U (left :*: right)) :*: ancestors ->
			Store <--- unite right :*: lift . (focus <:*:>) . (<:*:> ancestors) . TT . (left <:*:>) . run

instance Substructure Ancestors (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Substance Ancestors (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = List <::> Horizontal <::> (Exactly <:*:> Binary)
	substructure = P_Q_T <-- \source -> case run @(->) <-|- run <-- lower source of
		focus :*: children :*: ancestors -> Store <--- ancestors :*: lift . (focus <:*:>) . (children <:*:>)

instance Substructure Medium (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Substance Medium (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = Construction (Maybe <:*:> Maybe)
	substructure = P_Q_T <-- \source -> case run @(->) <-|- run <-- lower source of
		focus :*: children :*: ancestors -> Store
			<--- (Construct <-- extract focus <-- run children)
				:*: lift . T_U . ((<:*:> ancestors) <-|-) . run . reconstruct

instance Slidable (Down Left) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Sliding (Down Left) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = Maybe
	slide :: forall e . State > Zipper Binary e :> Maybe >>> ()
	slide = void . wrap . zoom @(Zipper Binary e) (sub @Medium) . change . constant
			=====<< lift . run =====<< wrap <--- zoom <-- sub @(Left Tree) <-- current @(Binary e)
		-*------ wrap . zoom (sub @Ancestors) . zoom primary . overlook . push @List
			-- TODO: Try to use Semimonoidal instance for lenses
			=====<< TT . Right .:.. (<:*:>)
				<-|--- wrap <--- zoom <-- sub @Root <-- current
				<-*--- wrap <--- zoom <-- sub @(Right Tree) <-- current

instance Slidable (Down Right) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Sliding (Down Right) (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = Maybe
	slide :: forall e . State > Zipper Binary e :> Maybe >>> ()
	slide = void . wrap . zoom @(Zipper Binary e) (sub @Medium) . change . constant
			=====<< lift . run =====<< wrap <--- zoom <-- sub @(Right Tree) <-- current @(Binary e)
		-*------ wrap . zoom (sub @Ancestors) . zoom primary . overlook . push @List
			-- TODO: Try to use Semimonoidal instance for lenses
			=====<< TT . Left .:.. (<:*:>)
				<-|--- wrap <--- zoom <-- sub @Root <-- current
				<-*--- wrap <--- zoom <-- sub @(Left Tree) <-- current

instance Slidable Up (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) where
	type Sliding Up (Exactly <:*:> (Maybe <:*:> Maybe) <::> Construction (Maybe <:*:> Maybe) <:*:> List <::> Horizontal <::> (Exactly <:*:> Binary)) = Maybe
	slide :: forall e . State > Zipper Binary e :> Maybe >>> ()
	slide = void . wrap . zoom @(Zipper Binary e) (sub @Medium) . change . branching
		=====<< lift . extract =====<< wrap <----- zoom @(Zipper Binary e) <---- sub @Ancestors
			<---- zoom <--- primary <--- overlook <-- pop @List where

		branching :: Horizontal <::> (Exactly <:*:> Binary) >>>>> e
			-> Nonempty Binary e -> Nonempty Binary e
		branching (TT (Left (T_U (Exactly root :*: left)))) right =
			Construct <----- root <----- run left <:*:> Just right
		branching (TT (Right (T_U (Exactly root :*: right)))) left =
			Construct <----- root <----- Just left <:*:> run right
