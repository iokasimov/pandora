{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Binary where

import Pandora.Core.Functor (type (~>), type (:=), type (:=>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (-->), (--->))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|-|-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((===<<)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%), (&))
import Pandora.Paradigm.Primary.Algebraic ((<-*-), (<-*--), (<-*-*-), extract, point)
import Pandora.Paradigm.Primary.Object.Ordering (order)
import Pandora.Paradigm.Primary.Functor (Comparison)
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Primary (twosome)
import Pandora.Paradigm.Schemes (TT (TT), T_U (T_U), P_Q_T (P_Q_T), type (<::>), type (<:.:>))
import Pandora.Paradigm.Controlflow.Effect.Conditional (iff)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Ability.Gettable (get)
import Pandora.Paradigm.Inventory.Ability.Settable (set)
import Pandora.Paradigm.Inventory.Ability.Modifiable (modify)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Obscure)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), morph, premorph
	, Morph (Rotate, Into, Insert, Lookup, Key), Vertical (Up, Down), lookup)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure), Segment (Root), sub)
import Pandora.Paradigm.Structure.Ability.Zipper (Zippable (Breadcrumbs))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed (Prefixed))

type Binary = Maybe <::> Construction Wye

instance {-# OVERLAPS #-} Traversable (->) (->) (Construction Wye) where
	f <<- Construct x (Left l) = Construct <-|-- f x <-*-- Left <-|- f <<- l
	f <<- Construct x (Right r) = Construct <-|-- f x <-*-- Right <-|- f <<- r
	f <<- Construct x (Both l r) = Construct <-|-- f x <-*-- Both <-|- f <<- l <-*- f <<- r
	f <<- Construct x End = Construct % End <-|- f x

--rebalance :: Chain a => (Wye :. Construction Wye := a) -> Nonempty Binary a
--rebalance (Both x y) = extract x <=> extract y & order
--	# Construct (extract x) (Both # rebalance (deconstruct x) # rebalance (deconstruct y))
--	# Construct (extract y) (Both # x # rebalance (deconstruct y))
--	# Construct (extract x) (Both # rebalance (deconstruct x) # y)

instance Morphable Insert Binary where
	type Morphing Insert Binary = (Exactly <:.:> Comparison := (:*:)) <:.:> Binary := (->)
	morphing struct = case run ---> premorph struct of
		Nothing -> T_U <-- \(T_U (Exactly x :*: _)) -> lift <-- leaf x
		Just binary -> T_U <-- \(T_U (Exactly x :*: Convergence f)) ->
			let continue xs = run <-- morph @Insert @(Nonempty Binary) xs <--- twosome <-- Exactly x <-- Convergence f in
			let step = iff @Just <-|-|- get @(Obscure Lens) <-*-*- modify @(Obscure Lens) continue <-*-*- set @(Obscure Lens) <-- leaf x in
			lift <---- order binary
				<--- step <-- sub @Left <-- binary
				<--- step <-- sub @Right <-- binary
				<--- f x <-- extract binary

instance Substructure Left Binary where
	type Available Left Binary = Maybe
	type Substance Left Binary = Construction Wye
	substructure = P_Q_T <-- \struct -> case run . lower ---> struct of
		Nothing -> Store <--- Nothing :*: lift . TT
		Just tree -> lift . lift @(->) <-|-- run <-- sub @Left <-- tree

instance Substructure Right Binary where
	type Available Right Binary = Maybe
	type Substance Right Binary = Construction Wye
	substructure = P_Q_T <-- \struct -> case run . extract . run ---> struct of
		Nothing -> Store <--- Nothing :*: lift . TT
		Just tree -> lift . lift @(->) <-|-- run <-- sub @Right <-- tree

-------------------------------------- Non-empty binary tree ---------------------------------------

type instance Nonempty Binary = Construction Wye

instance Morphable (Into Binary) (Construction Wye) where
	type Morphing (Into Binary) (Construction Wye) = Binary
	morphing = lift . premorph

instance Morphable Insert (Construction Wye) where
	type Morphing Insert (Construction Wye) = (Exactly <:.:> Comparison := (:*:)) <:.:> Construction Wye := (->)
	morphing (premorph -> struct) = T_U <-- \(T_U (Exactly x :*: Convergence f)) ->
		let continue xs = run <--- morph @Insert @(Nonempty Binary) xs <---- twosome <--- Exactly x <--- Convergence f in
		let step = iff @Just <-|-|- get @(Obscure Lens) <-*-*- modify @(Obscure Lens) continue <-*-*- set @(Obscure Lens) (leaf x) in
		order struct 
			<---- step <--- sub @Left <--- struct
			<---- step <--- sub @Right <--- struct
			<---- f x <--- extract struct

instance Substructure Root (Construction Wye) where
	type Available Root (Construction Wye) = Exactly
	type Substance Root (Construction Wye) = Exactly
	substructure = P_Q_T <-- \struct -> case lower struct of
		Construct x xs -> Store <--- (Exactly <-- Exactly x) :*: lift . (Construct % xs) . extract . extract

instance Substructure Left (Construction Wye) where
	type Available Left (Construction Wye) = Maybe
	type Substance Left (Construction Wye) = Construction Wye
	substructure = P_Q_T <-- \struct -> case extract ---> run struct of
		Construct x End -> Store <--- Nothing :*: lift . resolve (Construct x . Left) (leaf x)
		Construct x (Left lst) -> Store <--- Just lst :*: lift . Construct x . resolve Left End
		Construct x (Right rst) -> Store <--- Nothing :*: lift . Construct x . resolve (Both % rst) (Right rst)
		Construct x (Both lst rst) -> Store <--- Just lst :*: lift . Construct x . resolve (Both % rst) (Right rst)

instance Substructure Right (Construction Wye) where
	type Available Right (Construction Wye) = Maybe
	type Substance Right (Construction Wye) = Construction Wye
	substructure = P_Q_T <-- \struct -> case extract ---> run struct of
		Construct x End -> Store <--- Nothing :*: lift . resolve (Construct x . Right) (leaf x)
		Construct x (Left lst) -> Store <--- Nothing :*: lift . Construct x . resolve (Both lst) (Left lst)
		Construct x (Right rst) -> Store <--- Just rst :*: lift . Construct x . resolve Right End
		Construct x (Both lst rst) -> Store <--- Just rst :*: lift . Construct x . resolve (Both lst) (Left lst)

-------------------------------------- Prefixed binary tree ----------------------------------------

instance Chain k => Morphable (Lookup Key) (Prefixed Binary k) where
	type Morphing (Lookup Key) (Prefixed Binary k) = (->) k <::> Maybe
	morphing struct = case run . run . premorph <-- struct of
		Nothing -> TT <-- \_ -> Nothing
		Just tree -> TT <-- \key ->
			key <=> attached <-- extract tree & order
				<---- Just --> extract --> extract tree
				<---- lookup @Key key . Prefixed ===<< get @(Obscure Lens) <-- sub @Left <-- tree
				<---- lookup @Key key . Prefixed ===<< get @(Obscure Lens) <-- sub @Right <-- tree

-- instance Chain k => Morphable (Vary Element) (Prefixed Binary k) where
	-- type Morphing (Vary Element) (Prefixed Binary k) = ((:*:) k <::> Exactly) <:.:> Prefixed Binary k := (->)
	-- morphing struct = case run . run . premorph ! struct of
		-- Nothing -> T_U ! \(TT (key :*: Exactly value)) -> Prefixed . lift . leaf ! key :*: value
		-- Just tree -> T_U ! \(TT (key :*: Exactly value)) ->
			-- let continue = ((vary @Element @k @_ @(Prefixed Binary _) key value -#=) -#=) in
			-- Prefixed . lift ! key <=> attached (extract tree) & order
				-- # over (sub @Root) (!!!>- value) tree
				-- # over (sub @Left) continue tree
				-- # over (sub @Right) continue tree

---------------------------------- Prefixed non-empty binary tree ----------------------------------

instance Chain key => Morphable (Lookup Key) (Prefixed (Construction Wye) key) where
	type Morphing (Lookup Key) (Prefixed (Construction Wye) key) = (->) key <::> Maybe
	morphing (run . premorph -> Construct x xs) = TT <-- \key ->
		key <=> attached x & order
			<---- Just <-- extract x
			<---- lookup @Key key . Prefixed . extract ===<< get @(Obscure Lens) <-- sub @Left <-- xs
			<---- lookup @Key key . Prefixed . extract ===<< get @(Obscure Lens) <-- sub @Left <-- xs

-------------------------------------- Zipper of binary tree ---------------------------------------

data Biforked a = Top | Leftward a | Rightward a

instance Covariant (->) (->) Biforked where
	_ <-|- Top = Top
	f <-|- Leftward l = Leftward <-- f l
	f <-|- Rightward r = Rightward <-- f r

instance Traversable (->) (->) Biforked where
	_ <<- Top = point Top
	f <<- Leftward l = Leftward <-|- f l
	f <<- Rightward r = Rightward <-|- f r

type Bifurcation = Biforked <::> Construction Biforked

type Bicursor = Exactly <:.:> Binary := (:*:)

instance Zippable (Construction Wye) where
	type Breadcrumbs (Construction Wye) = (Wye <::> Construction Wye) <:.:> (Bifurcation <::> Bicursor) := (:*:)

_focused_part_to_nonempty_binary_tree :: (Exactly <:.:> Wye <::> Construction Wye := (:*:)) ~> Construction Wye
_focused_part_to_nonempty_binary_tree (T_U (Exactly x :*: xs)) = Construct x <-- run xs

instance Morphable (Rotate Up) ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> (Bifurcation <::> Bicursor) := (:*:)) where
	type Morphing (Rotate Up) ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> (Bifurcation <::> Bicursor) := (:*:))
		= Maybe <::> ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> Bifurcation <::> Bicursor := (:*:))
	morphing struct = case run ---> premorph struct of
		focused :*: TT (TT (Rightward (Construct (T_U (Exactly parent :*: rest)) next))) ->
			lift . (twosome % TT (TT next)) . twosome (Exactly parent) . TT <---- resolve
				<--- Both <-- _focused_part_to_nonempty_binary_tree focused
				<--- Left <-- _focused_part_to_nonempty_binary_tree focused
				<--- run rest
		focused :*: TT (TT (Leftward (Construct (T_U (Exactly parent :*: rest)) next))) ->
			lift . (twosome % TT (TT next)) . twosome (Exactly parent) . TT <---- resolve
				<--- Both % _focused_part_to_nonempty_binary_tree focused
				<--- Right <-- _focused_part_to_nonempty_binary_tree focused
				<--- run rest
		_ -> TT Nothing

_nonempty_binary_tree_to_focused_part :: Construction Wye ~> Exactly <:.:> Wye <::> Construction Wye := (:*:)
_nonempty_binary_tree_to_focused_part (Construct x xs) = twosome <--- Exactly x <--- TT xs

instance Morphable (Rotate (Down Left)) ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> (Bifurcation <::> Bicursor) := (:*:)) where
	type Morphing (Rotate (Down Left)) ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> (Bifurcation <::> Bicursor) := (:*:))
		= Maybe <::> ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> Bifurcation <::> Bicursor := (:*:))
	morphing struct = case run ---> premorph struct of
		T_U (Exactly x :*: TT (Left lst)) :*: TT (TT next) ->
			lift . twosome (_nonempty_binary_tree_to_focused_part lst)
				. TT . TT . Leftward <---- Construct 
					<--- twosome <-- Exactly x <-- TT Nothing 
					<--- next
		T_U (Exactly x :*: TT (Both lst rst)) :*: TT (TT next) ->
			lift . twosome (_nonempty_binary_tree_to_focused_part lst)
				. TT . TT . Leftward <---- Construct
					<--- twosome <-- Exactly x <-- lift rst 
					<--- next
		_ -> TT Nothing

instance Morphable (Rotate (Down Right)) ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> (Bifurcation <::> Bicursor) := (:*:)) where
	type Morphing (Rotate (Down Right)) ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> (Bifurcation <::> Bicursor) := (:*:))
		= Maybe <::> ((Exactly <:.:> Wye <::> Construction Wye := (:*:)) <:.:> Bifurcation <::> Bicursor := (:*:))
	morphing struct = case run ---> premorph struct of
		T_U (Exactly x :*: TT (Right rst)) :*: TT (TT next) ->
			lift . twosome (_nonempty_binary_tree_to_focused_part rst)
				. TT . TT . Rightward <---- Construct (twosome <--- Exactly x <--- TT Nothing) next
		T_U (Exactly x :*: TT (Both lst rst)) :*: TT (TT next) ->
			lift . twosome (_nonempty_binary_tree_to_focused_part rst)
				. TT . TT . Rightward <---- Construct (twosome <--- Exactly x <--- lift lst) next
		_ -> TT Nothing

leaf :: a :=> Nonempty Binary
leaf x = Construct x End
