{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Binary where

import Pandora.Core.Functor (type (~>), type (>), type (>>>>>>), type (<), type (:=>))
import Pandora.Core.Interpreted (run, (<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (-->), (--->))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|-|-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>), attached)
import Pandora.Paradigm.Algebraic.Exponential ((%), (&), (.:..))
import Pandora.Paradigm.Algebraic ((<-*-), (<-*--), (<-*-*-), extract, point, empty)
import Pandora.Paradigm.Primary.Auxiliary (Vertical (Up, Down), Horizontal (Leftward, Rightward))
import Pandora.Paradigm.Primary.Object.Ordering (order)
import Pandora.Paradigm.Primary.Functor (Comparison)
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Schemes (TT (TT), T_U (T_U), P_Q_T (P_Q_T), type (<::>), type (<:.:>))
import Pandora.Paradigm.Inventory.Ability.Gettable (get)
import Pandora.Paradigm.Inventory.Ability.Settable (set)
import Pandora.Paradigm.Inventory.Ability.Modifiable (modify)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Obscure, view, replace, mutate)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), morph, premorph
	, Morph (Rotate, Into, Insert, Lookup, Key), lookup)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure), Segment (Root, Branch), sub)
import Pandora.Paradigm.Structure.Interface.Zipper (Zippable (Breadcrumbs))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed)

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

instance Substructure (Left Branch) Binary where
	type Substance (Left Branch) Binary = Binary
	substructure = P_Q_T <-- \struct -> case run <-- lower struct of
		Nothing -> Store <--- empty :*: lift . constant empty
		Just tree -> lift . lift @(->) <-|- sub @(Left Branch) <~ tree

instance Substructure (Right Branch) Binary where
	type Substance (Right Branch) Binary = Binary
	substructure = P_Q_T <-- \struct -> case run . extract . run ---> struct of
		Nothing -> Store <--- empty :*: lift . constant empty
		Just tree -> lift . lift @(->) <-|- sub @(Right Branch) <~ tree

-------------------------------------- Non-empty binary tree ---------------------------------------

type instance Nonempty Binary = Construction (Maybe <:*:> Maybe)

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

-- instance Substructure Left (Construction (Maybe <:*:> Maybe)) where
-- 	type Substance Left (Construction (Maybe <:*:> Maybe)) = Binary
-- 	substructure = P_Q_T <-- \struct -> case extract ---> run struct of
-- 		Construct x (T_U (Nothing :*: Nothing)) -> Store <--- TT Nothing :*: lift . resolve (Construct x . left) (point x) . run
-- 		Construct x (T_U (Just lst :*: Nothing)) -> Store <--- TT (Just lst) :*: lift . Construct x . resolve left end . run
-- 		Construct x (T_U (Nothing :*: Just rst)) -> Store <--- TT Nothing :*: lift . Construct x . resolve (both % rst) (right rst) . run
-- 		Construct x (T_U (Just lst :*: Just rst)) -> Store <--- TT (Just lst) :*: lift . Construct x . resolve (both % rst) (right rst) . run
--
-- instance Substructure Right (Construction (Maybe <:*:> Maybe)) where
-- 	type Substance Right (Construction (Maybe <:*:> Maybe)) = Binary
-- 	substructure = P_Q_T <-- \struct -> case extract <-- run struct of
-- 		Construct x (T_U (Nothing :*: Nothing)) -> Store <--- TT Nothing :*: lift . resolve (Construct x . right) (point x) . run
-- 		Construct x (T_U (Just lst :*: Nothing)) -> Store <--- TT Nothing :*: lift . Construct x . resolve (both lst) (left lst) . run
-- 		Construct x (T_U (Nothing :*: Just rst)) -> Store <--- TT (Just rst) :*: lift . Construct x . resolve right end . run
-- 		Construct x (T_U (Just lst :*: Just rst)) -> Store <--- TT (Just rst) :*: lift . Construct x . resolve (both lst) (left lst) . run

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
			<---- lookup @Key key . TT @Covariant @Covariant ==<< get @(Obscure Lens) <-- sub @Left <-- xs
			<---- lookup @Key key . TT @Covariant @Covariant ==<< get @(Obscure Lens) <-- sub @Left <-- xs

-------------------------------------- Zipper of binary tree ---------------------------------------
	{-
data Biforked a = Top | Horizontal (Horizontal a)

instance Covariant (->) (->) Biforked where
	_ <-|- Top = Top
	f <-|- Horizontal (Leftward l) = Horizontal . Leftward <-- f l
	f <-|- Horizontal (Rightward r) = Horizontal . Rightward <-- f r

instance Traversable (->) (->) Biforked where
	_ <<- Top = point Top
	f <<- Horizontal (Leftward l) = Horizontal . Leftward <-|- f l
	f <<- Horizontal (Rightward r) = Horizontal . Rightward <-|- f r

type Bifurcation = Biforked <::> Construction Biforked

type Bicursor = Exactly <:.:> Binary > (:*:)

instance Zippable (Construction Wye) where
	type Breadcrumbs (Construction Wye) = (Wye <::> Construction Wye) <:.:> (Bifurcation <::> Bicursor) > (:*:)

_focused_part_to_nonempty_binary_tree :: (Exactly <:.:> Wye <::> Construction Wye > (:*:)) ~> Construction Wye
_focused_part_to_nonempty_binary_tree (T_U (Exactly x :*: xs)) = Construct x <-- run xs

instance Morphable (Rotate Up) ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> (Bifurcation <::> Bicursor) > (:*:)) where
	type Morphing (Rotate Up) ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> (Bifurcation <::> Bicursor) > (:*:))
		= Maybe <::> ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> Bifurcation <::> Bicursor > (:*:))
	morphing struct = case run ---> premorph struct of
		focused :*: TT (TT (Horizontal (Rightward (Construct (T_U (Exactly parent :*: rest)) next)))) ->
			lift . ((<:*:>) % TT (TT next)) . (<:*:>) (Exactly parent) . TT <---- resolve
				<--- Both <-- _focused_part_to_nonempty_binary_tree focused
				<--- Left <-- _focused_part_to_nonempty_binary_tree focused
				<--- run rest
		focused :*: TT (TT (Horizontal (Leftward (Construct (T_U (Exactly parent :*: rest)) next)))) ->
			lift . ((<:*:>) % TT (TT next)) . (<:*:>) (Exactly parent) . TT <---- resolve
				<--- Both % _focused_part_to_nonempty_binary_tree focused
				<--- Right <-- _focused_part_to_nonempty_binary_tree focused
				<--- run rest
		_ -> TT Nothing

_nonempty_binary_tree_to_focused_part :: Construction Wye ~> Exactly <:.:> Wye <::> Construction Wye > (:*:)
_nonempty_binary_tree_to_focused_part (Construct x xs) = Exactly x <:*:> TT xs

instance Morphable (Rotate > Down Left) ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> (Bifurcation <::> Bicursor) > (:*:)) where
	type Morphing (Rotate > Down Left) ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> (Bifurcation <::> Bicursor) > (:*:))
		= Maybe <::> ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> Bifurcation <::> Bicursor > (:*:))
	morphing struct = case run ---> premorph struct of
		T_U (Exactly x :*: TT (Left lst)) :*: TT (TT next) ->
			lift . (<:*:>) (_nonempty_binary_tree_to_focused_part lst)
				. TT . TT . Horizontal . Leftward <------- Construct
					<------ Exactly x <:*:> TT Nothing
					<------ next
		T_U (Exactly x :*: TT (Both lst rst)) :*: TT (TT next) ->
			lift . (<:*:>) (_nonempty_binary_tree_to_focused_part lst)
				. TT . TT . Horizontal . Leftward <------- Construct
					<------ Exactly x <:*:> lift rst
					<------ next
		_ -> TT Nothing

instance Morphable (Rotate > Down Right) ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> (Bifurcation <::> Bicursor) > (:*:)) where
	type Morphing (Rotate > Down Right) ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> (Bifurcation <::> Bicursor) > (:*:))
		= Maybe <::> ((Exactly <:.:> Wye <::> Construction Wye > (:*:)) <:.:> Bifurcation <::> Bicursor > (:*:))
	morphing struct = case run ---> premorph struct of
		T_U (Exactly x :*: TT (Right rst)) :*: TT (TT next) ->
			lift . (<:*:>) (_nonempty_binary_tree_to_focused_part rst)
				. TT . TT . Horizontal . Rightward <---- Construct (Exactly x <:*:> TT Nothing) next
		T_U (Exactly x :*: TT (Both lst rst)) :*: TT (TT next) ->
			lift . (<:*:>) (_nonempty_binary_tree_to_focused_part rst)
				. TT . TT . Horizontal . Rightward <---- Construct (Exactly x <:*:> lift lst) next
		_ -> TT Nothing
-}
