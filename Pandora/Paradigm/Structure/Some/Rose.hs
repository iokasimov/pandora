{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Rose where

import Pandora.Core.Functor (type (:.), type (>>>))
import Pandora.Core.Interpreted (run, unite, (<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), identity)
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant ((<-|-), (<-|--))
import Pandora.Pattern.Functor.Contravariant ((>-|-))
import Pandora.Pattern.Functor.Traversable ((<-/-))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Setoid (Setoid ((?=)))
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Paradigm.Algebraic.Exponential ((%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>), attached)
import Pandora.Paradigm.Algebraic.Functor (extract, point, empty)
import Pandora.Paradigm.Primary.Auxiliary (Vertical (Up), Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (equate)
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, reconstruct)
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse)
import Pandora.Paradigm.Schemes (TU (TU), TT (TT), T_U (T_U), P_Q_T (P_Q_T),  type (<::>), type (<:.>))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (view)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Into, Rotate, Lookup, Element, Key), premorph, find)
import Pandora.Paradigm.Structure.Modification.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure)
	, Segment (Root, Rest, Branch, Ancestors, Siblings, Children, Tree), Location (Focused), sub)
import Pandora.Paradigm.Structure.Interface.Zipper (Zippable (Breadcrumbs))
import Pandora.Paradigm.Structure.Interface.Stack (Stack (pop))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed)
import Pandora.Paradigm.Structure.Modification.Tape (Tape)
import Pandora.Paradigm.Structure.Some.List (List)

type Rose = Maybe <::> Construction List

--------------------------------------- Prefixed rose tree -----------------------------------------

instance Setoid k => Morphable (Lookup Key) (Prefixed Rose k) where
	type Morphing (Lookup Key) (Prefixed Rose k) = (->) (Nonempty List k) <:.> Maybe
	morphing prefixed_rose_tree = case run <-- premorph prefixed_rose_tree of
		TT Nothing -> TU <-- constant Nothing
		TT (Just tree) -> TU <-- find_rose_sub_tree % tree

-- TODO: Ineffiecient - we iterate over all branches in subtree, but we need to short-circuit on the first matching part of
--instance Setoid k => Morphable (Vary Element) (Prefixed Rose k) where
--	type Morphing (Vary Element) (Prefixed Rose k) = ((:*:) (Nonempty List k) <:.> Exactly) <:.:> Prefixed Rose k > (->)
--	morphing (run . run . premorph -> Nothing) = T_U ! \(TU (Construct key _ :*: Exactly value)) -> Prefixed . lift ! Construct (key :*: value) empty
--	morphing (run . run . premorph -> Just (Construct focused subtree)) = T_U ! \(TU (breadcrumbs :*: Exactly value)) -> case breadcrumbs of
--		Construct key Nothing -> Prefixed . lift ! attached focused == key ? Construct (key :*: value) subtree ! Construct focused subtree
--		Construct key (Just keys) -> Prefixed . lift ! attached focused != key ? Construct focused subtree
--			! Construct focused ! vary @Element @_ @_ @(Nonempty (Prefixed Rose k)) keys value -#=!> subtree

---------------------------------- Non-empty prefixed rose tree ------------------------------------

-- TODO: Ineffiecient - we iterate over all branches in subtree, but we need to short-circuit on the first matching part of
--instance Setoid k => Morphable (Vary Element) (Prefixed (Construction List) k) where
--	type Morphing (Vary Element) (Prefixed (Construction List) k) =
--		((:*:) (Nonempty List k) <:.> Exactly) <:.:> Prefixed (Construction List) k > (->)
--	morphing (run . premorph -> Construct x (TU Nothing)) = T_U ! \(TU (breadcrumbs :*: Exactly value)) -> case breadcrumbs of
--		Construct key Nothing -> Prefixed ! attached x == key ? Construct (key :*: value) empty ! Construct x empty
--		Construct _ (Just _) -> Prefixed ! Construct x (TU Nothing)
--	morphing (run . premorph -> Construct x (TU (Just subtree))) = T_U ! \(TU (breadcrumbs :*: Exactly value)) -> case breadcrumbs of
--		Construct key Nothing -> Prefixed ! attached x != key ? Construct x # lift subtree
--			! Construct (key :*: value) (lift subtree)
--		Construct key (Just keys) -> Prefixed ! attached x != key ? Construct x # lift subtree
--			! Construct (key :*: value) . lift ! vary @Element @_ @_ @(Nonempty (Prefixed Rose k)) keys value -#=!> subtree

find_rose_sub_tree :: forall k a . Setoid k => Nonempty List k -> Nonempty Rose >>> k :*: a -> Maybe a
find_rose_sub_tree (Construct k ks) tree = k ?= attached (extract tree)
	<----- case ks of
		Just keys -> find_rose_sub_tree keys =<< subtree keys
		Nothing -> Just <--- extract <-- extract tree
	<----- Nothing where

	subtree :: Nonempty List k -> Maybe :. Nonempty Rose >>> k :*: a
	subtree keys = find @Element
		<---- attached . extract
			>-|- equate <-- extract keys
		<---- deconstruct tree

------------------------------ Non-empty rose tree zipper -----------------------------

type Roses = List <::> Construction List

instance Zippable (Construction List) where
	type Breadcrumbs (Construction List) = Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)

-- TODO: Try to use substructure @Right . substructure @Right . substructure @Right . substructure @Right here
instance Substructure Ancestors (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) where
	type Substance Ancestors (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) = List <::> Tape Roses
	substructure = P_Q_T <-- \zipper -> case run <-- lower zipper of
		Exactly x :*: T_U (down :*: T_U (left :*: T_U (right :*: up))) ->
			Store <--- up :*: lift . (Exactly x <:*:>) . (down <:*:>) . (left <:*:>) . (right <:*:>)

-- TODO: Try to use substructure @Left . substructure @Right here
instance Substructure Children (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) where
	type Substance Children (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) = Roses
	substructure = P_Q_T <-- \zipper -> case run <-- lower zipper of
		Exactly x :*: T_U (down :*: rest) ->
			Store <--- down :*: lift . (Exactly x <:*:>) . (<:*:> rest)

instance Substructure (Focused Tree) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) where
	type Substance (Focused Tree) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) = Construction List
	substructure = P_Q_T <-- \zipper -> case run <-- lower zipper of
		Exactly x :*: T_U (children :*: rest) ->
			Store <--- Construct x (run children) :*: lift . T_U . ((<:*:> rest) <-|-) . run . reconstruct

-- TODO: Rename to Substructure (Left Siblings)
-- TODO: Try to use substructure @Left . substructure @Right . substructure @Right here
instance Substructure (Left Siblings) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) where
	type Substance (Left Siblings) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) = Reverse Roses
	substructure = P_Q_T <-- \zipper -> case run <-- lower zipper of
		Exactly x :*: T_U (down :*: T_U (left :*: rest)) ->
			Store <--- left :*: lift . (Exactly x <:*:>) . (down <:*:>) . (<:*:> rest)

-- TODO: Rename to Substructure (Right Siblings)
-- TODO: Try to use substructure  @Left . substructure @Right . substructure @Right . substructure @Right here
instance Substructure (Right Siblings) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) where
	type Substance (Right Siblings) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) = Roses
	substructure = P_Q_T <-- \zipper -> case run <-- lower zipper of
		Exactly x :*: T_U (down :*: T_U (left :*: T_U (right :*: rest))) ->
			Store <--- right :*: lift . (Exactly x <:*:>) . (down <:*:>) . (left <:*:>) . (<:*:> rest)

instance Morphable (Into (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) (Construction List) where
	type Morphing (Into (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) (Construction List) =
		Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)
	morphing nonempty_rose_tree = case premorph nonempty_rose_tree of
		Construct x xs -> Exactly x <:*:> unite xs <:*:> empty <:*:> empty <:*:> empty

instance Morphable (Rotate Up) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) where
	type Morphing (Rotate Up) (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)) =
		Maybe <::> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))
	morphing (premorph -> z) = TT <----- restruct <-|-- identity @(->) <-/- pop @List <~ run (view <-- sub @Ancestors <-- z) where

		-- TODO: Add type declaration
		restruct (parents :*: parent) =
			let child_node = extract <--- view <-- sub @Root <-- z in
			let central_children = run <--- view <-- sub @Children <-- z in
			let left_children = run @(->) <---- run <--- view <-- sub @(Left Siblings) <-- z in
			let right_children = run <--- view <-- sub @(Right Siblings) <-- z in
			view <-- sub @Root <-- parent
				<:*:> unite <-- left_children + point (Construct child_node central_children) + right_children
				<:*:> view <--- sub @(Left Branch) <--- view <-- sub @Rest <-- parent
				<:*:> view <--- sub @(Right Branch) <--- view <-- sub @Rest <-- parent
				<:*:> unite parents
