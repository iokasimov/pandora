{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Rose where

import Pandora.Core.Functor (type (:.), type (>), type (>>>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), identity)
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant ((<-|-), (<-|-|-))
import Pandora.Pattern.Functor.Contravariant ((>-|-))
import Pandora.Pattern.Functor.Traversable ((<<-))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Transformer.Hoistable ((/|\))
import Pandora.Pattern.Object.Setoid (Setoid ((==), (!=), (?=)))
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Algebraic.Exponential ((%))
import Pandora.Paradigm.Algebraic (extract, empty, (>-||-))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>), attached)
import Pandora.Paradigm.Algebraic.Exponential ((%))
import Pandora.Paradigm.Algebraic (extract, point)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (equate)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse)
import Pandora.Paradigm.Schemes (TU (TU), TT (TT), T_U (T_U), P_Q_T (P_Q_T),  type (<::>), type (<:.>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite, (<~), (=#-), (<~~~~))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (view)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing)
	, Morph (Into, Rotate, Lookup, Element, Key), Vertical (Up, Down), premorph, find)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure)
	, Segment (Root, Rest, Forest), sub, tagstruct)
import Pandora.Paradigm.Structure.Interface.Zipper (Zipper, Zippable (Breadcrumbs))
import Pandora.Paradigm.Structure.Interface.Stack (Stack (pop, push))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed)
import Pandora.Paradigm.Structure.Modification.Tape (Tape)
import Pandora.Paradigm.Structure.Some.List (List)

type Rose = Maybe <:.> Construction List

--------------------------------------- Non-empty rose tree ----------------------------------------

type instance Nonempty Rose = Construction List

--------------------------------------- Prefixed rose tree -----------------------------------------

instance Setoid k => Morphable (Lookup Key) (Prefixed Rose k) where
	type Morphing (Lookup Key) (Prefixed Rose k) = (->) (Nonempty List k) <:.> Maybe
	morphing prefixed_rose_tree = case run <-- premorph prefixed_rose_tree of
		TU Nothing -> TU <-- constant Nothing
		TU (Just tree) -> TU <-- find_rose_sub_tree % tree

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
instance Substructure (Up Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) where
	type Substance (Up Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) = List <::> Tape Roses
	substructure = P_Q_T <-- \zipper -> case run . lower <-- lower zipper of
		Exactly x :*: T_U (down :*: T_U (left :*: T_U (right :*: up))) ->
			Store <--- up :*: lift . lift . (Exactly x <:*:>) . (down <:*:>) . (left <:*:>) . (right <:*:>)

-- TODO: Try to use substructure @Left . substructure @Right here
instance Substructure (Down Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) where
	type Substance (Down Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) = Roses
	substructure = P_Q_T <-- \zipper -> case run . lower <-- lower zipper of
		Exactly x :*: T_U (down :*: rest) ->
			Store <--- down :*: lift . lift . (Exactly x <:*:>) . (<:*:> rest)

-- TODO: Try to use substructure @Left . substructure @Right . substructure @Right here
instance Substructure (Left Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) where
	type Substance (Left Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) = Reverse Roses
	substructure = P_Q_T <-- \zipper -> case run . lower <-- lower zipper of
		Exactly x :*: T_U (down :*: T_U (left :*: rest)) ->
			Store <--- left :*: lift . lift . (Exactly x <:*:>) . (down <:*:>) . (<:*:> rest)

-- TODO: Try to use substructure  @Left . substructure @Right . substructure @Right . substructure @Right here
instance Substructure (Right Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) where
	type Substance (Right Forest) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) = Roses
	substructure = P_Q_T <-- \zipper -> case run . lower <-- lower zipper of
		Exactly x :*: T_U (down :*: T_U (left :*: T_U (right :*: rest))) ->
			Store <--- right :*: lift . lift . (Exactly x <:*:>) . (down <:*:>) . (left <:*:>) . (<:*:> rest)

instance Morphable (Into (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)))) (Construction List) where
	type Morphing (Into (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)))) (Construction List) =
		Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))
	morphing nonempty_rose_tree = case premorph nonempty_rose_tree of
		Construct x xs -> lift <----- Exactly x <:*:> unite xs <:*:> empty <:*:> empty <:*:> empty

instance Morphable (Rotate Up) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) where
	type Morphing (Rotate Up) (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses))) =
		Maybe <::> (Tagged (Zippable structure) <:.> (Exactly <:*:> Roses <:*:> Reverse Roses <:*:> Roses <:*:> (List <::> Tape Roses)))
	morphing (premorph -> z) = TT <---- restruct <-|- identity @(->) <<- pop @List <~ run (view <-- sub @(Up Forest) <-- z) where

		-- TODO: Add type declaration
		restruct (parents :*: parent) =
			let child_node = extract <--- view <-- sub @Root <-- z in
			let central_children = run <--- view <-- sub @(Down Forest) <-- z in
			let left_children = run @(->) <---- run <--- view <-- sub @(Left Forest) <-- z in
			let right_children = run <--- view <-- sub @(Right Forest) <-- z in
			lift <----- view <-- sub @Root <-- parent
				<:*:> unite <-- left_children + point (Construct child_node central_children) + right_children
				<:*:> view <-- sub @Left <-- parent
				<:*:> view <-- sub @Right <-- parent
				<:*:> unite parents
