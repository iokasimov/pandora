{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.Rose where

import Pandora.Core.Functor (type (:.), type (>), type (<<), type (>>>))
import Pandora.Core.Interpreted (run, unite, (<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (<------), (<-------))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|----)))
import Pandora.Pattern.Functor.Contravariant ((>-|-))
import Pandora.Pattern.Functor.Traversable ((<-/-))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (====<<), (=====<<), (======<<)))
import Pandora.Pattern.Transformation.Liftable (lift)
import Pandora.Pattern.Transformation.Lowerable (lower)
import Pandora.Pattern.Object.Setoid (Setoid ((?=)))
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Pattern.Operation.Exponential ((%), (.:..))
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Algebraic (type (<:*:>), (<:*:>))
import Pandora.Paradigm.Algebraic.Functor ((<-*-), (<-*----), extract, point, empty, void)
import Pandora.Paradigm.Primary.Auxiliary (Vertical (Up, Down), Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (equate)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Schemes (TU (TU), TT (TT), T_U (T_U), P_Q_T (P_Q_T),  type (<::>), type (<:.>))
import Pandora.Paradigm.Controlflow.Effect.Transformer ((:>), wrap)
import Pandora.Paradigm.Inventory.Some.State (State, change, current)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, view, mutate, replace, primary)
import Pandora.Paradigm.Inventory (zoom, overlook, probably)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Lookup, Element, Key), premorph, find)
import Pandora.Paradigm.Structure.Modification.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Slidable (Slidable (Sliding, slide))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure)
	, Segment (Rest, Ancestors, Siblings, Children, Tree, Forest), Location (Focused), sub)
import Pandora.Paradigm.Structure.Interface.Zipper (Zipper, Zippable (Breadcrumbs, fasten, unfasten))
import Pandora.Paradigm.Structure.Interface.Stack (Stack (pop, push, top))
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

instance Zippable Rose where
	type Breadcrumbs Rose = Roses <:*:> List <::> Tape Roses
	fasten (TT (Just (Construct x xs))) = Just <----- Exactly x <:*:> TT xs <:*:> TT empty
	fasten (TT Nothing) = Nothing
	unfasten :: forall e . Zipper Rose e -> Nonempty Rose e
	unfasten (T_U (Exactly focus :*: T_U (TT children :*: TT ancestors))) =
		attached <-- (cover <-/- ancestors) <~ Construct focus children where

		cover :: Tape Roses e -> State << Nonempty Rose e << ()
		cover (T_U (Exactly x :*: T_U (Reverse (TT lf) :*: TT rf))) =

			void <--- change @(Nonempty Rose e) <-- \nrt ->
				Construct <------- x <------- lift <------ unfasten @List
					<----- Exactly nrt <:*:> Reverse lf <:*:> rf

-- TODO: Try to use substructure @Right . substructure @Right . substructure @Right . substructure @Right here
instance Substructure Ancestors (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Substance Ancestors (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = List <::> Tape Roses
	substructure = P_Q_T <-- \zipper -> case run @(->) <-|- run <-- lower zipper of
		Exactly x :*: down :*: up -> Store <--- up :*: lift . (Exactly x <:*:>) . (down <:*:>)

-- TODO: Try to use substructure @Left . substructure @Right here
instance Substructure Children (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Substance Children (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Roses
	substructure = P_Q_T <-- \zipper -> case run @(->) <-|- run <-- lower zipper of
		Exactly x :*: down :*: up -> Store <--- down :*: lift . (Exactly x <:*:>) . (<:*:> up)

-- Think about lens methods/operators that could make this code easier
instance Substructure Siblings (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Substance Siblings (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Maybe <::> (Reverse Roses <:*:> Roses)
	substructure = P_Q_T <-- \zipper -> case run @(->) <-|- run <-- lower zipper of
		Exactly x :*: down :*: up -> Store
			<--- (TT <---- view <-- sub @Rest <-|- view <-- top @List <-- run up)
				:*: \siblings -> lift <----- Exactly x <:*:> down
					<:*:> TT <--- mutate (replace <-|- run siblings <-*- Just <-- sub @Rest <-*-)
						<-- top @List <-- run up

instance Substructure (Focused Tree) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Substance (Focused Tree) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Construction List
	substructure = P_Q_T <-- \zipper -> case run @(->) <-|- run <-- lower zipper of
		Exactly x :*: children :*: up -> Store <--- Construct x (run children) :*: lift . T_U . ((<:*:> up) <-|-) . run . run

-- TODO: Refactor this instance, looks too complicated
-- TODO: it seem like this instance is wrong, when I try to update the focus, I lost andestors
instance Substructure (Focused Forest) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Substance (Focused Forest) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Tape List <::> Nonempty Rose
	substructure :: forall e . Lens (Tape List <::> Nonempty Rose)
		((Tagged (Focused Forest) <:.> (Exactly <:*:> Roses <:*:> List <::> Tape Roses)) > e) e
	substructure = P_Q_T <-- \zipper -> case run @(->) <-|- run @(->) <-- lower zipper of
		Exactly root :*: down :*: up -> Store <--- TT focused :*: lift . updated where

			focused :: Tape List > Nonempty Rose e
			focused = case run <--- view <-- top @List <-- run up of
				Nothing -> Exactly tree <:*:> Reverse empty <:*:> empty
				Just (T_U (Exactly _ :*: T_U (Reverse left :*: right)))
					-> Exactly tree <:*:> Reverse (run left) <:*:> run right

			tree :: Nonempty Rose e
			tree = Construct root <-- run down

			updated :: (Tape List <::> Nonempty Rose) e -> (Exactly <:*:> Roses <:*:> List <::> Tape Roses) e
			updated (TT (T_U (Exactly (Construct new_root new_down) :*: T_U (Reverse left_ :*: right_)))) =
				Exactly new_root <:*:> unite new_down <:*:> TT
					<--- mutate <-- (update_sides (Reverse (TT left_) <:*:> TT right_) <-|-) <-- top @List <-- run up

			update_sides :: (Reverse Roses <:*:> Roses) e -> Tape Roses e -> Tape Roses e
			update_sides sides tape = replace <-- sides <-- sub @Rest <-- tape

-- TODO: Try to use substructure @Left . substructure @Right . substructure @Right here
-- instance Substructure (Left Siblings) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
-- 	type Substance (Left Siblings) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Reverse Roses
-- 	substructure = P_Q_T <-- \zipper -> case run @(->) <-|- run <-- lower zipper of
-- 		Exactly x :*: down :*: up -> Store
-- 			<--- (view (sub @Left) . view (sub @Rest) <-- up)
-- 				:*: \left' -> lift (Exactly x <:*:> down <:*:> mutate (mutate (constant left') (sub @Left)) (sub @Rest) up)

-- TODO: Try to use substructure  @Left . substructure @Right . substructure @Right . substructure @Right here
-- instance Substructure (Right Siblings) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
-- 	type Substance (Right Siblings) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Roses
	--substructure = P_Q_T <-- \zipper -> case run @(->) <-|-|-|- run @(->) <-|-|- run @(->) <-|- run <-- lower zipper of
	--	Exactly x :*: down :*: left :*: right :*: rest -> Store <--- right :*: lift . (Exactly x <:*:>) . (down <:*:>) . (left <:*:>) . (<:*:> rest)

--instance Morphable (Into (Exactly <:*:> Roses <:*:> List <::> Tape Roses)) (Construction List) where
	--type Morphing (Into (Exactly <:*:> Roses <:*:> List <::> Tape Roses)) (Construction List) = Exactly <:*:> Roses <:*:> List <::> Tape Roses
	--morphing nonempty_rose_tree = case premorph nonempty_rose_tree of
		--Construct x xs -> Exactly x <:*:> unite xs <:*:> empty <:*:> empty <:*:> empty

--instance Morphable (Rotate Up) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	--type Morphing (Rotate Up) (Exactly <:*:> Roses <:*:> List <::> Tape Roses) =
		--Maybe <::> (Exactly <:*:> Roses <:*:> List <::> Tape Roses)
	--morphing (premorph -> z) = TT <----- restruct <-|-- identity @(->) <-/- pop @List <~ run (view <-- sub @Ancestors <-- z) where
--
		---- TODO: Add type declaration
		--restruct (parents :*: parent) =
			--let child_node = extract <--- view <-- sub @Root <-- z in
			--let central_children = run <--- view <-- sub @Children <-- z in
			----let left_children = run @(->) <---- run <--- view <-- sub @(Left Siblings) <-- z in
			--let right_children = run <--- view <-- sub @(Right Siblings) <-- z in
			--view <-- sub @Root <-- parent
				-- <:*:> unite <-- left_children + point (Construct child_node central_children) + right_children
				-- <:*:> view <--- sub @(Left Branch) <--- view <-- sub @Rest <-- parent
				-- <:*:> view <--- sub @(Right Branch) <--- view <-- sub @Rest <-- parent
				-- <:*:> unite parents

instance Slidable Up (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Sliding Up (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Maybe
	slide :: forall e . State > Zipper Rose e :> Maybe >>> ()
	slide = void . wrap . zoom @(Zipper Rose e) (sub @(Focused Tree)) . change . constant ======<< merging
		<-|---- wrap <--- zoom @(Zipper Rose e) <-- sub @(Focused Tree) <-- current
		<-*---- lift . extract ====<< wrap .:.. zoom @(Zipper Rose e) <--- sub @Ancestors
			<--- zoom <-- primary <-- overlook (pop @List) where

		merging :: Nonempty Rose e -> Tape (List <::> Nonempty Rose) e -> Nonempty Rose e
		merging x (T_U (Exactly p :*: T_U (Reverse ls :*: rs))) =
			-- TODO: This is wrong, we should add left part in reverse order
			Construct p <-- run ls + point x + run rs

instance Slidable Left (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Sliding Left (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Maybe
	slide :: forall e . State > Zipper Rose e :> Maybe >>> ()
	slide = void <------- lift . extract
		=====<< wrap .:.. zoom @(Zipper Rose e)
			<----- sub @(Focused Forest)
			<----- zoom <---- primary <---- overlook
				<--- probably <-- slide @Left

instance Slidable Right (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Sliding Right (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Maybe
	slide :: forall e . State > Zipper Rose e :> Maybe >>> ()
	slide = void <------- lift . extract
		=====<< wrap .:.. zoom @(Zipper Rose e)
			<----- sub @(Focused Forest)
			<----- zoom <---- primary <---- overlook
				<--- probably <-- slide @Right

instance Slidable Down (Exactly <:*:> Roses <:*:> List <::> Tape Roses) where
	type Sliding Down (Exactly <:*:> Roses <:*:> List <::> Tape Roses) = Maybe
	slide :: forall e . State > Zipper Rose e :> Maybe >>> ()
	slide = void . wrap . zoom @(Zipper Rose e) (sub @Ancestors) . zoom primary . overlook . push @List . ancestor
		====<< wrap . zoom @(Zipper Rose e) (sub @(Focused Tree)) . change . constant
		====<< lift . extract ====<< wrap .:.. zoom @(Zipper Rose e) <---- sub @(Focused Tree)
			<---- zoom <--- sub @Rest <--- zoom <-- primary <-- overlook (pop @List) where

		ancestor :: Construction List e -> Tape Roses e
		ancestor (Construct x xs) = Exactly x <:*:> Reverse <-- unite empty <:*:> unite xs
