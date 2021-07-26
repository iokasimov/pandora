{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Rose where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant ((>$<))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Setoid (Setoid ((==), (!=)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), (?))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Primary.Algebraic.Exponential ((!.), (%))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate), equate)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes (TU (TU), T_U (T_U), P_Q_T (P_Q_T), type (<:.>), type (<:.:>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (=||$>))
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing)
	, Morph (Lookup, Vary, Element, Key), premorph, find, vary)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure), Segment (Root, Tail))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed (Prefixed))
import Pandora.Paradigm.Structure.Some.List (List)

type Rose = Maybe <:.> Construction List

instance Nullable Rose where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

-- FIXME: If we want to remove root node, we ruin the whole tree
instance Substructure Root Rose where
	type Available Root Rose = Maybe
	type Substance Root Rose = Identity
	substructure = P_Q_T $ \rose -> case run # lower rose of
		Nothing -> Store $ Nothing :*: TU . Tag . TU . ((Construct % empty) . extract <$>)
		Just nonempty_rose -> Store $ Just (Identity # extract nonempty_rose) :*: \case
			Just (Identity new) -> lift . TU . Just . Construct new $ deconstruct nonempty_rose
			Nothing -> lift empty

instance Substructure Just Rose where
	type Available Just Rose = Identity
	type Substance Just Rose = List <:.> Construction List
	substructure = P_Q_T $ \rose -> case run . extract . run # rose of
		Nothing -> Store $ Identity empty :*: (lift empty !.)
		Just (Construct x xs) -> Store $ Identity (TU xs) :*: lift . lift . Construct x . run . extract

--------------------------------------- Non-empty rose tree ----------------------------------------

type instance Nonempty Rose = Construction List

instance Substructure Root (Construction List) where
	type Available Root (Construction List) = Identity
	type Substance Root (Construction List) = Identity
	substructure = P_Q_T $ \rose -> Store $ Identity (Identity # extract (lower rose)) :*: lift . (Construct % deconstruct (lower rose)) . extract . extract

instance Substructure Tail (Construction List) where
	type Available Tail (Construction List) = Identity
	type Substance Tail (Construction List) = List <:.> Construction List
	substructure = P_Q_T $ \rose -> case extract # run rose of
		Construct x xs -> Store $ Identity (TU xs) :*: lift . Construct x . run . extract

--------------------------------------- Prefixed rose tree -----------------------------------------

instance Setoid k => Morphable (Lookup Key) (Prefixed Rose k) where
	type Morphing (Lookup Key) (Prefixed Rose k) = (->) (Nonempty List k) <:.> Maybe
	morphing (run . premorph -> TU Nothing) = TU $ \_ -> Nothing
	morphing (run . premorph -> TU (Just tree)) = TU $ find_rose_sub_tree % tree

-- TODO: Ineffiecient - we iterate over all branches in subtree, but we need to short-circuit on the first matching part of
instance Setoid k => Morphable (Vary Element) (Prefixed Rose k) where
	type Morphing (Vary Element) (Prefixed Rose k) = ((:*:) (Nonempty List k) <:.> Identity) <:.:> Prefixed Rose k := (->)
	morphing (run . run . premorph -> Nothing) = T_U $ \(TU (Construct key _ :*: Identity value)) -> Prefixed . lift $ Construct (key :*: value) empty
	morphing (run . run . premorph -> Just (Construct focused subtree)) = T_U $ \(TU (breadcrumbs :*: Identity value)) -> case breadcrumbs of
		Construct key Nothing -> Prefixed . lift $ attached focused == key ? Construct (key :*: value) subtree $ Construct focused subtree
		Construct key (Just keys) -> Prefixed . lift $ attached focused != key ? Construct focused subtree
			$ Construct focused $ vary @Element @_ @_ @(Nonempty (Prefixed Rose k)) keys value =||$> subtree

---------------------------------- Non-empty prefixed rose tree ------------------------------------

-- TODO: Ineffiecient - we iterate over all branches in subtree, but we need to short-circuit on the first matching part of
instance Setoid k => Morphable (Vary Element) (Prefixed (Construction List) k) where
	type Morphing (Vary Element) (Prefixed (Construction List) k) =
		((:*:) (Nonempty List k) <:.> Identity) <:.:> Prefixed (Construction List) k := (->)
	morphing (run . premorph -> Construct x (TU Nothing)) = T_U $ \(TU (breadcrumbs :*: Identity value)) -> case breadcrumbs of
		Construct key Nothing -> Prefixed $ attached x == key ? Construct (key :*: value) empty $ Construct x empty
		Construct _ (Just _) -> Prefixed $ Construct x (TU Nothing)
	morphing (run . premorph -> Construct x (TU (Just subtree))) = T_U $ \(TU (breadcrumbs :*: Identity value)) -> case breadcrumbs of
		Construct key Nothing -> Prefixed $ attached x != key ? Construct x # lift subtree
			$ Construct (key :*: value) (lift subtree)
		Construct key (Just keys) -> Prefixed $ attached x != key ? Construct x # lift subtree
			$ Construct (key :*: value) . lift $ vary @Element @_ @_ @(Nonempty (Prefixed Rose k)) keys value =||$> subtree

find_rose_sub_tree :: forall k a . Setoid k => Nonempty List k -> Nonempty Rose := k :*: a -> Maybe a
find_rose_sub_tree (Construct k Nothing) tree = k == attached (extract tree) ? Just (extract $ extract tree) $ Nothing
find_rose_sub_tree (Construct k (Just ks)) tree = k != attached (extract tree) ? Nothing $ find_rose_sub_tree ks =<< subtree where

	subtree :: Maybe :. Nonempty Rose := k :*: a
	subtree = find @Element # attached . extract >$< equate (extract ks) # deconstruct tree
