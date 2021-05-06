{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Rose where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant (comap))
import Pandora.Pattern.Functor.Contravariant ((>$<))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Setoid (Setoid ((==), (!=)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), (?))
import Pandora.Paradigm.Primary.Functor.Function ((!), (%))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate), equate)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.PQ_ (PQ_ (PQ_))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Root))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Lookup, Element, Key), premorph, find)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed)
import Pandora.Paradigm.Structure.Some.List (List)

type Rose = Maybe <:.> Construction List

instance Focusable Root Rose where
	type Focusing Root Rose a = Maybe a
	focusing = PQ_ $ \x -> case run # extract x of
		Nothing -> Store $ Nothing :*: Tag . TU . comap (Construct % empty)
		Just rose -> Store $ Just (extract rose) :*: Tag . resolve (lift . Construct % deconstruct rose) empty

instance Nullable Rose where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Just Rose where
	type Substructural Just Rose = List <:.> Construction List
	substructure = PQ_ $ \rose -> case run . extract . run # rose of
		Nothing -> Store $ empty :*: (lift empty !)
		Just (Construct x xs) -> Store $ TU xs :*: lift . lift . Construct x . run

type instance Nonempty Rose = Construction List

instance Focusable Root (Construction List) where
	type Focusing Root (Construction List) a = a
	focusing = PQ_ $ \rose -> Store $ extract (extract rose) :*: Tag . Construct % deconstruct (extract rose)

instance Substructure Just (Construction List) where
	type Substructural Just (Construction List) = List <:.> Construction List
	substructure = PQ_ $ \rose -> case extract # run rose of
		Construct x xs -> Store $ TU xs :*: lift . Construct x . run

instance Setoid k => Morphable (Lookup Key) (Prefixed Rose k) where
	type Morphing (Lookup Key) (Prefixed Rose k) = (->) (Nonempty List k) <:.> Maybe
	morphing (run . premorph -> TU Nothing) = TU $ \_ -> Nothing
	morphing (run . premorph -> TU (Just tree)) = TU $ find_rose_sub_tree % tree

find_rose_sub_tree :: forall k a . Setoid k => Nonempty List k -> Nonempty Rose := k :*: a -> Maybe a
find_rose_sub_tree (Construct k Nothing) tree = k == attached (extract tree) ? Just (extract $ extract tree) $ Nothing
find_rose_sub_tree (Construct k (Just ks)) tree = k != attached (extract tree) ? Nothing $ subtree >>= find_rose_sub_tree ks where

	subtree :: Maybe :. Nonempty Rose := k :*: a
	subtree = find @Element # attached . extract >$< equate (extract ks) # deconstruct tree
