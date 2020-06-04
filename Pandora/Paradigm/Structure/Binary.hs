{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Binary where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Primary.Object.Ordering (order)
import Pandora.Paradigm.Primary.Functor (left, right)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing), maybe)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics ((%~))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focus, top, singleton))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (rotation), rotate)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, sub), substructure)

type Binary = Maybe <:.> Construction Wye

insert :: Chain a => a -> Binary a -> Binary a
insert x (TU Nothing) = lift . Construct x $ End
insert x tree@(TU (Just (Construct y _))) = x <=> y & order
	(extract $ sub @Left %~ insert x $ Tag tree) tree
	(extract $ sub @Right %~ insert x $ Tag tree)

rebalance :: Chain a => (Wye :. Construction Wye := a) -> Nonempty Binary a
rebalance (Both x y) = extract x <=> extract y & order
	(Construct (extract y) $ Both x (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) y)

instance (forall a . Chain a) => Focusable Binary where
	type Focus Binary a = Maybe a
	top (TU Nothing) = Store . (:*:) Nothing $ TU . (<$>) (Construct % End)
	top (TU (Just x)) = Store $ Just (extract x) :*: lift . maybe (rebalance $ deconstruct x) (Construct % deconstruct x)
	singleton = lift . Construct % End

instance Substructure Left Binary where
	type Substructural Left Binary a = Binary a
	sub (run . extract -> Nothing) = Store $ TU Nothing :*: ((Tag $ TU Nothing) !)
	sub (run . extract -> Just (Construct x End)) = Store $ TU Nothing :*: Tag . lift . Construct x . maybe End Left . run
	sub (run . extract -> Just (Construct x (Left lst))) = Store $ lift lst :*: Tag . lift . Construct x . maybe End Left . run
	sub (run . extract -> Just (Construct x (Right rst))) = Store $ TU Nothing :*: Tag . lift . Construct x . maybe (Right rst) (Both % rst) . run
	sub (run . extract -> Just (Construct x (Both lst rst))) = Store $ lift lst :*: Tag . lift . Construct x . maybe (Right rst) (Both % rst) . run

instance Substructure Right Binary where
	type Substructural Right Binary a = Binary a
	sub (run . extract -> Nothing) = Store $ TU Nothing :*: ((Tag $ TU Nothing) !)
	sub (run . extract -> Just (Construct x End)) = Store $ TU Nothing :*: Tag . lift . Construct x . maybe End Right . run
	sub (run . extract -> Just (Construct x (Left lst))) = Store $ TU Nothing :*: Tag . lift . Construct x . maybe (Left lst) (Both lst) . run
	sub (run . extract -> Just (Construct x (Right rst))) = Store $ lift rst :*: Tag . lift . Construct x . maybe End Right . run
	sub (run . extract -> Just (Construct x (Both lst rst))) = Store $ lift rst :*: Tag . lift . Construct x . maybe (Left lst) (Both lst) . run

type instance Nonempty Binary = Construction Wye

instance Focusable (Construction Wye) where
	type Focus (Construction Wye) a = a
	top (Construct x xs) = Store $ x :*: Construct % xs
	singleton = Construct % End

instance Substructure Left (Construction Wye) where
	type Substructural Left (Construction Wye) a = Maybe :. Construction Wye := a
	sub (Tag (Construct x End)) = Store $ Nothing :*: ((Tag $ Construct x End) !)
	sub (Tag (Construct x (Left lst))) = Store $ Just lst :*: Tag . Construct x . maybe End Left
	sub tree@(Tag (Construct x (Right rst))) = Store $ Nothing :*: maybe tree (Tag . Construct x . Both % rst)
	sub (Tag (Construct x (Both lst rst))) = Store $ Just lst :*: Tag . Construct x . maybe (Right rst) (Both % rst)

instance Substructure Right (Construction Wye) where
	type Substructural Right (Construction Wye) a = Maybe :. Construction Wye := a
	sub (extract -> Construct x End) = Store $ Nothing :*: ((Tag $ Construct x End) !)
	sub (extract -> Construct x (Left lst)) = Store $ Nothing :*: Tag . Construct x . maybe (Left lst) (Both lst)
	sub (extract -> Construct x (Right rst)) = Store $ Just rst :*: Tag . Construct x . maybe End Right
	sub (extract -> Construct x (Both lst rst)) = Store $ Just rst :*: Tag . Construct x . maybe (Left lst) (Both lst)

data Splay a = Zig a | Zag a

instance Rotatable (Left Zig) (Construction Wye) where
	rotation (Tag (Construct parent st)) = Construct % subtree <$> found where

		subtree = maybe_subtree a . Just . Construct parent $ maybe_subtree b c
		found = extract <$> left st
		a = deconstruct <$> left st >>= left
		b = deconstruct <$> left st >>= right
		c = right st

instance Rotatable (Right Zig) (Construction Wye) where
	rotation (Tag (Construct parent st)) = Construct % subtree <$> found where

		found = extract <$> right st
		subtree = maybe_subtree a . Just . Construct parent $ maybe_subtree b c
		a = left st
		b = deconstruct <$> right st >>= left
		c = deconstruct <$> right st >>= right

instance Rotatable (Left (Zig Zig)) (Construction Wye) where
	rotation (Tag tree) = rotate @(Left Zig) tree >>= rotate @(Left Zig)

instance Rotatable (Right (Zig Zig)) (Construction Wye) where
	rotation (Tag tree) = rotate @(Right Zig) tree >>= rotate @(Right Zig)

instance Rotatable (Left (Zig Zag)) (Construction Wye) where
	rotation tree = extract tree
		& substructure @Left %~ (>>= rotate @(Right Zig))
		& rotate @(Left Zig)

instance Rotatable (Right (Zig Zag)) (Construction Wye) where
	rotation tree = extract tree
		& substructure @Right %~ (>>= rotate @(Left Zig))
		& rotate @(Right Zig)

maybe_subtree :: Maybe a -> Maybe a -> Wye a
maybe_subtree (Just x) (Just y) = Both x y
maybe_subtree Nothing (Just y) = Right y
maybe_subtree (Just x) Nothing = Left x
maybe_subtree Nothing Nothing = End
