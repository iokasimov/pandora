{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Binary (Binary, insert) where

import Pandora.Core.Functor (type (:.), type (:=), type (|->))
import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
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
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, sub))

type Binary = Maybe <:.> Construction Wye

insert :: Chain a => a -> Binary a -> Binary a
insert x (TU Nothing) = TU . Just . Construct x $ End
insert x tree@(TU (Just (Construct y _))) = x <=> y & order
	(sub @Left %~ (insert x <$>) $ tree) tree
	(sub @Right %~ (insert x <$>) $ tree)

rebalance :: Chain a => (Wye :. Construction Wye := a) -> Nonempty Binary a
rebalance (Both x y) = extract x <=> extract y & order
	(Construct (extract y) $ Both x (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) y)

zig :: forall a . Nonempty Binary a |-> Maybe
zig (Construct parent st) = Construct <$> found <*> subtree where

	found :: Maybe a
	found = extract <$> left st

	subtree :: Maybe :. Wye :. Nonempty Binary := a
	subtree = Both <$> a <*> (Construct parent <$> (Both <$> b <*> c))

	a, b, c :: Maybe :. Nonempty Binary := a
	a = deconstruct <$> left st >>= left
	b = deconstruct <$> left st >>= right
	c = right st

instance (forall a . Chain a) => Focusable Binary where
	type Focus Binary a = Maybe a
	top (TU Nothing) = Store . (:*:) Nothing $ TU . (<$>) (Construct % End)
	top (TU (Just x)) = Store . (:*:) (Just $ extract x) $ maybe
		(TU . Just . rebalance $ deconstruct x)
		(TU . Just . Construct % deconstruct x)
	singleton = TU . Just . Construct % End

instance Substructure Left Binary where
	type Substructural Left Binary a = Binary a
	sub (TU Nothing) = Store $ (:*:) (Tag $ TU Nothing) $ (TU Nothing !)
	sub t@(TU (Just (Construct x End))) = Store $ (:*:) (Tag $ TU Nothing) $
		maybe t (TU . Just . Construct x . Left) . run . extract
	sub (TU (Just (Construct x (Left lst)))) = Store $ (:*:) (Tag . TU . Just $ lst) $
		maybe (TU . Just . Construct x $ End) (TU . Just . Construct x . Left) . run . extract
	sub t@(TU (Just (Construct x (Right rst)))) = Store $ (:*:) (Tag $ TU Nothing) $
		maybe t (TU . Just . Construct x . Both % rst) . run . extract
	sub (TU (Just (Construct x (Both lst rst)))) = Store $ (:*:) (Tag . TU . Just $ lst) $
		maybe (TU (Just (Construct x (Right rst)))) (TU . Just . Construct x . Both % rst) . run . extract

instance Substructure Right Binary where
	type Substructural Right Binary a = Binary a
	sub (TU Nothing) = Store $ Tag (TU Nothing) :*: (TU Nothing !)
	sub t@(TU (Just (Construct x End))) = Store $ Tag (TU Nothing)
		:*: maybe t (TU . Just . Construct x . Right) . run . extract
	sub t@(TU (Just (Construct x (Left lst)))) = Store $ Tag (TU Nothing)
		:*: maybe t (TU . Just . Construct x . Both lst) . run . extract
	sub (TU (Just (Construct x (Right rst)))) = Store $ (Tag . TU . Just $ rst)
		:*: maybe (TU . Just . Construct x $ End) (TU . Just . Construct x . Right) . run . extract
	sub (TU (Just (Construct x (Both lst rst)))) = Store $ (Tag . TU . Just $ rst)
		:*: maybe (TU . Just . Construct x $ Left lst) (TU . Just . Construct x . Both lst) . run . extract

type instance Nonempty Binary = Construction Wye

instance Focusable (Construction Wye) where
	type Focus (Construction Wye) a = a
	top (Construct x xs) = Store $ x :*: Construct % xs
	singleton = Construct % End

instance Substructure Left (Construction Wye) where
	type Substructural Left (Construction Wye) a = Maybe :. Construction Wye := a
	sub (Construct x End) = Store $ Tag Nothing :*: (Construct x End !)
	sub (Construct x (Left lst)) = Store $ Tag (Just lst)
		:*: maybe (Construct x End) (Construct x . Left) . extract
	sub tree@(Construct x (Right rst)) = Store $ Tag Nothing
		:*: maybe tree (Construct x . Both % rst) . extract
	sub (Construct x (Both lst rst)) = Store $ Tag (Just lst)
		:*: maybe (Construct x $ Right rst) (Construct x . Both % rst) . extract

instance Substructure Right (Construction Wye) where
	type Substructural Right (Construction Wye) a = Maybe :. Construction Wye := a
	sub (Construct x End) = Store $ Tag Nothing :*: (Construct x End !)
	sub tree@(Construct x (Left lst)) = Store $ Tag Nothing
		:*: maybe tree (Construct x . Both lst) . extract
	sub (Construct x (Right rst)) = Store $ Tag (Just rst)
		:*: maybe (Construct x End) (Construct x . Right) . extract
	sub (Construct x (Both lst rst)) = Store $ Tag (Just rst)
		:*: maybe (Construct x $ Left lst) (Construct x . Both lst) . extract
