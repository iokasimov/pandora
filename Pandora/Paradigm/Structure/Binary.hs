{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Binary where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Primary.Object.Ordering (order)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing), maybe)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (type (:-.), (|>), (%~))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Root))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure), sub)

type Binary = Maybe <:.> Construction Wye

insert :: Chain a => a -> Binary a -> Binary a
insert x (run -> Nothing) = lift . Construct x $ End
insert x tree@(run -> Just nonempty) = x <=> extract nonempty & order
	(sub @Left %~ insert x $ tree) tree (sub @Right %~ insert x $ tree)

rebalance :: Chain a => (Wye :. Construction Wye := a) -> Nonempty Binary a
rebalance (Both x y) = extract x <=> extract y & order
	(Construct (extract y) $ Both x (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) y)

instance (forall a . Chain a) => Focusable Root Binary where
	type Focusing Root Binary a = Maybe a
	focusing (run . extract -> Nothing) = Store $ Nothing :*: Tag . TU . comap (Construct % End)
	focusing (run . extract -> Just x) = Store $ Just (extract x) :*: Tag . lift . maybe (rebalance $ deconstruct x) (Construct % deconstruct x)

instance Substructure Left Binary where
	type Substructural Left Binary a = Binary a
	substructure empty_tree@(run . extract -> Nothing) = Store $ extract empty_tree :*: (!) empty_tree
	substructure (run . extract -> Just tree) = Tag . lift <$> (sub @Left |> can_be_empty) tree

instance Substructure Right Binary where
	type Substructural Right Binary a = Binary a
	substructure empty_tree@(run . extract -> Nothing) = Store $ extract empty_tree :*: (!) empty_tree
	substructure (run . extract -> Just tree) = Tag . lift <$> (sub @Right |> can_be_empty) tree

can_be_empty :: Maybe (Construction Wye a) :-. Binary a
can_be_empty maybe_tree = Store $ TU maybe_tree :*: run

type instance Nonempty Binary = Construction Wye

instance Focusable Root (Construction Wye) where
	type Focusing Root (Construction Wye) a = a
	focusing (extract -> Construct x xs) = Store $ x :*: Tag . Construct % xs

instance Substructure Left (Construction Wye) where
	type Substructural Left (Construction Wye) a = Maybe :. Construction Wye := a
	substructure empty_tree@(extract -> Construct _ End) = Store $ Nothing :*: (!) empty_tree
	substructure (extract -> Construct x (Left lst)) = Store $ Just lst :*: Tag . Construct x . maybe End Left
	substructure (extract -> Construct x (Right rst)) = Store $ Nothing :*: Tag . Construct x . maybe (Right rst) (Both % rst)
	substructure (extract -> Construct x (Both lst rst)) = Store $ Just lst :*: Tag . Construct x . maybe (Right rst) (Both % rst)

instance Substructure Right (Construction Wye) where
	type Substructural Right (Construction Wye) a = Maybe :. Construction Wye := a
	substructure emtpy_tree@(extract -> Construct _ End) = Store $ Nothing :*: (!) emtpy_tree
	substructure (extract -> Construct x (Left lst)) = Store $ Nothing :*: Tag . Construct x . maybe (Left lst) (Both lst)
	substructure (extract -> Construct x (Right rst)) = Store $ Just rst :*: Tag . Construct x . maybe End Right
	substructure (extract -> Construct x (Both lst rst)) = Store $ Just rst :*: Tag . Construct x . maybe (Left lst) (Both lst)
