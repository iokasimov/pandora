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
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes (TU (TU), T_ (T_), T_U (T_U), type (<:.>), type (<:.:>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (type (:-.), (|>), (%~))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Root))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
import Pandora.Paradigm.Structure.Ability.Insertable (Insertable (insert))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (Rotational, rotation))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure), sub)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)

type Binary = Maybe <:.> Construction Wye

rebalance :: Chain a => (Wye :. Construction Wye := a) -> Nonempty Binary a
rebalance (Both x y) = extract x <=> extract y & order
	(Construct (extract y) $ Both x (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) y)

instance (forall a . Chain a) => Insertable Binary where
	insert x (run -> Nothing) = lift . Construct x $ End
	insert x tree@(run -> Just nonempty) = x <=> extract nonempty & order
		(sub @Left %~ insert x $ tree) tree (sub @Right %~ insert x $ tree)

instance (forall a . Chain a) => Focusable Root Binary where
	type Focusing Root Binary a = Maybe a
	focusing (run . extract -> Nothing) = Store $ Nothing :*: Tag . TU . comap (Construct % End)
	focusing (run . extract -> Just x) = Store $ Just (extract x) :*: Tag . lift . resolve (Construct % deconstruct x) (rebalance $ deconstruct x)

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

instance (forall a . Chain a) => Insertable (Construction Wye) where
	insert x nonempty = let change = Just . resolve (insert x) (Construct x End) in
		x <=> extract nonempty & order (sub @Left %~ change $ nonempty) nonempty (sub @Right %~ change $ nonempty)

instance Substructure Left (Construction Wye) where
	type Substructural Left (Construction Wye) a = Maybe :. Construction Wye := a
	substructure empty_tree@(extract -> Construct _ End) = Store $ Nothing :*: (empty_tree !)
	substructure (extract -> Construct x (Left lst)) = Store $ Just lst :*: Tag . Construct x . resolve Left End
	substructure (extract -> Construct x (Right rst)) = Store $ Nothing :*: Tag . Construct x . resolve (Both % rst) (Right rst)
	substructure (extract -> Construct x (Both lst rst)) = Store $ Just lst :*: Tag . Construct x . resolve (Both % rst) (Right rst)

instance Substructure Right (Construction Wye) where
	type Substructural Right (Construction Wye) a = Maybe :. Construction Wye := a
	substructure emtpy_tree@(extract -> Construct _ End) = Store $ Nothing :*: (emtpy_tree !)
	substructure (extract -> Construct x (Left lst)) = Store $ Nothing :*: Tag . Construct x . resolve (Both lst) (Left lst)
	substructure (extract -> Construct x (Right rst)) = Store $ Just rst :*: Tag . Construct x . resolve Right End
	substructure (extract -> Construct x (Both lst rst)) = Store $ Just rst :*: Tag . Construct x . resolve (Both lst) (Left lst)

data Biforked a = Top | Leftward a | Rightward a

type instance Zipper (Construction Wye) = Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))

data Direction a = Up a | Down a

instance Rotatable Up (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) where
	type Rotational Up (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) a
		= Maybe :. (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) := a
	rotation (run . extract -> focused :*: TU (TU (Leftward (Construct (T_ (parent :*: TU (Just rst))) next)))) =
		Just . T_U $ Construct parent (Both focused rst) :*: TU (TU next)
	rotation (run . extract -> focused :*: TU (TU (Leftward (Construct (T_ (parent :*: TU Nothing)) next)))) =
		Just . T_U $ Construct parent (Left focused) :*: TU (TU next)
	rotation (run . extract -> focused :*: TU (TU (Rightward (Construct (T_ (parent :*: TU (Just lst))) next)))) =
		Just . T_U $ Construct parent (Both lst focused) :*: TU (TU next)
	rotation (run . extract -> focused :*: TU (TU (Rightward (Construct (T_ (parent :*: TU Nothing)) next)))) =
		Just . T_U $ Construct parent (Right focused) :*: TU (TU next)
	rotation (extract -> T_U (_ :*: TU (TU Top))) = Nothing

instance Rotatable (Down Left) (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) where
	type Rotational (Down Left) (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) a
		= Maybe :. (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) := a
	rotation (run . extract -> Construct x (Left lst) :*: TU (TU next)) = Just . T_U . (:*:) lst . TU . TU . Leftward . Construct (T_ $ x :*: TU Nothing) $ next
	rotation (run . extract -> Construct x (Both lst rst) :*: TU (TU next)) = Just . T_U . (:*:) lst . TU . TU . Leftward . Construct (T_ $ x :*: TU (Just rst)) $ next
	rotation (run . extract -> Construct _ (Right _) :*: _) = Nothing
	rotation (run . extract -> Construct _ End :*: _) = Nothing

instance Rotatable (Down Right) (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) where
	type Rotational (Down Right) (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) a
		= Maybe :. (Construction Wye <:.:> ((Biforked <:.> Construction Biforked) <:.> T_ Covariant (Maybe <:.> Construction Wye))) := a
	rotation (run . extract -> Construct x (Right rst) :*: TU (TU next)) = Just . T_U . (:*:) rst . TU . TU . Rightward . Construct (T_ $ x :*: TU Nothing) $ next
	rotation (run . extract -> Construct x (Both lst rst) :*: TU (TU next)) = Just . T_U . (:*:) rst . TU . TU . Rightward . Construct (T_ $ x :*: TU (Just lst)) $ next
	rotation (run . extract -> Construct _ (Left _) :*: _) = Nothing
	rotation (run . extract -> Construct _ End :*: _) = Nothing
