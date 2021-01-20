{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure (module Exports) where

import Pandora.Paradigm.Structure.Ability as Exports
import Pandora.Paradigm.Structure.Interface as Exports
import Pandora.Paradigm.Structure.Rose as Exports
import Pandora.Paradigm.Structure.Splay as Exports
import Pandora.Paradigm.Structure.Binary as Exports
import Pandora.Paradigm.Structure.Stack as Exports
import Pandora.Paradigm.Structure.Stream as Exports

import Pandora.Pattern (($), (.), (+), extract)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Functor.Delta (Delta ((:^:)))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Both, Left, Right, End))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Schemes.TU (type (<:.>))

instance Monotonic s a => Monotonic s (s :*: a) where
	reduce f r x = reduce f (f (attached x) r) $ extract x

instance Nullable Maybe where
	null = Predicate $ \case { Just _ -> True ; _ -> False }

instance Substructure Left (Product s) a where
	type Substructural Left (Product s) a = s
	substructure (extract -> s :*: x) = Store $ s :*: Tag . (:*: x)

instance Substructure Right (Product s) a where
	type Substructural Right (Product s) a = a
	substructure (extract -> s :*: x) = Store $ x :*: Tag . (s :*:)

instance Substructure Left Delta a where
	type Substructural Left Delta a = a
	substructure (extract -> l :^: r) = Store $ l :*: Tag . (:^: r)

instance Substructure Right Delta a where
	type Substructural Right Delta a = a
	substructure (extract -> l :^: r) = Store $ r :*: Tag . (l :^:)

instance Substructure Left (Delta <:.> t) a where
	type Substructural Left (Delta <:.> t) a = t a
	substructure (run . extract -> l :^: r) = Store $ r :*: Tag . unite . (l :^:)

instance Substructure Right (Delta <:.> t) a where
	type Substructural Right (Delta <:.> t) a = t a
	substructure (run . extract -> l :^: r) = Store $ l :*: Tag . unite . (:^: r)

instance Substructure Tail (Tap t) a where
	type Substructural Tail (Tap t) a = t a
	substructure (extract -> Tap x xs) = Store $ xs :*: Tag . Tap x

-- TODO: define traversal order
instance Convertible Just (Construction Wye) (Construction Maybe) where
	conversion (extract -> Construct x End) = Construct x Nothing
	conversion (extract -> Construct x (Left lst)) = Construct x . Just . conversion $ Tag @Just lst
	conversion (extract -> Construct x (Right rst)) = Construct x . Just . conversion $ Tag @Just rst
	conversion (extract -> Construct x (Both lst rst)) = Construct x . Just
		$ conversion (Tag @Just lst) + conversion (Tag @Just rst)
