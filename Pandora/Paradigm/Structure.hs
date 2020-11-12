{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure (module Exports, find) where

import Pandora.Paradigm.Structure.Ability as Exports
import Pandora.Paradigm.Structure.Interface as Exports
import Pandora.Paradigm.Structure.Rose as Exports
import Pandora.Paradigm.Structure.Splay as Exports
import Pandora.Paradigm.Structure.Binary as Exports
import Pandora.Paradigm.Structure.Stack as Exports
import Pandora.Paradigm.Structure.Stream as Exports

import Pandora.Pattern (($), (.), extract)
import Pandora.Pattern.Functor ((<+>))
import Pandora.Paradigm.Inventory (Store (Store))
import Pandora.Paradigm.Primary.Functor (Maybe (Nothing), Predicate, satisfy, Product ((:*:)), Tagged (Tag), Wye (Left, Right))

instance Substructure Left (Product s) where
	type Substructural Left (Product s) a = s
	substructure (extract -> s :*: x) = Store $ s :*: Tag . (:*: x)

instance Substructure Right (Product s) where
	type Substructural Right (Product s) a = a
	substructure (extract -> s :*: x) = Store $ x :*: Tag . (s :*:)

find :: Monotonic e a => Predicate a -> e -> Maybe a
find p struct = iterate (\x r -> r <+> satisfy p x) Nothing struct
