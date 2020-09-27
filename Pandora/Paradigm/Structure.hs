{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure (module Exports) where

import Pandora.Paradigm.Structure.Ability as Exports
import Pandora.Paradigm.Structure.Interface as Exports
import Pandora.Paradigm.Structure.Rose as Exports
import Pandora.Paradigm.Structure.Splay as Exports
import Pandora.Paradigm.Structure.Binary as Exports
import Pandora.Paradigm.Structure.Stack as Exports

import Pandora.Pattern (($), (.), extract)
import Pandora.Paradigm.Primary (Product ((:*:)), Tagged (Tag), Wye (Left, Right))
import Pandora.Paradigm.Inventory (Store (Store))

instance Substructure Left (Product s) where
	type Substructural Left (Product s) a = s
	substructure (extract -> s :*: x) = Store $ s :*: Tag . (:*: x)

instance Substructure Right (Product s) where
	type Substructural Right (Product s) a = a
	substructure (extract -> s :*: x) = Store $ x :*: Tag . (s :*:)
