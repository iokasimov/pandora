module Pandora.Paradigm.Structure.Interface.Set where

import Pandora.Core.Morphism ((!))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Object.Setoid (Setoid ((==), (/=)))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Nothing), maybe)
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Inventory.State (find)

-- member :: (Traversable t, Setoid a) => a -> t a -> Boolean
-- member x = maybe False (True !) . find (Predicate (== x))
--
-- subset :: (Traversable t, Setoid a) => t a -> t a -> Boolean
-- subset ss s = Nothing /= (ss ->> \x -> find (Predicate (== x)) s)
