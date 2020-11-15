module Pandora.Paradigm.Structure.Interface.Set where

import Pandora.Core.Morphism ((!), (%))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Object.Setoid (Setoid ((/=)))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Nothing), maybe)
import Pandora.Paradigm.Primary.Functor.Predicate (equate)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Inventory.State (State)
import Pandora.Paradigm.Controlflow (run)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic, find)

member :: (Setoid a, Monotonic e a) => a -> e -> Boolean
member x = maybe False (True !) . find (equate x)

-- subset :: forall t a . (Traversable t, Setoid a, Setoid (t a)) => t a -> t a -> Boolean
-- subset ss s = let result = extract . run @(State (Maybe a)) % Nothing
-- 	in Nothing /= (ss ->> result . find % s . equate)
