{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite)
import Pandora.Paradigm.Inventory.Optics (type (:~.), (|>))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Schemes.TU (type (<:.>))

class Substructure f t where
	type Substructural (f :: k) (t :: * -> *) :: * -> *
	substructure :: Tagged f <:.> t :~. Substructural f t

	sub :: Covariant t => t :~. Substructural f t
	sub x = extract . run <$> substructure @f (lift x)

data Segment a = Tail a

data (|>) (i :: * -> k) (j :: * -> k') a

instance (Covariant t, Covariant (Substructural i t), Substructure i t, Substructure j (Substructural i t)) => Substructure (i |> j) t where
	type Substructural (i |> j) t = Substructural j (Substructural i t)
	substructure = extract . run >-> (unite . point <$>) $ sub @i |> sub @j
