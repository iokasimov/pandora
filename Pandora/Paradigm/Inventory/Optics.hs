{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Optics where

import Pandora.Pattern.Category (Category (identity, (.), ($)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (run))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Inventory.Store (Store (Store), position, look, retrofit)
import Pandora.Paradigm.Schemes.PQ_ (PQ_ (PQ_))
import Pandora.Paradigm.Schemes.P_T (P_T (P_T))

infixr 0 :-.
infixl 2 #=@

type Lens (available :: * -> *) = PQ_ (->) (P_T Store available)

type (:-.) source target = Lens Identity source target

instance Category (Lens Identity) where
	identity = PQ_ $ \source -> P_T . Store $ Identity source :*: identity . extract
	PQ_ to . PQ_ from = PQ_ $ \source -> P_T $ source <$ (run . to . extract @Identity . position . run $ from source)

-- Lens as natural transformation
type (#=@) source target available = forall a . Lens available (source a) (target a)

-- | Get focused target value
view :: Lens available source target -> source -> available target
view lens = position . run . run lens

-- Replace focused target value with new value
set :: Lens available source target -> available target -> source -> source
set lens new = look new . run . run lens

-- | Modify focused target value
over :: Covariant available => Lens available source target -> (available target -> available target) -> source -> source
over lens f = extract . retrofit f . run . run lens

-- | Representable based lens
represent :: (Representable t, Setoid (Representation t)) => Representation t -> Lens Identity (t a) a
represent r = PQ_ $ \x -> P_T $ Store $ Identity (r <#> x) :*: \new -> tabulate (\r' -> r' == r ? extract new $ r' <#> x)
