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
infixr 0 :~.
infixl 2 #=@

type Optics (mode :: * -> *) = PQ_ (->) (P_T Store mode)

type (:-.) src tgt = Lens src tgt

-- Reference to taret within some source
type Lens = Optics Identity

instance Category Lens where
	identity = PQ_ $ \src -> P_T . Store $ Identity src :*: identity . extract
	PQ_ to . PQ_ from = PQ_ $ \src -> P_T $ src <$ (run . to . extract @Identity . position . run $ from src)

-- Lens as natural transformation
type (:~.) src tgt = forall a . Lens (src a) (tgt a)

type (#=@) src tgt mode = forall a . Optics mode (src a) (tgt a)

-- | Get the target of a lens
view :: Optics mode src tgt -> src -> mode tgt
view lens = position . run . run lens

set :: Optics mode src tgt -> mode tgt -> src -> src
set lens new = look new . run . run lens

-- | Modify the target of a lens
over :: Covariant mode => Optics mode src tgt -> (mode tgt -> mode tgt) -> src -> src
over lens f = extract . retrofit f . run . run lens

-- | Representable based lens
represent :: (Representable t, Setoid (Representation t)) => Representation t -> t a :-. a
represent r = PQ_ $ \x -> P_T $ Store $ Identity (r <#> x) :*: \new -> tabulate (\r' -> r' == r ? extract new $ r' <#> x)
