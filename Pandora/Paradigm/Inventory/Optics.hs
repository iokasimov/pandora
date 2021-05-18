{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Optics where

import Pandora.Pattern.Category (Category (identity, (.), ($)))
import Pandora.Pattern.Functor.Covariant ((<$>), (<$))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Pattern.Functor.Invariant (Invariant ((<$<)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip)
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Inventory.Store (Store (Store), position, look, retrofit)
import Pandora.Paradigm.Schemes.PQ_ (PQ_ (PQ_))
import Pandora.Paradigm.Schemes.P_T (P_T)

infixr 0 :-.
infixr 0 :~.

type (:-.) src tgt = Lens src tgt

-- Reference to taret within some source
type Lens = PQ_ (->) Store

instance Category Lens where
	identity = PQ_ $ \src -> Store $ src :*: identity
	PQ_ to . PQ_ from = PQ_ $ \src -> src <$ (to . position $ from src)

instance Invariant (Flip Lens tgt) where
	f <$< g = ((g >-> (f <$>) ||=) ||=)

-- Lens as natural transformation
type (:~.) src tgt = forall a . Lens (src a) (tgt a)

-- | Get the target of a lens
view :: Lens src tgt -> src -> tgt
view lens = position . run lens

-- | Replace the target of a lens
set :: Lens src tgt -> tgt -> src -> src
set lens new = look new . run lens

-- | Modify the target of a lens
over :: Lens src tgt -> (tgt -> tgt) -> src -> src
over lens f = extract . retrofit f . run lens

-- | Representable based lens
represent :: (Representable t, Setoid (Representation t)) => Representation t -> t a :-. a
represent r = PQ_ $ \x -> Store $ r <#> x :*: \new -> tabulate (\r' -> r' == r ? new $ r' <#> x)

type Prism = PQ_ (->) (P_T Store Maybe)

preview :: Prism src tgt -> src -> Maybe tgt
preview prism = position . run . run prism
