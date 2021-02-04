{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Optics where

import Pandora.Core.Functor (type (:=>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant ((<$))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Inventory.Store (Store (Store), position, look, retrofit)

infixr 0 :-.
infixr 0 :~.

type (:-.) src tgt = Lens src tgt

-- Reference to taret within some source
type Lens src tgt = src :=> Store tgt

-- Lens as natural transformation
type (:~.) src tgt = forall a . Lens (src a) (tgt a)

-- | Lens composition infix operator
(|>) :: Lens src old -> Lens old new -> Lens src new
from |> to = \x -> ((<$) x . to) . position . from $ x

-- | Get the target of a lens
view :: Lens src tgt -> src -> tgt
view lens = position . lens

-- | Replace the target of a lens
set :: Lens src tgt -> tgt -> src -> src
set lens new = look new . lens

-- | Modify the target of a lens
over :: Lens src tgt -> (tgt -> tgt) -> src -> src
over lens f = extract . retrofit f . lens

-- | Representable based lens
represent :: (Representable t, Setoid (Representation t)) => Representation t -> t a :-. a
represent r x = Store $ (r <#> x) :*: \new -> tabulate (\r' -> r' == r ? new $ r' <#> x)
