module Pandora.Paradigm.Inventory.Optics (Lens, (|>), view, set, over, (^.), (.~), (%~)) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Identity (Identity)
import Pandora.Paradigm.Inventory.Storage (Storage (Storage), access, position, retrofit)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))

type Lens src tgt = src -> Storage tgt Identity src

-- | Lens composition infix operator
(|>) :: Lens src btw -> Lens btw tgt -> Lens src tgt
from |> to = \x -> ((<$) x . to) . position . from $ x

-- | Get the target of a lens
view :: Lens src tgt -> src -> tgt
view lens = position . lens

-- | Infix version of `view`
(^.) :: Lens src tgt -> src -> tgt
(^.) = view

-- | Replace the target of a lens
set :: Lens src tgt -> tgt -> src -> src
set lens new = access new . lens

-- | Infix version of `set`
(.~) :: Lens src tgt -> tgt -> src -> src
lens .~ new = set lens new

-- | Modify the target of a lens
over :: Lens src tgt -> (tgt -> tgt) -> src -> src
over lens f = extract . retrofit f . lens

-- | Infix version of `over`
(%~) :: Lens src tgt -> (tgt -> tgt) -> src -> src
lens %~ f = over lens f