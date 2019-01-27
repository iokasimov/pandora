module Pandora.Paradigm.Inventory.Optics (Lens, view, set, over, (^.), (.~), (%~)) where

import Pandora.Core.Morphism ((.))
import Pandora.Paradigm.Inventory.Storage (Storage, position, access, retrofit)
import Pandora.Pattern.Functor.Extractable (Extractable (extract))

infixr 9 .~, ^., %~

type Lens t source target = source -> Storage target t source

-- | Get the target of a lens
view :: Lens t a p -> a -> p
view lens = position . lens

-- | Infix version of `view`
(^.) :: Lens t a p -> a -> p
(^.) = view

-- | Replace the target of a lens
set :: Extractable t => Lens t a p -> p -> a -> a
set lens new = access new . lens

-- | Infix version of `set`
(.~) :: Extractable t => Lens t a p -> p -> a -> a
lens .~ new = set lens new

-- | Modify the target of a lens
over :: Extractable t => Lens t a p -> (p -> p) -> a -> a
over lens f = extract . retrofit f . lens

-- | Infix version of `over`
(%~) :: Extractable t => Lens t a p -> (p -> p) -> a -> a
lens %~ f = over lens f
