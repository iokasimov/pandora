{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Optics (Lens, type (:-.), (|>), view, set, over, (^.), (.~), (%~)) where

import Pandora.Core.Morphism ((!), (%))
import Pandora.Paradigm.Basis.Product (Product ((:*:)))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (unwrap))
import Pandora.Paradigm.Inventory.State (State (State))
import Pandora.Paradigm.Inventory.Store (Store (Store), access, position, retrofit)
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Functor.Divariant (($))

instance Adjoint (Store s) (State s) where
	v -| f = State $ \s -> (:*:) s . f . Store $ s :*: (v !)
	Store (s :*: f) |- g = extract . unwrap % s . g $ f s

infixr 0 :-.
type (:-.) src tgt = Lens src tgt

type Lens src tgt = src -> Store tgt src

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
