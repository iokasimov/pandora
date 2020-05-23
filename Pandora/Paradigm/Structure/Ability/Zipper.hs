module Pandora.Paradigm.Structure.Ability.Zipper (Zipper, Tap (..)) where

type family Zipper (s :: * -> *) = (r :: * -> *) | r -> s

data Tap t a = Tap a (t a)
