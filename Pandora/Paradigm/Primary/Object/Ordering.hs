module Pandora.Paradigm.Primary.Object.Ordering where

data Ordering = Less | Equal | Greater

-- order :: a -> a -> a -> Ordering -> a
-- order x _ _ Less = x
-- order _ y _ Equal = y
-- order _ _ z Greater = z

order :: a -> a -> a -> Ordering -> a
order _ x _ Less = x
order y _ _ Equal = y
order _ _ z Greater = z
