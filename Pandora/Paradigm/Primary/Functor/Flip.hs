module Pandora.Paradigm.Primary.Functor.Flip where

newtype Flip (t :: * -> * -> *) a e = Flip (t e a)
