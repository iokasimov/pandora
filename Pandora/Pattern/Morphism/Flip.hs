module Pandora.Pattern.Morphism.Flip where

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)
