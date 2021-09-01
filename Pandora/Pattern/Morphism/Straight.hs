module Pandora.Pattern.Morphism.Straight where

newtype Straight (v :: * -> * -> *) a e = Straight (v a e)
