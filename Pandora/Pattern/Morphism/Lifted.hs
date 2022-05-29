module Pandora.Pattern.Morphism.Lifted where

newtype Lifted t (v :: * -> * -> *) source target = Lifted (v (t source) (t target))
