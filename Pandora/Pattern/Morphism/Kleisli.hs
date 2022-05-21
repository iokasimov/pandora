module Pandora.Pattern.Morphism.Kleisli where

newtype Kleisli t (v :: * -> * -> *) a e = Kleisli (v a (t e))
