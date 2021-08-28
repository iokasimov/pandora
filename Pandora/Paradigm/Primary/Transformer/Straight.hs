module Pandora.Paradigm.Primary.Transformer.Straight where

newtype Straight (v :: * -> * -> *) a e = Straight (v e a)
