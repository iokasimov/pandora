module Pandora.Paradigm.Primary.Transformer.Flip where

newtype Flip (v :: * -> * -> *) a e = Flip (v e a)
