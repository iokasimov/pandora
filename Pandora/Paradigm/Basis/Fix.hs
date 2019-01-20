module Pandora.Paradigm.Basis.Fix (Fix (..)) where

newtype Fix t = Fix { unfix :: t (Fix t) }
