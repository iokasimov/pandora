module Paradigm.Fix (Fix (..)) where

newtype Fix t = Fix { unfix :: t (Fix t) }
