{-# LANGUAGE EmptyCase #-}

module Pandora.Paradigm.Algebraic.Zero where

data Zero

absurd :: Zero -> a
absurd x = case x of {}
