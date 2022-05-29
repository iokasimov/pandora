{-# LANGUAGE EmptyCase #-}
module Pandora.Pattern.Operation.Zero where

data Zero

absurd :: Zero -> a
absurd x = case x of {}
