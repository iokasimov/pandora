module Pandora.Core.Morphism (fix) where

fix :: (a -> a) -> a
fix f = let x = f x in x
