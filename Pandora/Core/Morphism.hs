module Pandora.Core.Morphism (fix, (&), (%)) where

infixl 1 &
infixr 9 %

fix :: (a -> a) -> a
fix f = let x = f x in x

{-# INLINE (&) #-}
(&) :: a -> (a -> b) -> b
x & f = f x

{-# INLINE (%) #-}
(%) :: (a -> b -> c) -> b -> a -> c
(%) f x y = f y x
