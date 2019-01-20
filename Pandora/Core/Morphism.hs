module Pandora.Core.Morphism (identity, flip, fix, (.), ($), (&), (!)) where

infixr 9 .
infixr 0 $
infixl 1 &
infixr 2 !

{-# INLINE identity #-}
identity :: a -> a
identity x = x

{-# INLINE flip #-}
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

fix :: (a -> a) -> a
fix f = let x = f x in x

{-# INLINE (.) #-}
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

{-# INLINE ($) #-}
($) :: (a -> b) -> a -> b
f $ x = f x

{-# INLINE (&) #-}
(&) :: a -> (a -> b) -> b
x & f = f x

{-# INLINE (!) #-}
(!) :: a -> b -> a
x ! y = x
