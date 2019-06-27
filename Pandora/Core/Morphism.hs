module Pandora.Core.Morphism (identity, fix, (.), ($), (&), (!), (?)) where

infixr 8 .
infixr 0 $
infixl 1 &
infixr 2 !
infixr 9 ?

{-# INLINE identity #-}
identity :: a -> a
identity x = x

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
x ! _ = x

{-# INLINE (?) #-}
(?) :: (a -> b -> c) -> b -> a -> c
(?) f x y = f y x
