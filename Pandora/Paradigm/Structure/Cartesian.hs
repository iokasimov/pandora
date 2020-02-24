module Pandora.Paradigm.Structure.Cartesian (Cartesian (..)) where

import Pandora.Paradigm.Basis.Product (type (:*:))

class Cartesian (t :: * -> *) where
	{-# MINIMAL (-:*:-) #-}
	(-:*:-) :: t a -> t b -> t (a :*: b)
