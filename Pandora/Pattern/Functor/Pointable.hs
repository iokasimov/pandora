module Pandora.Pattern.Functor.Pointable where

import Pandora.Pattern.Functor.Covariant (Covariant_)

class Covariant_ t source source => Pointable t source where
	{-# MINIMAL point #-}
	point :: source a (t a)

class Pointable_ t target where
	point_ :: target a (t a)
