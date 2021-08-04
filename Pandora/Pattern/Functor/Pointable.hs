module Pandora.Pattern.Functor.Pointable where

import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t source source => Pointable t source where
	{-# MINIMAL point #-}
	point :: source a (t a)
