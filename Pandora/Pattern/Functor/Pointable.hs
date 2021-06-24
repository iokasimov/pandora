module Pandora.Pattern.Functor.Pointable where

import Pandora.Core.Functor (type (:=>))
import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t => Pointable t where
	{-# MINIMAL point #-}
	point :: a :=> t

	pass :: t ()
	pass = point ()

class Pointable_ t target where
	point_ :: target a (t a)
