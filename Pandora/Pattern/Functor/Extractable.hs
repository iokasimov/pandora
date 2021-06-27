module Pandora.Pattern.Functor.Extractable where

import Pandora.Core.Functor (type (<:=))
import Pandora.Pattern.Functor.Covariant (Covariant_)

class Covariant_ t source source => Extractable t source where
	{-# MINIMAL extract #-}
	extract :: source (t a) a
