module Pandora.Pattern.Functor.Extractable where

import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t source source => Extractable t source where
	{-# MINIMAL extract #-}
	extract :: source (t a) a
