module Pandora.Pattern.Functor.Extractable where

import Pandora.Core.Functor (type (<-|))
import Pandora.Pattern.Functor.Covariant (Covariant)

class Covariant t => Extractable t where
	{-# MINIMAL extract #-}
	extract :: a <-| t
