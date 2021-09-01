module Pandora.Pattern.Morphism (module Exports, Opposite) where

import Pandora.Pattern.Morphism.Flip as Exports
import Pandora.Pattern.Morphism.Straight as Exports

type family Opposite m where
	Opposite Straight = Flip
	Opposite Flip = Straight
