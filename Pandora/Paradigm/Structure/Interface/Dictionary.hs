module Pandora.Paradigm.Structure.Interface.Dictionary where

import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)

class Dictionary a k t where
	(?=) :: k -> t a -> Maybe a
