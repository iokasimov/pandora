module Pandora.Paradigm.Structure.Interface.Stack where

import Pandora.Core.Functor (type (~>))

class Stack t where
	push :: a -> t a -> t a
	pop :: t ~> t
