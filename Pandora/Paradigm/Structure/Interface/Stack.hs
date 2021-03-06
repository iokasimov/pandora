module Pandora.Paradigm.Structure.Interface.Stack where

import Pandora.Core.Functor (type (~>), type (:=:=>))

class Stack t where
	push :: a :=:=> t
	pop :: t ~> t
