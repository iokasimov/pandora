module Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (..), (:>)(..)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted)
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)

class Interpreted t => Transformer t where
	{-# MINIMAL lay, wrap #-}
	type Schema (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	lay :: Covariant u => u ~> t :> u
	wrap :: Pointable u => t ~> t :> u

infixr 3 :>
newtype (:>) t u a = T { trans :: Transformer t => Schema t u a }
