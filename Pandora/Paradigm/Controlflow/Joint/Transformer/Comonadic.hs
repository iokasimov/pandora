{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Joint.Transformer.Comonadic (Comonadic (..), (:<) (..)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted)
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Extractable (Extractable)
import Pandora.Pattern.Functor.Comonad (Comonad)

class Interpreted t => Comonadic t where
	{-# MINIMAL flick, bring #-}
	flick :: Covariant u => t :< u ~> u
	bring :: Extractable u => t :< u ~> t

infixr 3 :<
newtype (:<) t u a = TC { tc :: Schematic Comonad t u a }
