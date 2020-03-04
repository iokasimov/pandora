module Pandora.Paradigm.Controlflow.Joint.Transformer (module Exports, Transformer) where

import Pandora.Paradigm.Controlflow.Joint.Transformer.Comonadic as Exports
import Pandora.Paradigm.Controlflow.Joint.Transformer.Monadic as Exports

import Pandora.Pattern.Functor (Monad, Comonad)

type family Transformer c t where
	Transformer Monad t = Monadic t
	Transformer Comonad t = Comonadic t
