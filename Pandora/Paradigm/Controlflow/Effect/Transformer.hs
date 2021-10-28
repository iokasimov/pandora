module Pandora.Paradigm.Controlflow.Effect.Transformer (module Exports, Transformer) where

import Pandora.Paradigm.Controlflow.Effect.Transformer.Comonadic as Exports
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic as Exports

import Pandora.Pattern.Functor (Monad, Comonad)

type family Transformer c m t where
	Transformer Monad m t = Monadic m t
	Transformer Comonad m t = Comonadic m t
