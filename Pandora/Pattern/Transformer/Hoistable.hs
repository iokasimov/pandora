module Pandora.Pattern.Transformer.Hoistable (Hoistable (..)) where

import Pandora.Core.Functor (type (~>))

class Hoistable t where
	hoist :: u ~> v -> t u ~> t v
