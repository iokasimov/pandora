{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Controlflow.Effect.Conditional where

import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))

infixr 1 ?

class Conditional clause where
	(?) :: clause -> a -> a -> a

instance Conditional Boolean where
	(?) True x _ = x
	(?) False _ y = y

instance Conditional (Maybe a) where
	(?) (Just _) x _ = x
	(?) Nothing _ y = y
