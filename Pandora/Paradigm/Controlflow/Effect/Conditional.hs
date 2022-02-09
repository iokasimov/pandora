{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Controlflow.Effect.Conditional where

import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))

class Conditional prompt clause where
	iff :: clause -> a -> a -> a

instance Conditional True Boolean where
	iff True x _ = x
	iff False _ y = y

instance Conditional False Boolean where
	iff False x _ = x
	iff True _ y = y

instance Conditional Just (Maybe a) where
	iff (Just _) x _ = x
	iff Nothing _ y = y

instance Conditional Nothing (Maybe a) where
	iff Nothing x _ = x
	iff (Just _) _ y = y
