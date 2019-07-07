module Pandora.Paradigm.Inventory.Environmental (Environmental (..), env, local) where

import Pandora.Core.Morphism (identity, (.), ($), (!))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)

newtype Environmental e a = Environmental (e -> a)

environmentally :: e -> Environmental e a -> a
environmentally e (Environmental f) = f e

instance Covariant (Environmental e) where
	f <$> Environmental x = Environmental $ f . x

instance Pointable (Environmental e) where
	point x = Environmental $ (x !)

instance Applicative (Environmental e) where
	f <*> x = Environmental $ \e -> environmentally e f $ environmentally e x

instance Bindable (Environmental e) where
	Environmental x >>= f = Environmental $ \e -> environmentally e . f . x $ e

instance Monad (Environmental e) where

env :: Environmental e e
env = Environmental identity

local :: (e -> e) -> Environmental e a -> Environmental e a
local f (Environmental x) = Environmental $ x . f
