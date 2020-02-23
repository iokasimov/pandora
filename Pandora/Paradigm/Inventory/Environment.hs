module Pandora.Paradigm.Inventory.Environment (Environment (..), env, local) where

import Pandora.Core.Morphism (identity, (.), (!))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Divariant (($))

newtype Environment e a = Environment (e -> a)

environmentally :: e -> Environment e a -> a
environmentally e (Environment f) = f e

instance Covariant (Environment e) where
	f <$> Environment x = Environment $ f . x

instance Pointable (Environment e) where
	point x = Environment (x !)

instance Applicative (Environment e) where
	f <*> x = Environment $ \e -> environmentally e f $ environmentally e x

instance Bindable (Environment e) where
	Environment x >>= f = Environment $ \e -> environmentally e . f . x $ e

instance Monad (Environment e) where

env :: Environment e e
env = Environment identity

local :: (e -> e) -> Environment e a -> Environment e a
local f (Environment x) = Environment $ x . f
