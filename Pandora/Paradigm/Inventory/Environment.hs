module Pandora.Paradigm.Inventory.Environment (Environment (..), Environ, ask, local) where

import Pandora.Core.Composition ((:.:))
import Pandora.Core.Morphism ((.), ($), (!), flip)
import Pandora.Paradigm.Basis.Identity (Identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Liftable (Liftable (lift))

newtype Environment e t a = Environment { environmentally :: ((->) e :.: t) a }

type Environ e = Environment e Identity

instance Covariant t => Covariant (Environment e t) where
	f <$> Environment x = Environment $ comap f . x

instance Pointable t => Pointable (Environment e t) where
	point x = Environment $ (!) (point x)

instance Applicative t => Applicative (Environment e t) where
	f <*> x = Environment $ \e -> environmentally f e <*> environmentally x e

instance Alternative t => Alternative (Environment e t) where
	x <+> y = Environment $ \e -> environmentally x e <+> environmentally y e

instance Bindable t => Bindable (Environment e t) where
	Environment x >>= f = Environment $ \e -> x e >>= flip environmentally e . f

instance Monad t => Monad (Environment e t) where

instance Liftable (Environment e) where
	lift = Environment . (!)

ask :: Pointable t => Environment e t e
ask = Environment point

local :: (e -> e) -> Environment e t a -> Environment e t a
local f (Environment x) = Environment $ x . f
