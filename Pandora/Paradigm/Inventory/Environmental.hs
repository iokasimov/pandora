module Pandora.Paradigm.Inventory.Environmental (Environmental (..), Environ, ask, local) where

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

newtype Environmental e t a = Environmental { environmentally :: ((->) e :.: t) a }

type Environ e = Environmental e Identity

instance Covariant t => Covariant (Environmental e t) where
	f <$> Environmental x = Environmental $ comap f . x

instance Pointable t => Pointable (Environmental e t) where
	point x = Environmental $ (!) (point x)

instance Applicative t => Applicative (Environmental e t) where
	f <*> x = Environmental $ \e -> environmentally f e <*> environmentally x e

instance Alternative t => Alternative (Environmental e t) where
	x <+> y = Environmental $ \e -> environmentally x e <+> environmentally y e

instance Bindable t => Bindable (Environmental e t) where
	Environmental x >>= f = Environmental $ \e -> x e >>= flip environmentally e . f

instance Monad t => Monad (Environmental e t) where

instance Liftable (Environmental e) where
	lift = Environmental . (!)

ask :: Pointable t => Environmental e t e
ask = Environmental point

local :: (e -> e) -> Environmental e t a -> Environmental e t a
local f (Environmental x) = Environmental $ x . f
