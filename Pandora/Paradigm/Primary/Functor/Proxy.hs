module Pandora.Paradigm.Primary.Functor.Proxy where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable_ ((-=<<-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Paradigm.Primary.Algebraic.Exponential ()

data Proxy a = Proxy

instance Covariant Proxy where
	_ <$> Proxy = Proxy

instance Covariant_ Proxy (->) (->) where
	_ -<$>- Proxy = Proxy

instance Contravariant Proxy where
	_ >$< _ = Proxy

instance Pointable Proxy (->) where
	point _ = Proxy

instance Applicative Proxy where
	_ <*> _ = Proxy

instance Alternative Proxy where
	_ <+> _ = Proxy

instance Distributive Proxy (->) (->) where
	_ -<< _ = Proxy

instance Bindable_ Proxy (->) where
	_ -=<<- _ = Proxy

instance Monad Proxy

instance Extendable Proxy where
	_ =>> _ = Proxy
