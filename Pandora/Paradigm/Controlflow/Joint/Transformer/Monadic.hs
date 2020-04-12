{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Joint.Transformer.Monadic (Monadic (..), (:>) (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)

class Interpreted t => Monadic t where
	{-# MINIMAL lay, wrap #-}
	lay :: Covariant u => u ~> t :> u
	wrap :: Pointable u => t ~> t :> u

infixr 3 :>
newtype (:>) t u a = TM { tm :: Schematic Monad t u a }

instance Covariant (Schematic Monad t u) => Covariant (t :> u) where
	f <$> TM x = TM $ f <$> x

instance Pointable (Schematic Monad t u) => Pointable (t :> u) where
	point = TM . point

instance Extractable (Schematic Monad t u) => Extractable (t :> u) where
	extract = extract . tm

instance Applicative (Schematic Monad t u) => Applicative (t :> u) where
	TM f <*> TM x = TM $ f <*> x

instance Alternative (Schematic Monad t u) => Alternative (t :> u) where
	TM x <+> TM y = TM $ x <+> y

instance Traversable (Schematic Monad t u) => Traversable (t :> u) where
	TM x ->> f = TM <$> x ->> f

instance Distributive (Schematic Monad t u) => Distributive (t :> u) where
	x >>- f = TM $ x >>- tm . f

instance Bindable (Schematic Monad t u) => Bindable (t :> u) where
	TM x >>= f = TM $ x >>= tm . f

instance Extendable (Schematic Monad t u) => Extendable (t :> u) where
	TM x =>> f = TM $ x =>> f . TM

instance (Pointable (t :> u), Bindable (t :> u)) => Monad (t :> u) where

instance (Interpreted (Schematic Monad t u)) => Interpreted (t :> u) where
	type Primary (t :> u) a = Primary (Schematic Monad t u) a
	run (TM x) = run x
