{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (..), (:>) (..)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
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

-- type family Schematic (c :: (* -> *) -> k) (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> c t u

class Interpreted t => Transformer t where
	{-# MINIMAL lay, wrap #-}

	lay :: Covariant u => u ~> t :> u
	wrap :: Pointable u => t ~> t :> u

infixr 3 :>
newtype (:>) t u a = T { trans :: Schematic Monad t u a }

instance Covariant (Schematic Monad t u) => Covariant (t :> u) where
	f <$> T x = T $ f <$> x

instance Pointable (Schematic Monad t u) => Pointable (t :> u) where
	point = T . point

instance Extractable (Schematic Monad t u) => Extractable (t :> u) where
	extract = extract . trans

instance Applicative (Schematic Monad t u) => Applicative (t :> u) where
	T f <*> T x = T $ f <*> x

instance Alternative (Schematic Monad t u) => Alternative (t :> u) where
	T x <+> T y = T $ x <+> y

instance Traversable (Schematic Monad t u) => Traversable (t :> u) where
	T x ->> f = T <$> x ->> f

instance Distributive (Schematic Monad t u) => Distributive (t :> u) where
	x >>- f = T $ x >>- trans . f

instance Bindable (Schematic Monad t u) => Bindable (t :> u) where
	T x >>= f = T $ x >>= trans . f

instance Extendable (Schematic Monad t u) => Extendable (t :> u) where
	T x =>> f = T $ x =>> f . T

instance (Interpreted (Schematic Monad t u), Transformer t) => Interpreted (t :> u) where
	type Primary (t :> u) a = Primary (Schematic Monad t u) a
	unwrap (T x) = unwrap x
