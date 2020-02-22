module Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (..), (:>) (..)) where

import Pandora.Core.Morphism ((.))
import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Divariant (($))

class Interpreted t => Transformer t where
	{-# MINIMAL lay, wrap #-}
	type Schema (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	lay :: Covariant u => u ~> t :> u
	wrap :: Pointable u => t ~> t :> u

infixr 3 :>
newtype (:>) t u a = T { trans :: Schema t u a }

instance Covariant (Schema t u) => Covariant (t :> u) where
	f <$> T x = T $ f <$> x

instance Pointable (Schema t u) => Pointable (t :> u) where
	point = T . point

instance Applicative (Schema t u) => Applicative (t :> u) where
	T f <*> T x = T $ f <*> x

instance Bindable (Schema t u) => Bindable (t :> u) where
	T x >>= f = T $ x >>= trans . f

instance (Interpreted (Schema t u), Transformer t) => Interpreted (t :> u) where
	type Primary (t :> u) a = Primary (Schema t u) a
	unwrap (T x) = unwrap x
