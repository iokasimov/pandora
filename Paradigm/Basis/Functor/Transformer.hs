module Paradigm.Basis.Functor.Transformer (T (..), type (:!:)) where

import Core.Composition ((:.:))
import Core.Morphism ((.), ($))
import Core.Variant (Variant (Co, Contra))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Bindable (Bindable ((>>=), bind))
import Pattern.Functor.Liftable (Liftable (lift))
import Pattern.Functor.Lowerable (Lowerable (lower))

newtype T t u a = T { t :: (u :.: t) a }

infixr 0 :!:
type (:!:) t u = T t u

instance (Covariant t, Covariant u) => Covariant (T t u) where
	f <$> T x = T $ (comap . comap) f x

instance (Pointable t, Pointable u) => Pointable (T t u) where
	point = T . point . point

instance Pointable t => Liftable (T t) where
	lift x = T $ point <$> x

instance Extractable t => Lowerable (T t) where
	lower (T x) = extract <$> x
