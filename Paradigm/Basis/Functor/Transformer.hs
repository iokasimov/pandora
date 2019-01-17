module Paradigm.Basis.Functor.Transformer (UT (..), type (:!:)) where

import Core.Morphism ((.), ($))
import Core.Variant (Variant (Co, Contra))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Bindable (Bindable ((>>=), bind))
import Pattern.Functor.Liftable (Liftable (lift))
import Pattern.Functor.Lowerable (Lowerable (lower))

newtype UT t u a = UT { ut :: u (t a) }

infixr 0 :!:
type (:!:) t u = UT t u

instance (Covariant t, Covariant u) => Covariant (UT t u) where
	f <$> UT x = UT $ (comap . comap) f x

instance (Pointable t, Pointable u) => Pointable (UT t u) where
	point = UT . point . point

instance Pointable t => Liftable (UT t) where
	lift x = UT $ point <$> x

instance Extractable t => Lowerable (UT t) where
	lower (UT x) = extract <$> x
