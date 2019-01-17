module Paradigm.Basis.Functor.Transformer (Transformer (..), type (:!:)) where

import Core.Composition ((:.:))
import Core.Morphism ((.), ($))
import Core.Variant (Variant (Co, Contra))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Bindable (Bindable ((>>=), bind))
import Pattern.Functor.Liftable (Liftable (lift))
import Pattern.Functor.Lowerable (Lowerable (lower))

newtype Transformer t u a = Transformer { transformer :: (u :.: t) a }

infixr 0 :!:
type (:!:) t u = Transformer t u

instance (Covariant t, Covariant u) => Covariant (Transformer t u) where
	f <$> Transformer x = Transformer $ (comap . comap) f x

instance (Pointable t, Pointable u) => Pointable (Transformer t u) where
	point = Transformer . point . point

instance Pointable t => Liftable (Transformer t) where
	lift x = Transformer $ point <$> x

instance Extractable t => Lowerable (Transformer t) where
	lower (Transformer x) = extract <$> x
