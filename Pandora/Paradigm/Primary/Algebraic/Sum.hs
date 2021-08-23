module Pandora.Paradigm.Primary.Algebraic.Sum where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))

infixr 0 :+:

data (:+:) s a = Option s | Adoption a

instance Covariant (->) (->) ((:+:) s) where
	_ -<$>- Option s = Option s
	f -<$>- Adoption x = Adoption $ f x

instance Bivariant (->) (->) (->) (:+:) where
	f <-> g = \case
		Option s -> Option $ f s
		Adoption x -> Adoption $ g x

instance Covariant (->) (->) (Flip (:+:) a) where
	_ -<$>- Flip (Adoption x) = Flip $ Adoption x
	f -<$>- Flip (Option y) = Flip . Option $ f y

sum :: (e -> r) -> (a -> r) -> e :+: a -> r
sum f _ (Option x) = f x
sum _ s (Adoption x) = s x
