module Pandora.Paradigm.Primary.Algebraic.Sum where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Interpreted ((!))

infixr 0 :+:

data (:+:) s a = Option s | Adoption a

instance Covariant (->) (->) ((:+:) s) where
	_ <-|- Option s = Option s
	f <-|- Adoption x = Adoption ! f x

instance Bivariant (->) (->) (->) (:+:) where
	f <-> g = \case
		Option s -> Option ! f s
		Adoption x -> Adoption ! g x

instance Covariant (->) (->) (Flip (:+:) a) where
	_ <-|- Flip (Adoption x) = Flip ! Adoption x
	f <-|- Flip (Option y) = Flip . Option ! f y

sum :: (e -> r) -> (a -> r) -> e :+: a -> r
sum f _ (Option x) = f x
sum _ s (Adoption x) = s x

-- TODO: keep it until we realize how to implement n-ary functors
bitraverse_sum :: Covariant (->) (->) t => (e -> t e') -> (a -> t a') -> (e :+: a) -> t (e' :+: a')
bitraverse_sum f _ (Option x) = Option <-|- f x
bitraverse_sum _ g (Adoption x) = Adoption <-|- g x
