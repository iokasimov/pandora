module Pandora.Paradigm.Algebraic.Sum where

import Pandora.Core.Functor (type (>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Paradigm.Algebraic.Exponential ()
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Schemes.T_U (type (<:.:>), type (>:.:>), type (<:.:<), type (>:.:<))

infixr 7 :+:

data (:+:) o a = Option o | Adoption a

instance Covariant (->) (->) ((:+:) o) where
	_ <-|- Option s = Option s
	f <-|- Adoption x = Adoption <-- f x

instance Covariant (->) (->) (Flip (:+:) a) where
	_ <-|- Flip (Adoption x) = Flip . Adoption <-- x
	f <-|- Flip (Option y) = Flip . Option <-- f y

sum :: (e -> r) -> (a -> r) -> e :+: a -> r
sum f _ (Option x) = f x
sum _ s (Adoption x) = s x

-- TODO: keep it until we realize how to implement n-ary functors
bitraverse_sum :: Covariant (->) (->) t => (e -> t e') -> (a -> t a') -> (e :+: a) -> t (e' :+: a')
bitraverse_sum f _ (Option x) = Option <-|- f x
bitraverse_sum _ g (Adoption x) = Adoption <-|- g x

type (<:+:>) t u = t <:.:> u > (:+:)
type (>:+:>) t u = t >:.:> u > (:+:)
type (<:+:<) t u = t <:.:< u > (:+:)
type (>:+:<) t u = t >:.:< u > (:+:)
