module Pandora.Paradigm.Primary.Functor.Fix where

import Pandora.Core.Functor (type (<:=), type (:=>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()

newtype Fix t = Fix { unfix :: t (Fix t) }

cata :: Covariant (->) (->) t => (a <:= t) -> Fix t -> a
cata f = f . (cata f <-|-) . unfix

ana :: Covariant (->) (->) t => (a :=> t) -> a -> Fix t
ana f = Fix . (ana f <-|-) . f

hylo :: Covariant (->) (->) t => (b <:= t) -> (a :=> t) -> (a -> b)
hylo phi psi = cata phi . ana psi
