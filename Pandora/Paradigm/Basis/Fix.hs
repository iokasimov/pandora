module Pandora.Paradigm.Basis.Fix (Fix (..), cata, ana, hylo) where

import Pandora.Core.Functor (type (<-|), type (|->))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant (comap))

newtype Fix t = Fix { unfix :: t (Fix t) }

cata :: Covariant t => (a <-| t) -> Fix t -> a
cata f = f . comap (cata f) . unfix

ana :: Covariant t => (a |-> t) -> a -> Fix t
ana f = Fix . comap (ana f) . f

hylo :: Covariant t => (b <-| t) -> (a |-> t) -> (a -> b)
hylo phi psi = cata phi . ana psi
