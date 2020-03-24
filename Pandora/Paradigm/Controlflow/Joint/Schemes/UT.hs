module Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype UT ct cu t u a = UT (u :. t := a)

instance Interpreted (UT ct cu t u) where
	type Primary (UT ct cu t u) a = u :. t := a
	run (UT x) = x

instance Pointable t => Liftable (UT Covariant Covariant t) where
	lift x = UT $ point <$> x
