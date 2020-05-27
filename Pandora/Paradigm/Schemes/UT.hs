module Pandora.Paradigm.Schemes.UT where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype UT ct cu t u a = UT (u :. t := a)

type (<.:>) = UT Covariant Covariant
type (>.:>) = UT Contravariant Covariant
type (<.:<) = UT Covariant Contravariant
type (>.:<) = UT Contravariant Contravariant

instance Interpreted (UT ct cu t u) where
	type Primary (UT ct cu t u) a = u :. t := a
	run (UT x) = x

instance Pointable t => Liftable (UT Covariant Covariant t) where
	lift x = UT $ point <$> x

instance Extractable t => Lowerable (UT Covariant Covariant t) where
	lower (UT x) = extract <$> x
