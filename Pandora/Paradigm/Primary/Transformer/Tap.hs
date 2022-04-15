{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Primary.Transformer.Tap where

import Pandora.Core.Functor (type (>>>>>>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<------))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=), (<<==)))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Core.Interpreted ((<~), (<~~~))
import Pandora.Paradigm.Algebraic ((<-*-), (<-||--), extract)
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), (<:*:>))
import Pandora.Paradigm.Algebraic.Exponential (type (<--), type (-->))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

data Tap t a = Tap a (t a)

instance Covariant (->) (->) t => Covariant (->) (->) (Tap t) where
	f <-|- Tap x xs = Tap <---- f x <---- f <-|- xs

instance Semimonoidal (-->) (:*:) (:*:) t => Semimonoidal (-->) (:*:) (:*:) (Tap t) where
	mult = Straight <-- \(Tap x xs :*: Tap y ys) -> Tap <---- x :*: y <---- mult @(-->) <~~~ xs :*: ys

instance Semimonoidal (<--) (:*:) (:*:) t => Semimonoidal (<--) (:*:) (:*:) (Tap t) where
	mult = Flip <-- \(Tap (x :*: y) xys) -> Tap x <-||-- Tap y <-|- mult @(<--) <~ xys

instance Semimonoidal (<--) (:*:) (:*:) t => Monoidal (<--) (-->) (:*:) (:*:) (Tap t) where
	unit _ = Flip <-- \(Tap x _) -> Straight (\_ -> x)

instance Traversable (->) (->) t => Traversable (->) (->) (Tap t) where
	f <<- Tap x xs = Tap <-|- f x <-*- f <<- xs

instance (Semimonoidal (<--) (:*:) (:*:) t, Extendable (->) t, Covariant (->) (->) t) => Extendable (->) (Tap t) where
	f <<= x = Tap <--- f x <--- f . Tap (extract x) <<= lower x

instance Lowerable (->) Tap where
	lower (Tap _ xs) = xs

instance Hoistable (->) Tap where
	f /|\ Tap x xs = Tap x <-- f xs

instance {-# OVERLAPS #-} Semimonoidal (-->) (:*:) (:*:) t => Semimonoidal (-->) (:*:) (:*:) (Tap (t <:.:> t >>>>>> (:*:))) where
	mult = Straight <-- \(Tap x (T_U (xls :*: xrs)) :*: Tap y (T_U (yls :*: yrs))) ->
		Tap (x :*: y) <------ (mult @(-->) <~~~ xls :*: yls) <:*:> (mult @(-->) <~~~ xrs :*: yrs)
