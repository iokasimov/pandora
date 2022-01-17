module Pandora.Pattern.Functor.Contravariant where

import Pandora.Pattern.Category (Category)
import Pandora.Pattern.Betwixt (Betwixt)

infixl 4 >-|-, >$<
infixl 3 >-|-|-, >$$<

{- |
> When providing a new instance, you should ensure it satisfies:
> * Exactly morphism: (identity >-|-) ≡ identity
> * Interpreted of morphisms: (f >-|-) . (g >-|-) ≡ (g . f >-|-)
-}

class (Category source, Category target) => Contravariant source target t where
	(>-|-) :: source a b -> target (t b) (t a)

	(>-|-|-) :: (Contravariant source (Betwixt source target) u, Contravariant (Betwixt source target) target t)
		=> source a b -> target (t (u a)) (t (u b))
	(>-|-|-) s = ((>-|-) ((>-|-) @source @(Betwixt source target) @_ s))

(>$<) :: Contravariant source target t => source a b -> target (t b) (t a)
(>$<) = (>-|-)

(>$$<) :: (Contravariant source target t, Contravariant source (Betwixt source target) u, Contravariant (Betwixt source target) target t) => source a b -> target (t (u a)) (t (u b))
(>$$<) = (>-|-|-)
