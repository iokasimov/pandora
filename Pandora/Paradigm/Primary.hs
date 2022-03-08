{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Primary (module Exports, Simplification, twosome) where

import Pandora.Paradigm.Primary.Linear as Exports
import Pandora.Paradigm.Primary.Transformer as Exports
import Pandora.Paradigm.Primary.Functor as Exports
import Pandora.Paradigm.Primary.Object as Exports

import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Pattern.Kernel (Kernel (constant))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((|-), (-|)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Algebraic.Exponential (type (-->), type (<--), (&), (%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (<~))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Schemes (TU (TU), T_U (T_U), UT, TUT, P_Q_T (P_Q_T), type (<:.>), type (<:.:>))
-- import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
-- import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Into), premorph)
-- import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure))

instance Adjoint (->) (->) (Flip (:*:) s) ((->) s) where
	f -| x = \s -> f . Flip <--- x :*: s
	f |- Flip (x :*: s) = f x s

-- instance Morphable (Into Maybe) (Conclusion e) where
-- 	type Morphing (Into Maybe) (Conclusion e) = Maybe
-- 	morphing = conclusion (constant Nothing) Just . premorph

-- instance Morphable (Into (Conclusion e)) Maybe where
-- 	type Morphing (Into (Conclusion e)) Maybe = (->) e <:.> Conclusion e
-- 	morphing (premorph -> Just x) = TU <-- \_ -> Success x
-- 	morphing (premorph -> Nothing) = TU <-- \e -> Failure e

-- instance Morphable (Into (Flip Conclusion e)) Maybe where
-- 	type Morphing (Into (Flip Conclusion e)) Maybe = (->) e <:.> Flip Conclusion e
-- 	morphing (run . premorph -> Just x) = TU <-- \_ -> Flip <-- Failure x
-- 	morphing (run . premorph -> Nothing) = TU <-- Flip . Success

-- instance Morphable (Into (Left Maybe)) Wye where
-- 	type Morphing (Into (Left Maybe)) Wye = Maybe
-- 	morphing (premorph -> Both ls _) = Just ls
-- 	morphing (premorph -> Left ls) = Just ls
-- 	morphing (premorph -> Right _) = Nothing
-- 	morphing (premorph -> End) = Nothing
--
-- instance Morphable (Into (Right Maybe)) Wye where
-- 	type Morphing (Into (Right Maybe)) Wye = Maybe
-- 	morphing (premorph -> Both _ rs) = Just rs
-- 	morphing (premorph -> Left _) = Nothing
-- 	morphing (premorph -> Right rs) = Just rs
-- 	morphing (premorph -> End) = Nothing
--
-- instance Morphable (Into (This Maybe)) (These e) where
-- 	type Morphing (Into (This Maybe)) (These e) = Maybe
-- 	morphing (premorph -> This x) = Just x
-- 	morphing (premorph -> That _) = Nothing
-- 	morphing (premorph -> These _ x) = Just x
--
-- instance Morphable (Into (That Maybe)) (Flip These a) where
-- 	type Morphing (Into (That Maybe)) (Flip These a) = Maybe
-- 	morphing (run . premorph -> This _) = Nothing
-- 	morphing (run . premorph -> That x) = Just x
-- 	morphing (run . premorph -> These y _) = Just y
--
-- instance Morphable (Into (Here Maybe)) (Flip Wedge a) where
-- 	type Morphing (Into (Here Maybe)) (Flip Wedge a) = Maybe
-- 	morphing (run . premorph -> Nowhere) = Nothing
-- 	morphing (run . premorph -> Here x) = Just x
-- 	morphing (run . premorph -> There _) = Nothing
--
-- instance Morphable (Into (There Maybe)) (Wedge e) where
-- 	type Morphing (Into (There Maybe)) (Wedge e) = Maybe
-- 	morphing (premorph -> Nowhere) = Nothing
-- 	morphing (premorph -> Here _) = Nothing
-- 	morphing (premorph -> There x) = Just x
--
-- instance Morphable (Into Wye) (Maybe <:.:> Maybe > (:*:)) where
-- 	type Morphing (Into Wye) (Maybe <:.:> Maybe > (:*:)) = Wye
-- 	morphing (run . premorph -> Just x :*: Just y) = Both x y
-- 	morphing (run . premorph -> Nothing :*: Just y) = Right y
-- 	morphing (run . premorph -> Just x :*: Nothing) = Left x
-- 	morphing (run . premorph -> Nothing :*: Nothing) = End
--
-- instance Substructure Left Wye where
-- 	type Substance Left Wye = Maybe
-- 	substructure = P_Q_T <-- \new -> case lower new of
-- 		End -> Store <--- Nothing :*: lift . resolve Left End
-- 		Left x -> Store <--- Just x :*: lift . resolve Left End
-- 		Right y -> Store <--- Nothing :*: lift . constant (Right y)
-- 		Both x y -> Store <--- Just x :*: lift . resolve (Both % y) (Right y)

-- instance Substructure Right Wye where
-- 	type Substance Right Wye = Maybe
-- 	substructure = P_Q_T <-- \new -> case lower new of
-- 		End -> Store <--- Nothing :*: lift . resolve Right End
-- 		Left x -> Store <--- Nothing :*: lift . constant (Left x)
-- 		Right y -> Store <--- Just y :*: lift . resolve Right End
-- 		Both x y -> Store <--- Just y :*: lift . resolve (Both x) (Left x)

-- instance (Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) u) => Semimonoidal (-->) (:*:) (:*:) (t <:.:> u > (:*:)) where
-- 	mult = Straight <-- \(T_U (xls :*: xrs) :*: T_U (yls :*: yrs)) -> T_U <--- (mult @(-->) <~) (xls :*: yls) :*: (mult @(-->) <~) (xrs :*: yrs)

twosome :: t a -> u a -> (<:.:>) t u (:*:) a
twosome x y = T_U <--- x :*: y

type family Simplification (t :: * -> *) (a :: *) where
	Simplification Exactly a = a
	Simplification (TU _ _ t u) a = t :. u > a
	Simplification (UT _ _ t u) a = u :. t > a
	Simplification (TUT _ _ _ t t' u) a = t :. u :. t' > a
	Simplification (T_U _ _ p t u) a = p (t a) (u a)
	Simplification t a = t a
