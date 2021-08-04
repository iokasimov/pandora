{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary (module Exports) where

import Pandora.Paradigm.Primary.Linear as Exports
import Pandora.Paradigm.Primary.Transformer as Exports
import Pandora.Paradigm.Primary.Functor as Exports
import Pandora.Paradigm.Primary.Object as Exports
import Pandora.Paradigm.Primary.Algebraic as Exports

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (($), (#), identity))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((|-), (-|)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Schemes (TU (TU), P_Q_T (P_Q_T), type (<:.>), type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Into), premorph)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure))

instance Semigroupoid (Flip (->)) where
	Flip f . Flip g = Flip $ \x -> g (f x)

instance Category (Flip (->)) where
	identity = Flip identity

instance Extractable (Flip (:*:) a) (->) where
	extract (Flip (x :*: _)) = x

instance Adjoint (Flip (:*:) s) ((->) s) (->) (->) where
	f -| x = \s -> f $ Flip $ x :*: s
	f |- Flip (x :*: s) = f x s

instance Morphable (Into Maybe) (Conclusion e) where
	type Morphing (Into Maybe) (Conclusion e) = Maybe
	morphing = conclusion (Nothing !.) Just . premorph

instance Morphable (Into (Conclusion e)) Maybe where
	type Morphing (Into (Conclusion e)) Maybe = (->) e <:.> Conclusion e
	morphing (premorph -> Just x) = TU $ \_ -> Success x
	morphing (premorph -> Nothing) = TU $ \e -> Failure e

instance Morphable (Into (Flip Conclusion e)) Maybe where
	type Morphing (Into (Flip Conclusion e)) Maybe = (->) e <:.> Flip Conclusion e
	morphing (run . premorph -> Just x) = TU $ \_ -> Flip $ Failure x
	morphing (run . premorph -> Nothing) = TU $ Flip . Success

instance Morphable (Into (Left Maybe)) Wye where
	type Morphing (Into (Left Maybe)) Wye = Maybe
	morphing (premorph -> Both ls _) = Just ls
	morphing (premorph -> Left ls) = Just ls
	morphing (premorph -> Right _) = Nothing
	morphing (premorph -> End) = Nothing

instance Morphable (Into (Right Maybe)) Wye where
	type Morphing (Into (Right Maybe)) Wye = Maybe
	morphing (premorph -> Both _ rs) = Just rs
	morphing (premorph -> Left _) = Nothing
	morphing (premorph -> Right rs) = Just rs
	morphing (premorph -> End) = Nothing

instance Morphable (Into (This Maybe)) (These e) where
	type Morphing (Into (This Maybe)) (These e) = Maybe
	morphing (premorph -> This x) = Just x
	morphing (premorph -> That _) = Nothing
	morphing (premorph -> These _ x) = Just x

instance Morphable (Into (That Maybe)) (Flip These a) where
	type Morphing (Into (That Maybe)) (Flip These a) = Maybe
	morphing (run . premorph -> This _) = Nothing
	morphing (run . premorph -> That x) = Just x
	morphing (run . premorph -> These y _) = Just y

instance Morphable (Into (Here Maybe)) (Flip Wedge a) where
	type Morphing (Into (Here Maybe)) (Flip Wedge a) = Maybe
	morphing (run . premorph -> Nowhere) = Nothing
	morphing (run . premorph -> Here x) = Just x
	morphing (run . premorph -> There _) = Nothing

instance Morphable (Into (There Maybe)) (Wedge e) where
	type Morphing (Into (There Maybe)) (Wedge e) = Maybe
	morphing (premorph -> Nowhere) = Nothing
	morphing (premorph -> Here _) = Nothing
	morphing (premorph -> There x) = Just x

instance Morphable (Into Wye) (Maybe <:.:> Maybe := (:*:)) where
	type Morphing (Into Wye) (Maybe <:.:> Maybe := (:*:)) = Wye
	morphing (run . premorph -> Just x :*: Just y) = Both x y
	morphing (run . premorph -> Nothing :*: Just y) = Right y
	morphing (run . premorph -> Just x :*: Nothing) = Left x
	morphing (run . premorph -> Nothing :*: Nothing) = End

instance Substructure Left Wye where
	type Available Left Wye = Maybe
	type Substance Left Wye = Identity
	substructure = P_Q_T $ \new -> case lower new of
		End -> Store $ Nothing :*: lift . resolve Left End . (extract @_ @(->) -<$>-)
		Left x -> Store $ Just (Identity x) :*: lift . resolve Left End . (extract @_ @(->) -<$>-)
		Right y -> Store $ Nothing :*: (lift # Right y !.) . (extract @_ @(->) -<$>-)
		Both x y -> Store $ Just (Identity x) :*: lift . resolve (Both % y) (Right y) . (extract @_ @(->) -<$>-)

instance Substructure Right Wye where
	type Available Right Wye = Maybe
	type Substance Right Wye = Identity
	substructure = P_Q_T $ \new -> case lower new of
		End -> Store $ Nothing :*: lift . resolve Right End . (extract @_ @(->) -<$>-)
		Left x -> Store $ Nothing :*: (lift # Left x !.) . (extract @_ @(->) -<$>-)
		Right y -> Store $ Just (Identity y) :*: lift . resolve Right End . (extract @_ @(->) -<$>-)
		Both x y -> Store $ Just (Identity y) :*: lift . resolve (Both x) (Left x) . (extract @_ @(->) -<$>-)
