{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Primary.Transformer.Tap where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=), (<<==)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Controlflow.Effect.Interpreted ((!), (-#=))
import Pandora.Paradigm.Primary.Algebraic ((<-*-), extract)
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), type (-->), (%))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Substructure
	(Substructure (Available, Substance, substructure), Segment (Root))

data Tap t a = Tap a (t a)

instance Covariant (->) (->) t => Covariant (->) (->) (Tap t) where
	f <-|- Tap x xs = Tap <--- f x <--- f <-|- xs

instance Semimonoidal (-->) (:*:) (:*:) t => Semimonoidal (-->) (:*:) (:*:) (Tap t) where
	mult = Straight ! \(Tap x xs :*: Tap y ys) -> Tap <--- (x :*: y) <--- (mult @(-->) ! (xs :*: ys))

instance Semimonoidal (<--) (:*:) (:*:) t => Semimonoidal (<--) (:*:) (:*:) (Tap t) where
	mult = Flip ! \(Tap (x :*: y) xys) -> ((-#=) @(->) @(Flip _ _) (Tap x <-|-) . (Tap y <-|-)) (mult @(<--) ! xys)

instance Semimonoidal (<--) (:*:) (:*:) t => Monoidal (<--) (-->) (:*:) (:*:) (Tap t) where
	unit _ = Flip ! \(Tap x _) -> Straight (\_ -> x)

instance Traversable (->) (->) t => Traversable (->) (->) (Tap t) where
	f <<- Tap x xs = Tap <-|- f x <-*- f <<- xs

instance (Semimonoidal (<--) (:*:) (:*:) t, Extendable (->) t, Covariant (->) (->) t) => Extendable (->) (Tap t) where
	f <<= x = Tap <--- f x <--- f . Tap (extract x) <<== lower x

instance Lowerable (->) Tap where
	lower (Tap _ xs) = xs

instance Hoistable (->) Tap where
	f /|\ Tap x xs = Tap x <-- f xs

instance {-# OVERLAPS #-} Semimonoidal (-->) (:*:) (:*:) t => Semimonoidal (-->) (:*:) (:*:) (Tap (t <:.:> t := (:*:))) where
	mult = Straight ! \(Tap x (T_U (xls :*: xrs)) :*: Tap y (T_U (yls :*: yrs))) ->
		Tap (x :*: y) . T_U ! (mult @(-->) ! xls :*: yls) :*: (mult @(-->) ! xrs :*: yrs)

instance (Covariant (->) (->) t) => Substructure Root (Tap (t <:.:> t := (:*:))) where
	type Available Root (Tap (t <:.:> t := (:*:))) = Exactly
	type Substance Root (Tap (t <:.:> t := (:*:))) = Exactly
	substructure = P_Q_T ! \zipper -> case lower zipper of
		Tap x xs -> Store ! Exactly (Exactly x) :*: lift . (Tap % xs) . extract . extract

instance (Covariant (->) (->) t) => Substructure Left (Tap (t <:.:> t := (:*:))) where
	type Available Left (Tap (t <:.:> t := (:*:))) = Exactly
	type Substance Left (Tap (t <:.:> t := (:*:))) = t
	substructure = P_Q_T ! \zipper -> case lower zipper of
		Tap x (T_U (future :*: past)) -> Store ! Exactly future :*: lift . Tap x . T_U . (:*: past) . extract

instance (Covariant (->) (->) t) => Substructure Right (Tap (t <:.:> t := (:*:))) where
	type Available Right (Tap (t <:.:> t := (:*:))) = Exactly
	type Substance Right (Tap (t <:.:> t := (:*:))) = t
	substructure = P_Q_T ! \zipper -> case lower zipper of
		Tap x (T_U (future :*: past)) -> Store ! Exactly past :*: lift . Tap x . T_U . (future :*:) . extract
