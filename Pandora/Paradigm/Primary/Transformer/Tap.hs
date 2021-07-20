{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Tap where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category ((.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((-<<--)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run) 
import Pandora.Paradigm.Primary.Algebraic ((-<*>-))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), twosome)
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Substructure
	(Substructure (Available, Substance, substructure), Segment (Root))

data Tap t a = Tap a (t a)

instance Covariant t => Covariant (Tap t) where
	f <$> Tap x xs = Tap # f x # f <$> xs

instance Covariant_ t (->) (->) => Covariant_ (Tap t) (->) (->) where
	f -<$>- Tap x xs = Tap # f x # f -<$>- xs

instance (Avoidable t, Covariant_ t (->) (->)) => Pointable (Tap t) (->) where
	point = Tap % empty

instance (Covariant t, Covariant_ t (->) (->))  => Extractable (Tap t) (->) where
	extract (Tap x _) = x

instance Applicative t => Applicative (Tap t) where
	Tap f fs <*> Tap x xs = Tap # f x # fs <*> xs

instance Traversable t (->) (->) => Traversable (Tap t) (->) (->) where
	f -<<-- Tap x xs = Tap -<$>- f x -<*>- f -<<-- xs

instance (Extractable t (->), Alternative t, Bindable t) => Bindable (Tap t) where
	Tap x xs >>= f = case f x of ~(Tap y ys) -> Tap y $ ys <+> (xs >>= lower . f)

instance (Extendable t, Covariant_ t (->) (->)) => Extendable (Tap t) where
	x =>> f = Tap # f x $ lower x =>> f . Tap (extract x)

instance Lowerable Tap where
	lower (Tap _ xs) = xs

instance Hoistable Tap where
	f /|\ Tap x xs = Tap x # f xs

instance {-# OVERLAPS #-} Applicative t => Applicative (Tap (t <:.:> t := (:*:))) where
	Tap f (T_U (lfs :*: rfs)) <*> Tap x (T_U (ls :*: rs)) = Tap # f x # T_U (lfs <*> ls :*: rfs <*> rs)

instance {-# OVERLAPS #-} Traversable t (->) (->) => Traversable (Tap (t <:.:> t := (:*:))) (->) (->) where
	f -<<-- Tap x (T_U (future :*: past)) = (\past' x' future' -> Tap x' $ twosome # future' # run past')
		-<$>- f -<<-- Reverse past -<*>- f x -<*>- f -<<-- future

instance (Covariant t, Covariant_ t (->) (->)) => Substructure Root (Tap (t <:.:> t := (:*:))) where
	type Available Root (Tap (t <:.:> t := (:*:))) = Identity
	type Substance Root (Tap (t <:.:> t := (:*:))) = Identity
	substructure = P_Q_T $ \zipper -> case lower zipper of
		Tap x xs -> Store $ Identity (Identity x) :*: lift . (Tap % xs) . extract . extract

instance (Covariant t, Covariant_ t (->) (->)) => Substructure Left (Tap (t <:.:> t := (:*:))) where
	type Available Left (Tap (t <:.:> t := (:*:))) = Identity
	type Substance Left (Tap (t <:.:> t := (:*:))) = t
	substructure = P_Q_T $ \zipper -> case lower zipper of
		Tap x (T_U (future :*: past)) -> Store $ Identity future :*: lift . Tap x . T_U . (:*: past) . extract

instance (Covariant t, Covariant_ t (->) (->)) => Substructure Right (Tap (t <:.:> t := (:*:))) where
	type Available Right (Tap (t <:.:> t := (:*:))) = Identity
	type Substance Right (Tap (t <:.:> t := (:*:))) = t
	substructure = P_Q_T $ \zipper -> case lower zipper of
		Tap x (T_U (future :*: past)) -> Store $ Identity past :*: lift . Tap x . T_U . (future :*:) . extract
