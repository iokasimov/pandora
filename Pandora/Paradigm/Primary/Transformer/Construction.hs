{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Construction where

import Pandora.Core.Functor (type (:.), type (:=), type (:=>), type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)), Covariant_ ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply_))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)), (-<<-<<-))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\), hoist))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid ((*))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Algebraic ((-<*>-))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))
import Pandora.Paradigm.Schemes (type (<:.>))

infixr 7 .-+

data Construction t a = Construct a (t :. Construction t := a)

instance Covariant t => Covariant (Construction t) where
	f <$> ~(Construct x xs) = Construct # f x # f <$$> xs

instance Covariant_ t (->) (->) => Covariant_ (Construction t) (->) (->) where
	f -<$>- ~(Construct x xs) = Construct # f x # f -<$$>- xs

instance Covariant_ t (->) (->) => Extractable (Construction t) (->) where
	extract ~(Construct x _) = x

instance (Covariant_ t (->) (->), Semimonoidal t (->) (:*:) (:*:)) => Semimonoidal (Construction t) (->) (:*:) (:*:) where
	multiply_ (Construct x xs :*: Construct y ys) = Construct (x :*: y) (multiply_ @_ @(->) @(:*:) -<$>- multiply_ (xs :*: ys))

instance Traversable t (->) (->) => Traversable (Construction t) (->) (->) where
	f <<- ~(Construct x xs) = Construct -<$>- f x -<*>- f -<<-<<- xs

instance Covariant_ t (->) (->) => Extendable (Construction t) (->) where
	f <<= x = Construct # f x # (f <<=) -<$>- deconstruct x

instance (Covariant t, Covariant_ t (->) (->)) => Comonad (Construction t) (->) where

instance Lowerable Construction where
	lower x = extract @_ @(->) -<$>- deconstruct x

instance Hoistable Construction where
	f /|\ x = Construct # extract x $ f # hoist f -<$>- deconstruct x

instance (Setoid a, forall b . Setoid b => Setoid (t b), Covariant t, Covariant_ t (->) (->)) => Setoid (Construction t a) where
	x == y = (extract x == extract y) * (deconstruct x == deconstruct y)

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b), Covariant t, Covariant_ t (->) (->)) => Semigroup (Construction t a) where
	x + y = Construct # extract x + extract y # deconstruct x + deconstruct y

instance (Monoid a, forall b . Semigroup b => Monoid (t b), Covariant t, Covariant_ t (->) (->)) => Monoid (Construction t a) where
	zero = Construct zero zero

instance Monotonic a (t :. Construction t := a) => Monotonic a (Construction t a) where
	reduce f r ~(Construct x xs) = f x $ reduce f r xs

instance Monotonic a (t :. Construction t := a) => Monotonic a (t <:.> Construction t := a) where
	reduce f r = reduce f r . run

deconstruct :: Construction t a -> t :. Construction t := a
deconstruct ~(Construct _ xs) = xs

-- Generate a construction from seed using effectful computation
(.-+) :: Covariant t => a :=> t -> a :=> Construction t
f .-+ x = Construct x $ (f .-+) <$> f x

section :: Comonad t (->) => t ~> Construction t
section xs = Construct # extract xs $ section <<= xs
