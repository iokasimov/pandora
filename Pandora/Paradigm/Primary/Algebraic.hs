{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic (module Exports, Applicative, Alternative, Divisible, Decidable, Extractable, Pointable, (!>-), (!!>-), (!!!>-), (<-*-), (.-*-), (<-*-*-), (.-*-*-), forever_, (<-+-), (.-+-), void, empty, point, pass, extract, (<-|-<-|-), (<-|->-|-), (>-|-<-|-), (>-|->-|-)) where

import Pandora.Paradigm.Primary.Algebraic.Exponential as Exports
import Pandora.Paradigm.Primary.Algebraic.Product as Exports
import Pandora.Paradigm.Primary.Algebraic.Sum as Exports
import Pandora.Paradigm.Primary.Algebraic.Zero as Exports
import Pandora.Paradigm.Primary.Algebraic.One as Exports

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)), (<-|-|-), (<-|-|-|-))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit), Unit)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Functor.Proxy (Proxy (Proxy))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted ((!), (=||)))

type instance Unit (:*:) = One
type instance Unit (:+:) = Zero

infixl 4 <-*-, .-*-
infixl 4 <-*-*-, .-*-*-
infixl 3 <-+-, .-+-
infixl 0 <-|-<-|-, <-|->-|-, >-|-<-|-, >-|->-|-

(!>-) :: Covariant (->) (->) t => t a -> b -> t b
x !>- r = constant r <-|- x

(!!>-) :: (Covariant (->) (->) t, Covariant (->) (->) u) => t (u a) -> b -> t (u b)
x !!>- r = constant r <-|-|- x

(!!!>-) :: (Covariant (->) (->) t, Covariant (->) (->) u, Covariant (->) (->) v) => t (u (v a)) -> b -> t (u (v b))
x !!!>- r = constant r <-|-|-|- x

void :: Covariant (->) (->) t => t a -> t ()
void x = x !>- ()

instance (Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) u) => Semimonoidal (<--) (:*:) (:*:) (t <:.:> u := (:*:)) where
	mult = Flip ! \(T_U lrxys) ->
		-- TODO: I need matrix transposing here
		let ((lxs :*: lys) :*: (rxs :*: rys)) = ((mult @(<--) !) :*: (mult @(<--) !) <-|-<-|-) lrxys in
		T_U (lxs :*: rxs) :*: T_U (lys :*: rys)

instance Traversable (->) (->) ((:*:) s) where
	f <<- x = (attached x :*:) <-|- f (extract x)

instance Adjoint (->) (->) ((:*:) s) ((->) s) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f ! s :*: x
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

instance Semimonoidal (-->) (:*:) (:*:) ((->) e) where
	mult :: ((e -> a) :*: (e -> b)) --> (e -> (a :*: b))
	mult = Straight ! \(g :*: h) -> \x -> g x :*: h x

instance Monoidal (-->) (-->) (:*:) (:*:) ((->) e) where
	unit _ = Straight ! constant . (! One)

instance Semimonoidal (<--) (:*:) (:*:) ((->) e) where
	mult :: ((e -> a) :*: (e -> b)) <-- (e -> a :*: b)
	mult = Flip ! \f -> attached . f :*: extract . f

instance Semimonoidal (-->) (:*:) (:+:) ((:+:) e) where
	mult :: ((e :+: a) :*: (e :+: b)) --> (e :+: a :+: b)
	mult = Straight ! \case
		Option _ :*: Option e' -> Option e'
		Option _ :*: Adoption y -> Adoption ! Adoption y
		Adoption x :*: _ -> Adoption ! Option x

instance Semimonoidal (-->) (:*:) (:*:) ((:+:) e) where
	mult = Straight ! \case
		Adoption x :*: Adoption y -> Adoption ! x :*: y
		Option e :*: _ -> Option e
		_ :*: Option e -> Option e

instance Monoidal (-->) (-->) (:*:) (:*:) ((:+:) e) where
	unit _ = Straight ! Adoption . (! One)

instance Semimonoidal (<--) (:*:) (:*:) ((:*:) s) where
	mult = Flip ! \(s :*: x :*: y) -> (s :*: x) :*: (s :*: y)

instance Monoidal (<--) (-->) (:*:) (:*:) ((:*:) s) where
	unit _ = Flip ! \(_ :*: x) -> Straight (\_ -> x)

instance Comonad (->) ((:*:) s) where

instance Semimonoidal (<--) (:*:) (:*:) (Flip (:*:) a) where
	mult = Flip ! \(Flip ((sx :*: sy) :*: r)) -> Flip (sx :*: r) :*: Flip (sy :*: r)

instance Monoidal (<--) (-->) (:*:) (:*:) (Flip (:*:) a) where
	unit _ = Flip ! \(Flip (s :*: _)) -> Straight (\_ -> s)

--instance Semimonoidal (-->) (:*:) (:*:) (Flip (:*:) a) where
--mult = Straight ! \(Flip ((sx :*: sy) :*: r)) -> Flip (sx :*: r) :*: Flip (sy :*: r)

type Applicative t = (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t, Monoidal (-->) (-->) (:*:) (:*:) t)
type Alternative t = (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:+:) t, Monoidal (-->) (-->) (:*:) (:+:) t)
type Divisible t = (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t, Monoidal (-->) (<--) (:*:) (:*:) t)
type Decidable t = (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:+:) t, Monoidal (-->) (<--) (:*:) (:+:) t)

(<-*-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t (a -> b) -> t a -> t b
f <-*- x = (|-) @(->) @(->) (&) <-|- (mult @(-->) @_ @(:*:) ! (f :*: x))

(<-*-*-) :: (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) u) => t (u (a -> b)) -> t (u a) -> t (u b)
f <-*-*- x = (<-*-) <-|- f <-*- x

(.-*-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t b -> t a -> t b
y .-*- x = (\_ y' -> y') <-|- x <-*- y

(.-*-*-) :: (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) u) => t (u b) -> t (u a) -> t (u b)
y .-*-*- x = (\_ y' -> y') <-|-|- x <-*-*- y

forever_ :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t a -> t b
forever_ x = let r = r .-*- x in r

(<-+-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:+:) t) => t b -> t a -> (a :+: b -> r) -> t r
y <-+- x = \f -> f <-|- (mult @(-->) ! (x :*: y))

(.-+-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:+:) t) => t a -> t a -> t a
y .-+- x = (\r -> case r of Option rx -> rx; Adoption ry -> ry) <-|- (mult @(-->) ! (x :*: y))

type Extractable t = Monoidal (<--) (-->) (:*:) (:*:) t
type Pointable t = Monoidal (-->) (-->) (:*:) (:*:) t
type Emptiable t = Monoidal (-->) (-->) (:*:) (:+:) t

extract :: Extractable t => t a -> a
extract j = unit @(<--) @(-->) Proxy ! j ! One

point :: Pointable t => a -> t a
point x = unit @(-->) Proxy ! (Straight ! \One -> x)

pass :: Pointable t => t ()
pass = point ()

empty :: Emptiable t => t a
empty = unit @(-->) Proxy ! Straight absurd

--instance Appliable (->) b c (->) e d => Appliable (->) a (b -> c) (->) (a :*: e) d where
--	f ! (x :*: y) = f x ! y

(<-|-<-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Covariant m m (p a), Covariant m m (Flip p d), Interpreted m (Flip p d))
	=> m a b :*: m c d -> m (p a c) (p b d)
(<-|-<-|-) (f :*: g) = (=||) @m @(Flip p d) ((<-|-) f) . ((<-|-) g)

(<-|->-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Covariant m m (Flip p c), Contravariant m m (p a), Interpreted m (Flip p c))
	=> m a b :*: m c d -> m (p a d) (p b c)
(<-|->-|-) (f :*: g) = (=||) @m @(Flip p c) ((<-|-) f) . ((>-|-) g)

(>-|-<-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Contravariant m m (Flip p d), Covariant m m (p b), Interpreted m (Flip p d))
	=> m a b :*: m c d -> m (p b c) (p a d)
(>-|-<-|-) (f :*: g) = (=||) @m @(Flip p d) ((>-|-) f) . ((<-|-) g)

(>-|->-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Contravariant m m (p b), Contravariant m m (Flip p c), Interpreted m (Flip p c))
	=> m a b :*: m c d -> m (p b d) (p a c)
(>-|->-|-) (f :*: g) = (=||) @m @(Flip p c) ((>-|-) f) . ((>-|-) g)
