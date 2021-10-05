{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic (module Exports, Applicative, Alternative, Extractable, Pointable, ($>-), ($$>-), ($$$>-), (<-*-), (*>-), forever_, (-+-), void, empty, point, extract) where

import Pandora.Paradigm.Primary.Algebraic.Exponential as Exports
import Pandora.Paradigm.Primary.Algebraic.Product as Exports
import Pandora.Paradigm.Primary.Algebraic.Sum as Exports
import Pandora.Paradigm.Primary.Algebraic.Zero as Exports
import Pandora.Paradigm.Primary.Algebraic.One as Exports

import Pandora.Core.Appliable ((!))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), (-<$$>-), (-<$$$>-))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit), Unit)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Functor.Proxy (Proxy (Proxy))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)

type instance Unit (:*:) = One
type instance Unit (:+:) = Zero

infixl 4 <-*-

($>-) :: Covariant (->) (->) t => t a -> b -> t b
x $>- r = (r !.) <$> x

($$>-) :: (Covariant (->) (->) t, Covariant (->) (->) u) => t (u a) -> b -> t (u b)
x $$>- r = (r !.) -<$$>- x

($$$>-) :: (Covariant (->) (->) t, Covariant (->) (->) u, Covariant (->) (->) v) => t (u (v a)) -> b -> t (u (v b))
x $$$>- r = (r !.) -<$$$>- x

void :: Covariant (->) (->) t => t a -> t ()
void x = x $>- ()

instance Traversable (->) (->) ((:*:) s) where
	f <<- x = (attached x :*:) <$> f (extract x)

instance Adjoint (->) (->) ((:*:) s) ((->) s) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f $ s :*: x
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

instance Semimonoidal (-->) (:*:) (:*:) ((->) e) where
	mult :: ((e -> a) :*: (e -> b)) --> (e -> (a :*: b))
	mult = Straight $ \(g :*: h) -> \x -> g x :*: h x

instance Semimonoidal (<--) (:*:) (:*:) ((->) e) where
	mult :: ((e -> a) :*: (e -> b)) <-- (e -> a :*: b)
	mult = Flip $ \f -> attached . f :*: extract . f

instance Semimonoidal (-->) (:*:) (:+:) ((:+:) e) where
	mult :: ((e :+: a) :*: (e :+: b)) --> (e :+: a :+: b)
	mult = Straight $ \case
		Option _ :*: Option e' -> Option e'
		Option _ :*: Adoption y -> Adoption $ Adoption y
		Adoption x :*: _ -> Adoption $ Option x

instance Semimonoidal (-->) (:*:) (:*:) ((:+:) e) where
	mult = Straight $ \case
		Adoption x :*: Adoption y -> Adoption $ x :*: y
		Option e :*: _ -> Option e
		_ :*: Option e -> Option e

instance Monoidal (-->) (->) (:*:) (:*:) ((:+:) e) where
	unit _ = Straight $ Adoption . ($ One)

instance Semimonoidal (<--) (:*:) (:*:) ((:*:) s) where
	mult = Flip $ \(s :*: x :*: y) -> (s :*: x) :*: (s :*: y)

instance Monoidal (<--) (->) (:*:) (:*:) ((:*:) s) where
	unit _ = Flip $ \(_ :*: x) -> (\_ -> x)

instance Comonad (->) ((:*:) s) where

instance Semimonoidal (<--) (:*:) (:*:) (Flip (:*:) a) where
	mult = Flip $ \(Flip ((sx :*: sy) :*: r)) -> Flip (sx :*: r) :*: Flip (sy :*: r)

instance Monoidal (<--) (->) (:*:) (:*:) (Flip (:*:) a) where
	unit _ = Flip $ \(Flip (s :*: _)) -> (\_ -> s)

type Applicative t = (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t, Monoidal (-->) (->) (:*:) (:*:) t)
type Alternative t = (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:+:) t, Monoidal (-->) (->) (:*:) (:+:) t)

(<-*-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t (a -> b) -> t a -> t b
f <-*- x = (|-) @(->) @(->) (&) <$> run (mult @(-->) @(:*:) @(:*:)) (f :*: x)

forever_ :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t a -> t b
forever_ x = let r = x *>- r in r

(*>-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t a -> t b -> t b
x *>- y = ((!.) %) <$> x <-*- y

(-+-) :: (Covariant (->) (->) t, Semimonoidal (->) (:*:) (:+:) t)
	=> t a -> t b -> (a :+: b -> r) -> t r
x -+- y = \f -> f <$> mult (x :*: y)

point :: Monoidal (-->) (->) (:*:) (:*:) t => a -> t a
point x = run (unit @(-->) @(->) @(:*:) @(:*:) Proxy) (\One -> x)

empty :: Monoidal (-->) (->) (:*:) (:+:) t => t a
empty = unit @(-->) @(->) @(:*:) @(:+:) (Proxy @(:*:)) ! absurd

type Extractable t = Monoidal (<--) (->) (:*:) (:*:) t
type Pointable t = Monoidal (-->) (->) (:*:) (:*:) t

extract :: Extractable t => t a -> a
extract j = run (unit @(<--) @(->) @(:*:) @(:*:) Proxy) j One

--instance Appliable (->) b c (->) e d => Appliable (->) a (b -> c) (->) (a :*: e) d where
--	f ! (x :*: y) = f x ! y
