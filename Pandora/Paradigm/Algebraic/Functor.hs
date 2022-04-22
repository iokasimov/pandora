{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Algebraic.Functor where

import Pandora.Core.Interpreted (Interpreted ((<~), (<~~~), (-#=)))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Morphism.Flip (Flip)
import Pandora.Pattern.Morphism.Trip (Trip)
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|---), (<-|-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit), Unit)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Algebraic.Exponential (type (-->), type (<--), (&))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Paradigm.Algebraic.Zero (Zero, absurd)
import Pandora.Paradigm.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Functor.Proxy (Proxy (Proxy))

infixl 1 <-*------, .-*------, <-||-----, >-||-----
infixl 2 <-*-----, .-*-----, <-||----, >-||----
infixl 3 <-*----, .-*----, <-||---, >-||---
infixl 4 <-*---, .-*---, <-*-*-, <-||--, >-||--
infixl 5 <-*--, .-*--, .-*-*-, <-||-, >-||-
infixl 6 <-*-, .-*-, <-+-
infixl 7 -+-

infixl 6 <-|-<-|-, <-|->-|-, >-|-<-|-, >-|->-|-

type instance Unit (:*:) = One
type instance Unit (:+:) = Zero

type Applicative t = (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t, Monoidal (-->) (-->) (:*:) (:*:) t)
type Alternative t = (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:+:) t, Monoidal (-->) (-->) (:*:) (:+:) t)
type Divisible t = (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:*:) t, Monoidal (-->) (<--) (:*:) (:*:) t)
type Decidable t = (Covariant (->) (->) t, Semimonoidal (<--) (:*:) (:+:) t, Monoidal (-->) (<--) (:*:) (:+:) t)

instance Adjoint (->) (->) ((:*:) s) ((->) s) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f (s :*: x)
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

(<-*--------), (<-*-------), (<-*------), (<-*-----), (<-*----), (<-*---), (<-*--), (<-*-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t (a -> b) -> t a -> t b
f <-*-------- x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x
f <-*------- x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x
f <-*------ x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x
f <-*----- x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x
f <-*---- x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x
f <-*--- x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x
f <-*-- x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x
f <-*- x = (|-) @(->) @(->) (&) <-|--- mult @(-->) @_ @(:*:) <~~~ f :*: x

(.-*--------), (.-*-------), (.-*------), (.-*-----), (.-*----), (.-*---), (.-*--), (.-*-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t b -> t a -> t b
y .-*-------- x = (\_ y' -> y') <-|- x <-*- y
y .-*------- x = (\_ y' -> y') <-|- x <-*- y
y .-*------ x = (\_ y' -> y') <-|- x <-*- y
y .-*----- x = (\_ y' -> y') <-|- x <-*- y
y .-*---- x = (\_ y' -> y') <-|- x <-*- y
y .-*--- x = (\_ y' -> y') <-|- x <-*- y
y .-*-- x = (\_ y' -> y') <-|- x <-*- y
y .-*- x = (\_ y' -> y') <-|- x <-*- y

(<-*-*-) :: (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) u) => t (u (a -> b)) -> t (u a) -> t (u b)
f <-*-*- x = (<-*-) <-|- f <-*- x

(.-*-*-) :: (Covariant (->) (->) t, Covariant (->) (->) u, Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) u) => t (u b) -> t (u a) -> t (u b)
y .-*-*- x = (\_ y' -> y') <-|-|- x <-*-*- y

(<-+-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:+:) t) => t b -> t a -> (a :+: b -> r) -> t r
y <-+- x = \f -> f <-|--- mult @(-->) <~~~ x :*: y

(-+-) :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:+:) t) => t a -> t a -> t a
y -+- x = (\r -> case r of Option rx -> rx; Adoption ry -> ry) <-|--- mult @(-->) <~~~ x :*: y

loop :: (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => t a -> t b
loop x = let r = r .-*- x in r

type Extractable t = Monoidal (<--) (-->) (:*:) (:*:) t
type Pointable t = Monoidal (-->) (-->) (:*:) (:*:) t
type Emptiable t = Monoidal (-->) (-->) (:*:) (:+:) t

extract :: Extractable t => t a -> a
extract j = unit @(<--) @(-->) Proxy <~ j <~ One

point :: Pointable t => a -> t a
point x = unit @(-->) Proxy <~~~ Straight <-- \One -> x

pass :: Pointable t => t ()
pass = point ()

empty :: Emptiable t => t a
empty = unit @(-->) Proxy <~ Straight absurd

(<-||-), (<-||--), (<-||---), (<-||----), (<-||-----), (<-||------), (<-||-------), (<-||--------)
	:: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c .
	(Covariant m m (Flip p c), Interpreted m (Flip p c)) => m a b -> m (p a c) (p b c)
(<-||--------) f = (-#=) @m @(Flip p c) ((<-|-) f)
(<-||-------) f = (-#=) @m @(Flip p c) ((<-|-) f)
(<-||------) f = (-#=) @m @(Flip p c) ((<-|-) f)
(<-||-----) f = (-#=) @m @(Flip p c) ((<-|-) f)
(<-||----) f = (-#=) @m @(Flip p c) ((<-|-) f)
(<-||---) f = (-#=) @m @(Flip p c) ((<-|-) f)
(<-||--) f = (-#=) @m @(Flip p c) ((<-|-) f)
(<-||-) f = (-#=) @m @(Flip p c) ((<-|-) f)


(<-|||-), (<-|||--), (<-|||---), (<-|||----), (<-|||-----), (<-|||------), (<-|||-------), (<-|||--------)
	:: forall (m :: * -> * -> *) (v :: * -> * -> * -> *) a b c d .
	(Covariant m m (Trip v d c), Interpreted m (Trip v d c)) => m a b -> m (v a c d) (v b c d)
(<-|||--------) f = (-#=) @m @(Trip v d c) ((<-|-) f)
(<-|||-------) f = (-#=) @m @(Trip v d c) ((<-|-) f)
(<-|||------) f = (-#=) @m @(Trip v d c) ((<-|-) f)
(<-|||-----) f = (-#=) @m @(Trip v d c) ((<-|-) f)
(<-|||----) f = (-#=) @m @(Trip v d c) ((<-|-) f)
(<-|||---) f = (-#=) @m @(Trip v d c) ((<-|-) f)
(<-|||--) f = (-#=) @m @(Trip v d c) ((<-|-) f)
(<-|||-) f = (-#=) @m @(Trip v d c) ((<-|-) f)

(>-||-), (>-||--), (>-||---), (>-||----), (>-||-----), (>-||------), (>-||-------), (>-||--------)
	:: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c .
	(Contravariant m m (Flip p c), Interpreted m (Flip p c)) => m a b -> m (p b c) (p a c)
(>-||--------) f = (-#=) @m @(Flip p c) ((>-|-) f)
(>-||-------) f = (-#=) @m @(Flip p c) ((>-|-) f)
(>-||------) f = (-#=) @m @(Flip p c) ((>-|-) f)
(>-||-----) f = (-#=) @m @(Flip p c) ((>-|-) f)
(>-||----) f = (-#=) @m @(Flip p c) ((>-|-) f)
(>-||---) f = (-#=) @m @(Flip p c) ((>-|-) f)
(>-||--) f = (-#=) @m @(Flip p c) ((>-|-) f)
(>-||-) f = (-#=) @m @(Flip p c) ((>-|-) f)

(<-|-<-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Covariant m m (p a), Covariant m m (Flip p d), Interpreted m (Flip p d))
	=> m a b :*: m c d -> m (p a c) (p b d)
(<-|-<-|-) (f :*: g) = (-#=) @m @(Flip p d) ((<-|-) f) . ((<-|-) g)

(<-|->-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Covariant m m (Flip p c), Contravariant m m (p a), Interpreted m (Flip p c))
	=> m a b :*: m c d -> m (p a d) (p b c)
(<-|->-|-) (f :*: g) = (-#=) @m @(Flip p c) ((<-|-) f) . ((>-|-) g)

(>-|-<-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Contravariant m m (Flip p d), Covariant m m (p b), Interpreted m (Flip p d))
	=> m a b :*: m c d -> m (p b c) (p a d)
(>-|-<-|-) (f :*: g) = (-#=) @m @(Flip p d) ((>-|-) f) . ((<-|-) g)

(>-|->-|-) :: forall (m :: * -> * -> *) (p :: * -> * -> *) a b c d .
	(Contravariant m m (p b), Contravariant m m (Flip p c), Interpreted m (Flip p c))
	=> m a b :*: m c d -> m (p b d) (p a c)
(>-|->-|-) (f :*: g) = (-#=) @m @(Flip p c) ((>-|-) f) . ((>-|-) g)

void :: Covariant (->) (->) t => t a -> t ()
void x = constant () <-|- x
