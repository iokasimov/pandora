{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic (module Exports, Applicative_, Alternative_, Extractable_, ($>-), ($$>-), ($$$>-), (-<*>-), (*>-), forever_, (-+-), void, empty, point, extract) where

import Pandora.Paradigm.Primary.Algebraic.Exponential as Exports
import Pandora.Paradigm.Primary.Algebraic.Product as Exports
import Pandora.Paradigm.Primary.Algebraic.Sum as Exports
import Pandora.Paradigm.Primary.Algebraic.Zero as Exports
import Pandora.Paradigm.Primary.Algebraic.One as Exports

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor (Endofunctor)
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)), (-<$$>-), (-<$$$>-))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit), Unit)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Functor.Proxy (Proxy (Proxy))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))

type instance Unit (:*:) = One
type instance Unit (:+:) = Zero

infixl 4 -<*>-

($>-) :: Covariant t (->) (->) => t a -> b -> t b
x $>- r = (r !.) -<$>- x

($$>-) :: (Covariant t (->) (->), Covariant u (->) (->)) => t (u a) -> b -> t (u b)
x $$>- r = (r !.) -<$$>- x

($$$>-) :: (Covariant t (->) (->), Covariant u (->) (->), Covariant v (->) (->)) => t (u (v a)) -> b -> t (u (v b))
x $$$>- r = (r !.) -<$$$>- x

void :: Covariant t (->) (->) => t a -> t ()
void x = x $>- ()

instance Traversable ((:*:) s) (->) (->) where
	f <<- x = (attached x :*:) -<$>- f (extract x)

instance Adjoint ((:*:) s) ((->) s) (->) (->) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f $ s :*: x
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

instance Semimonoidal ((->) e) (->) (:*:) (:*:) where
	multiply :: ((e -> a) :*: (e -> b)) -> e -> (a :*: b)
	multiply (g :*: h) = \x -> g x :*: h x

instance Semimonoidal ((->) e) (<--) (:*:) (:*:) where
	multiply = Flip $ \f -> (\e -> attached $ f e) :*: (\e -> extract $ f e)

instance Semimonoidal ((:+:) e) (->) (:*:) (:+:) where
	multiply :: ((e :+: a) :*: (e :+: b)) -> e :+: a :+: b
	multiply (Option _ :*: Option e') = Option e'
	multiply (Option _ :*: Adoption y) = Adoption $ Adoption y
	multiply (Adoption x :*: _) = Adoption $ Option x

instance Semimonoidal ((:+:) e) (->) (:*:) (:*:) where
	multiply (Adoption x :*: Adoption y) = Adoption $ x :*: y
	multiply (Option e :*: _) = Option e
	multiply (_ :*: Option e) = Option e

instance Monoidal ((:+:) e) (->) (->) (:*:) (:*:) where
	unit _ f = Adoption $ f One

instance Semimonoidal ((:*:) s) (<--) (:*:) (:*:) where
	multiply = Flip $ \(s :*: x :*: y) -> (s :*: x) :*: (s :*: y)

instance Monoidal ((:*:) s) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(_ :*: x) -> (\_ -> x)

instance Comonad ((:*:) s) (->) where

instance Semimonoidal (Flip (:*:) a) (<--) (:*:) (:*:) where
	multiply = Flip $ \(Flip ((sx :*: sy) :*: r)) -> Flip (sx :*: r) :*: Flip (sy :*: r)

instance Monoidal (Flip (:*:) a) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(Flip (s :*: _)) -> (\_ -> s)

type Applicative_ t = (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:), Monoidal t (->) (->) (:*:) (:*:))
type Alternative_ t = (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:+:), Monoidal t (->) (->) (:*:) (:+:))

(-<*>-) :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:))
	=> t (a -> b) -> t a -> t b
f -<*>- x = (|-) @_ @_ @(->) @(->) (&) -<$>- multiply @_ @_ @_ @(:*:) (f :*: x)

forever_ :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:)) => t a -> t b
forever_ x = let r = x *>- r in r

(*>-) :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:)) => t a -> t b -> t b
x *>- y = ((!.) %) -<$>- x -<*>- y

(-+-) :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:+:))
	  => t a -> t b -> (a :+: b -> r) -> t r
x -+- y = \f -> f -<$>- multiply (x :*: y)

point :: Monoidal t (->) (->) (:*:) (:*:) => a -> t a
point x = unit (Proxy @(:*:)) (\One -> x)

empty :: Monoidal t (->) (->) (:*:) (:+:) => t a
empty = unit (Proxy @(:*:)) absurd

type Extractable_ t = Monoidal t (<--) (->) (:*:) (:*:)

extract :: Extractable_ t => t a -> a
extract j = let Flip f = unit @_ @(<--) @(->) @(:*:) @(:*:) Proxy in f j $ One
