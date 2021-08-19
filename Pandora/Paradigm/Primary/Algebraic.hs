{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic (module Exports, Applicative_, Alternative_, Extractable_, ($>-), ($$>-), ($$$>-), (-<*>-), (*>-), forever_, (-+-), void, empty, point, extract_) where

import Pandora.Paradigm.Primary.Algebraic.Exponential as Exports
import Pandora.Paradigm.Primary.Algebraic.Product as Exports
import Pandora.Paradigm.Primary.Algebraic.Sum as Exports
import Pandora.Paradigm.Primary.Algebraic.Zero as Exports
import Pandora.Paradigm.Primary.Algebraic.One as Exports

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor (Endofunctor)
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)), (-<$$>-), (-<$$$>-))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply_))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit), Unit)
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
	f <<- x = (attached x :*:) -<$>- f (extract_ x)

instance Adjoint ((:*:) s) ((->) s) (->) (->) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f $ s :*: x
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

instance Semimonoidal ((->) e) (->) (:*:) (:*:) where
	multiply_ :: ((e -> a) :*: (e -> b)) -> e -> (a :*: b)
	multiply_ (g :*: h) = \x -> g x :*: h x

instance Semimonoidal ((->) e) (<--) (:*:) (:*:) where
	multiply_ = Flip $ \f -> (\e -> attached $ f e) :*: (\e -> extract_ $ f e)

instance Semimonoidal ((:+:) e) (->) (:*:) (:+:) where
	multiply_ :: ((e :+: a) :*: (e :+: b)) -> e :+: a :+: b
	multiply_ (Option _ :*: Option e') = Option e'
	multiply_ (Option _ :*: Adoption y) = Adoption $ Adoption y
	multiply_ (Adoption x :*: _) = Adoption $ Option x

instance Semimonoidal ((:+:) e) (->) (:*:) (:*:) where
	multiply_ (Adoption x :*: Adoption y) = Adoption $ x :*: y
	multiply_ (Option e :*: _) = Option e
	multiply_ (_ :*: Option e) = Option e

instance Monoidal ((:+:) e) (->) (->) (:*:) (:*:) where
	unit _ f = Adoption $ f One

instance Semimonoidal ((:*:) s) (<--) (:*:) (:*:) where
	multiply_ = Flip $ \(s :*: x :*: y) -> (s :*: x) :*: (s :*: y)

instance Monoidal ((:*:) s) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(_ :*: x) -> (\_ -> x)

instance Semimonoidal (Flip (:*:) a) (<--) (:*:) (:*:) where
	multiply_ = Flip $ \(Flip ((sx :*: sy) :*: r)) -> Flip (sx :*: r) :*: Flip (sy :*: r)

instance Monoidal (Flip (:*:) a) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(Flip (s :*: _)) -> (\_ -> s)

type Applicative_ t = (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:), Monoidal t (->) (->) (:*:) (:*:))
type Alternative_ t = (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:+:), Monoidal t (->) (->) (:*:) (:+:))

(-<*>-) :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:))
	=> t (a -> b) -> t a -> t b
f -<*>- x = (|-) @_ @_ @(->) @(->) (&) -<$>- multiply_ @_ @_ @_ @(:*:) (f :*: x)

forever_ :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:)) => t a -> t b
forever_ x = let r = x *>- r in r

(*>-) :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:*:)) => t a -> t b -> t b
x *>- y = ((!.) %) -<$>- x -<*>- y

(-+-) :: (Endofunctor Covariant t (->), Semimonoidal t (->) (:*:) (:+:))
	  => t a -> t b -> (a :+: b -> r) -> t r
x -+- y = \f -> f -<$>- multiply_ (x :*: y)

point :: Monoidal t (->) (->) (:*:) (:*:) => a -> t a
point x = unit (Proxy @(:*:)) (\One -> x)

empty :: Monoidal t (->) (->) (:*:) (:+:) => t a
empty = unit (Proxy @(:*:)) absurd

type Extractable_ t = Monoidal t (<--) (->) (:*:) (:*:)

extract_ :: Extractable_ t => t a -> a
extract_ j = let Flip f = unit @_ @(<--) @(->) @(:*:) @(:*:) Proxy in f j $ One
