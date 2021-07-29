{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic (module Exports, (-*-), (-+-)) where

import Pandora.Paradigm.Primary.Algebraic.Exponential as Exports
import Pandora.Paradigm.Primary.Algebraic.Product as Exports
import Pandora.Paradigm.Primary.Algebraic.Sum as Exports

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor (Endofunctor)
import Pandora.Pattern.Functor.Covariant (Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Semimonoidal_ (multiply_))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))

infixl 4 -*-

instance Traversable ((:*:) s) (->) (->) where
	f <<- x = (attached x :*:) -<$>- f (extract x)

instance Adjoint ((:*:) s) ((->) s) (->) (->) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f $ s :*: x
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

instance Semimonoidal_ ((->) e) (->) (:*:) (:*:) where
	multiply_ :: ((e -> a) :*: (e -> b)) -> e -> (a :*: b)
	multiply_ (g :*: h) = \x -> g x :*: h x

instance Semimonoidal_ ((:+:) e) (->) (:*:) (:+:) where
	multiply_ :: ((e :+: a) :*: (e :+: b)) -> e :+: a :+: b
	multiply_ (Option _ :*: Option e') = Option e'
	multiply_ (Option _ :*: Adoption y) = Adoption $ Adoption y
	multiply_ (Adoption x :*: _) = Adoption $ Option x

-- TODO: Generalize over (->)

type Applicative_ t = (Endofunctor Covariant_ t (->), Semimonoidal_ t (->) (:*:) (:*:))
type Alternative_ t = (Endofunctor Covariant_ t (->), Semimonoidal_ t (->) (:*:) (:+:))

(-*-) :: Applicative_ t => t (a -> b) -> t a -> t b
f -*- x = (|-) @_ @_ @(->) @(->) (&) -<$>- multiply_ @_ @_ @_ @(:*:) (f :*: x)

(-+-) :: Alternative_ t => t a -> t b -> (a :+: b -> r) -> t r
x -+- y = \f -> f -<$>- multiply_ (x :*: y)
