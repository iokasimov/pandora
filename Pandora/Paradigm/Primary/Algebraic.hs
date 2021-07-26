{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic (module Exports, (-<*>-)) where

import Pandora.Paradigm.Primary.Algebraic.Exponential as Exports
import Pandora.Paradigm.Primary.Algebraic.Product as Exports
import Pandora.Paradigm.Primary.Algebraic.Sum as Exports

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Semimonoidal (multiply), Semimonoidal_ (multiply_))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))

instance Semimonoidal ((->) e) (:*:) (->) (->) where
	multiply f (g :*: h) = \x -> f $ g x :*: h x

instance Semimonoidal ((:+:) e) (:*:) (->) (->) where
	multiply f (Adoption x :*: Adoption y) = Adoption . f $ x :*: y
	multiply _ (Option x :*: _) = Option x
	multiply _ (_ :*: Option x) = Option x

instance Traversable ((:*:) s) (->) (->) where
	f <<- x = (attached x :*:) -<$>- f (extract x)

instance Adjoint ((:*:) s) ((->) s) (->) (->) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f $ s :*: x
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

infixl 4 -<*>-

(-<*>-) :: forall a b t . (Semimonoidal t (:*:) (->) (->)) => t (a -> b) -> t a -> t b
(-<*>-) = (%) ((-|) @((:*:) (t (a -> b))) (multiply @t @(:*:) @(->) @(->) ((&) |-)))

instance Semimonoidal_ ((->) e) (->) (:*:) (:*:) where
	multiply_ :: ((e -> a) :*: (e -> b)) -> e -> (a :*: b)
	multiply_ (g :*: h) = \x -> g x :*: h x

instance Semimonoidal_ ((:+:) e) (->) (:*:) (:+:) where
	multiply_ :: ((e :+: a) :*: (e :+: b)) -> e :+: a :+: b
	multiply_ (Option e :*: Option e') = Option e'
	multiply_ (Option e :*: Adoption y) = Adoption $ Adoption y
	multiply_ (Adoption x :*: _) = Adoption $ Option x

type Applicative_ t = (Covariant_ t (->) (->), Semimonoidal_ t (->) (:*:) (:*:))
type Alternative_ t = (Covariant_ t (->) (->), Semimonoidal_ t (->) (:*:) (:+:))

(-*-) :: Applicative_ t => t (a -> b) -> t a -> t b
f -*- x = (|-) @_ @_ @(->) @(->) (&) -<$>- multiply_ @_ @_ @_ @(:*:) (f :*: x)
