{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Algebraic (module Exports, (-<*>-)) where

import Pandora.Paradigm.Primary.Algebraic.Exponential as Exports
import Pandora.Paradigm.Primary.Algebraic.Product as Exports

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative_ (multiply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)), Traversable_ ((-<<--)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))

infixl 4 -<*>-

instance Applicative_ ((->) e) (:*:) (->) (->) where
	multiply f (g :*: h) = \x -> f $ g x :*: h x

instance Traversable ((:*:) s) where
	x ->> f = (attached x :*:) <$> f (extract x)

instance Traversable_ ((:*:) s) (->) (->) where
	f -<<-- x = (attached x :*:) -<$>- f (extract x)

instance Adjoint ((:*:) s) ((->) s) (->) (->) where
	(-|) :: ((s :*: a) -> b) -> a -> (s -> b)
	f -| x = \s -> f $ s :*: x
	(|-) :: (a -> s -> b) -> (s :*: a) -> b
	f |- ~(s :*: x) = f x s

(-<*>-) :: forall a b t . (Applicative_ t (:*:) (->) (->)) => t (a -> b) -> t a -> t b
(-<*>-) = (%) ((-|) @((:*:) (t (a -> b))) (multiply @t @(:*:) @(->) @(->) ((&) |-)))
