{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Functor (module Exports, Equivalence, Comparison, match, (-<*>-)) where

import Pandora.Paradigm.Primary.Functor.Fix as Exports
import Pandora.Paradigm.Primary.Functor.Convergence as Exports
import Pandora.Paradigm.Primary.Functor.Predicate as Exports
import Pandora.Paradigm.Primary.Functor.These as Exports
import Pandora.Paradigm.Primary.Functor.Validation as Exports
import Pandora.Paradigm.Primary.Functor.Wedge as Exports
import Pandora.Paradigm.Primary.Functor.Wye as Exports
import Pandora.Paradigm.Primary.Functor.Edges as Exports
import Pandora.Paradigm.Primary.Functor.Conclusion as Exports
import Pandora.Paradigm.Primary.Functor.Maybe as Exports
import Pandora.Paradigm.Primary.Functor.Endo as Exports
import Pandora.Paradigm.Primary.Functor.Proxy as Exports
import Pandora.Paradigm.Primary.Functor.Tagged as Exports
import Pandora.Paradigm.Primary.Functor.Product as Exports
import Pandora.Paradigm.Primary.Functor.Constant as Exports
import Pandora.Paradigm.Primary.Functor.Identity as Exports
import Pandora.Paradigm.Primary.Functor.Function as Exports

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Applicative (Applicative_ (multiply))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)), Adjoint_ ((--|-), (-|--)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean, (?))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering)

type Equivalence = Convergence Boolean
type Comparison = Convergence Ordering

instance Adjoint (Product s) ((->) s) where
	(-|) :: a -> ((s :*: a) -> b) -> (s -> b)
	x -| f = \s -> f $ s :*: x
	(|-) :: (s :*: a) -> (a -> s -> b) -> b
	~(s :*: x) |- f = f x s

instance Adjoint_ (Product s) ((->) s) (->) (->) where
	(--|-) :: ((s :*: a) -> b) -> a -> (s -> b)
	f --|- x = \s -> f $ s :*: x
	(-|--) :: (a -> s -> b) -> (s :*: a) -> b
	f -|-- ~(s :*: x) = f x s

instance Applicative_ ((->) e) (:*:) (->) (->) where
	multiply f (g :*: h) = \x -> f $ g x :*: h x

match :: Predicate a -> (a -> r) -> a -> r -> r :*: a
match (Predicate p) f x r = p x ? (f x :*: x) $ r :*: x

(-<*>-) :: forall a b t v . (Applicative_ t v (->) (->), Adjoint_ (v (a -> b)) ((->) (a -> b)) (->) (->), Adjoint_ (v (t (a -> b))) ((->) (t (a -> b))) (->) (->)) => t (a -> b) -> t a -> t b
(-<*>-) = (%) ((--|-) @(v (t (a -> b))) (multiply @t @v @(->) @(->) ((&) -|--)))
