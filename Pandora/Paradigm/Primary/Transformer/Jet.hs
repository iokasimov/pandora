{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Jet where

import Pandora.Pattern.Functor.Covariant (Covariant_ ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)), (-<<-<<-))
import Pandora.Paradigm.Primary.Algebraic ((-<*>-))

data Jet t a = Jet a (Jet t (t a))

instance Covariant_ t (->) (->) => Covariant_ (Jet t) (->) (->) where
	f -<$>- Jet x xs = Jet (f x) (f -<$$>- xs)

instance Traversable t (->) (->) => Traversable (Jet t) (->) (->) where
	f <<- Jet x xs = Jet -<$>- f x -<*>- f -<<-<<- xs

instance Covariant_ t (->) (->) => Extractable (Jet t) (->) where
	extract (Jet x _) = x
