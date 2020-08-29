module Pandora.Pattern.Functor (module Exports, (<*+>), (<**+>), (<***+>)) where

import Pandora.Pattern.Functor.Bivariant as Exports
import Pandora.Pattern.Functor.Divariant as Exports
import Pandora.Pattern.Functor.Comonad as Exports
import Pandora.Pattern.Functor.Monad as Exports
import Pandora.Pattern.Functor.Representable as Exports
import Pandora.Pattern.Functor.Adjoint as Exports
import Pandora.Pattern.Functor.Extendable as Exports
import Pandora.Pattern.Functor.Bindable as Exports
import Pandora.Pattern.Functor.Distributive as Exports
import Pandora.Pattern.Functor.Traversable as Exports
import Pandora.Pattern.Functor.Determinable as Exports
import Pandora.Pattern.Functor.Extractable as Exports
import Pandora.Pattern.Functor.Pointable as Exports
import Pandora.Pattern.Functor.Avoidable as Exports
import Pandora.Pattern.Functor.Applicative as Exports
import Pandora.Pattern.Functor.Alternative as Exports
import Pandora.Pattern.Functor.Invariant as Exports
import Pandora.Pattern.Functor.Contravariant as Exports
import Pandora.Pattern.Functor.Covariant as Exports

import Pandora.Core.Functor (type (:.), type (:=))

(<*+>) :: (Applicative t, Alternative u) => t :. u := a -> t :. u := a -> t :. u := a
x <*+> y = (<+>) <$> x <*> y

(<**+>) :: (Applicative t, Applicative u, Alternative v)
	=> t :. u :. v := a -> t :. u :. v := a -> t :. u :. v := a
x <**+> y = (<+>) <$$> x <**> y

(<***+>) :: (Applicative t, Applicative u, Applicative v, Alternative w)
	=> t :. u :. v :. w := a -> t :. u :. v :. w := a -> t :. u :. v :. w := a
x <***+> y = (<+>) <$$$> x <***> y
