{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Algebraic (module Exports) where

import Pandora.Paradigm.Algebraic.Functor as Exports
import Pandora.Paradigm.Algebraic.Exponential as Exports
import Pandora.Paradigm.Algebraic.Product as Exports
import Pandora.Paradigm.Algebraic.Sum as Exports
import Pandora.Paradigm.Algebraic.Zero as Exports
import Pandora.Paradigm.Algebraic.One as Exports

import Pandora.Core.Functor (type (>))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted ((<~)))

instance (Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) u) => Semimonoidal (<--) (:*:) (:*:) (t <:*:> u) where
	mult = Flip <-- \(T_U lrxys) ->
		-- TODO: I need matrix transposing here
		let ((lxs :*: lys) :*: (rxs :*: rys)) = (mult @(<--) <~) <-||-- (mult @(<--) <~) <-|- lrxys in
		T_U (lxs :*: rxs) :*: T_U (lys :*: rys)

instance Traversable (->) (->) ((:*:) s) where
	f <<- x = (attached x :*:) <-|- f (extract x)

instance Semimonoidal (-->) (:*:) (:*:) ((->) e) where
	mult :: ((e -> a) :*: (e -> b)) --> (e -> (a :*: b))
	mult = Straight <-- \(g :*: h) -> \x -> g x :*: h x

instance Monoidal (-->) (-->) (:*:) (:*:) ((->) e) where
	unit _ = Straight <-- constant . (<~ One)

instance Semimonoidal (<--) (:*:) (:*:) ((->) e) where
	mult :: ((e -> a) :*: (e -> b)) <-- (e -> a :*: b)
	mult = Flip <-- \f -> attached . f :*: extract . f

instance Semimonoidal (-->) (:*:) (:+:) ((:+:) e) where
	mult :: ((e :+: a) :*: (e :+: b)) --> (e :+: a :+: b)
	mult = Straight <-- \case
		Option _ :*: Option e' -> Option e'
		Option _ :*: Adoption y -> Adoption <-- Adoption y
		Adoption x :*: _ -> Adoption <-- Option x

instance Semimonoidal (-->) (:*:) (:*:) ((:+:) e) where
	mult = Straight <-- \case
		Adoption x :*: Adoption y -> Adoption <--- x :*: y
		Option e :*: _ -> Option e
		_ :*: Option e -> Option e

instance Monoidal (-->) (-->) (:*:) (:*:) ((:+:) e) where
	unit _ = Straight <-- Adoption . (<~ One)

instance Semimonoidal (<--) (:*:) (:*:) ((:*:) s) where
	mult = Flip <-- \(s :*: x :*: y) -> (s :*: x) :*: (s :*: y)

instance Monoidal (<--) (-->) (:*:) (:*:) ((:*:) s) where
	unit _ = Flip <-- \(_ :*: x) -> Straight (\_ -> x)

instance Comonad (->) ((:*:) s) where

instance Semimonoidal (<--) (:*:) (:*:) (Flip (:*:) a) where
	mult = Flip <-- \(Flip ((sx :*: sy) :*: r)) -> Flip (sx :*: r) :*: Flip (sy :*: r)

instance Monoidal (<--) (-->) (:*:) (:*:) (Flip (:*:) a) where
	unit _ = Flip <-- \(Flip (s :*: _)) -> Straight (\_ -> s)