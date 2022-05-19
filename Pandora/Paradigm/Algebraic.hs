{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Algebraic (module Exports, type (:+*+:), type (:*+*:), type (:++*:)) where

import Pandora.Paradigm.Algebraic.Functor as Exports
import Pandora.Paradigm.Algebraic.Exponential as Exports
import Pandora.Paradigm.Algebraic.Product as Exports
import Pandora.Paradigm.Algebraic.Sum as Exports
import Pandora.Paradigm.Algebraic.Zero as Exports
import Pandora.Paradigm.Algebraic.One as Exports

import Pandora.Core.Interpreted (Interpreted ((<~)))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|---)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U))

instance (Semimonoidal (<--) (:*:) (:*:) t, Semimonoidal (<--) (:*:) (:*:) u) => Semimonoidal (<--) (:*:) (:*:) (t <:*:> u) where
	mult = Flip <-- \(T_U lrxys) ->
		-- TODO: I need matrix transposing here
		let ((lxs :*: lys) :*: (rxs :*: rys)) = (mult @(<--) <~) <<-|-- (mult @(<--) <~) <-|- lrxys in
		T_U (lxs :*: rxs) :*: T_U (lys :*: rys)

instance (Semimonoidal (-->) (:*:) (:*:) t, Semimonoidal (-->) (:*:) (:*:) u) => Semimonoidal (-->) (:*:) (:*:) (t <:*:> u) where
	mult = Straight <-- \(T_U (xls :*: xrs) :*: T_U (yls :*: yrs)) -> T_U <--- (mult @(-->) <~) (xls :*: yls) :*: (mult @(-->) <~) (xrs :*: yrs)

instance (Semimonoidal (-->) (:*:) (:+:) t, Semimonoidal (-->) (:*:) (:+:) u) => Semimonoidal (-->) (:*:) (:+:) (t <:*:> u) where
	mult = Straight <-- \(T_U (xls :*: xrs) :*: T_U (yls :*: yrs)) ->
		(mult @(-->) @(:*:) @(:+:) <~ (xls :*: yls)) <:*:> (mult @(-->) @(:*:) @(:+:) <~ (xrs :*: yrs))

instance (Monoidal (-->) (-->) (:*:) (:+:) t, Monoidal (-->) (-->) (:*:) (:+:) u)
	=> Monoidal (-->) (-->) (:*:) (:+:) (t <:*:> u) where
		unit _ = Straight <-- \_ -> empty <:*:> empty

instance (Traversable (->) (->) t, Traversable (->) (->) u) => Traversable (->) (->) (t <:*:> u) where
	f <-/- T_U (xs :*: ys) = T_U <-|--- (:*:) <-|-- f <-/- xs <-*-- f <-/- ys

instance Traversable (->) (->) ((:*:) s) where
	f <-/- x = (attached x :*:) <-|- f (extract x)

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

type (:+*+:) l r = (l :+: r) :*: (r :+: l)

type (:*+*:) l r = (l :*: r) :+: (r :*: l)

type (:++*:) l r = (l :+: r) :+: (l :*: r)
