module Pandora.Paradigm.Primary.Transformer.Construction where

import Pandora.Core.Functor (type (:.), type (:=), type (|->), type (~>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>), extend))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid ((*))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Controlflow (run)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

data Construction t a = Construct a (t :. Construction t := a)

instance Covariant t => Covariant (Construction t) where
	f <$> x = Construct (f $ extract x) $ f <$$> deconstruct x

instance Avoidable t => Pointable (Construction t) where
	point x = Construct x empty

instance Covariant t => Extractable (Construction t) where
	extract ~(Construct x _) = x

instance Applicative t => Applicative (Construction t) where
	f <*> x = Construct (extract f $ extract x)
		$ deconstruct f <**> deconstruct x

instance Traversable t => Traversable (Construction t) where
	x ->> f = Construct <$> f (extract x) <*> deconstruct x ->>> f

instance Alternative t => Bindable (Construction t) where
	x >>= f = Construct (extract . f $ extract x)
		$ (deconstruct . f $ extract x) <+> (>>= f) <$> deconstruct x

instance Covariant t => Extendable (Construction t) where
	x =>> f = Construct (f x) $ extend f <$> deconstruct x

instance (Avoidable t, Alternative t) => Monad (Construction t) where

instance Covariant t => Comonad (Construction t) where

instance Lowerable Construction where
	lower x = extract <$> deconstruct x

instance Hoistable Construction where
	hoist f x = Construct (extract x) . f $ hoist f <$> deconstruct x

instance (Setoid a, forall b . Setoid b => Setoid (t b), Covariant t) => Setoid (Construction t a) where
	x == y = (extract x == extract y) * (deconstruct x == deconstruct y)

instance (Semigroup a, forall b . Semigroup b => Semigroup (t b), Covariant t) => Semigroup (Construction t a) where
	x + y = Construct (extract x + extract y) $ deconstruct x + deconstruct y

instance (Monoid a, forall b . Semigroup b => Monoid (t b), Covariant t) => Monoid (Construction t a) where
	zero = Construct zero zero

deconstruct :: Construction t a -> (t :. Construction t) a
deconstruct ~(Construct _ xs) = xs

iterate :: Covariant t => (a |-> t) -> (a |-> Construction t)
iterate f x = Construct x $ iterate f <$> f x

section :: Comonad t => t ~> Construction t
section xs = Construct (extract xs) $ xs =>> section

instance (Covariant u, Covariant t) => Covariant (t <:.> Construction u) where
	f <$> g = TU $ f <$$> run g

instance (Avoidable u, Pointable t) => Pointable (t <:.> Construction u) where
	point x = TU . point . Construct x $ empty

instance (Applicative u, Applicative t) => Applicative (t <:.> Construction u) where
	f <*> x = TU $ run f <**> run x

instance (Covariant u, Alternative t) => Alternative (t <:.> Construction u) where
	x <+> y = TU $ run x <+> run y

instance (Covariant u, Avoidable t) => Avoidable (t <:.> Construction u) where
	empty = TU empty

instance (Traversable u, Traversable t) => Traversable (t <:.> Construction u) where
	g ->> f = TU <$> run g ->>> f
