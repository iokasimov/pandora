{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Accumulator (Accumulator (..), Accumulated, gather) where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (lay, wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.UT (UT (UT), type (<.:>))

newtype Accumulator e a = Accumulator (e :*: a)

instance Covariant (Accumulator e) where
	f <$> Accumulator x = Accumulator $ f <$> x

instance Semigroup e => Applicative (Accumulator e) where
	f <*> v = Accumulator $ k (run f) (run v) where
		k ~(e :*: g) ~(e' :*: w) = e + e' :*: g w

instance Monoid e => Pointable (Accumulator e) where
	point = Accumulator . (zero :*:)

instance Semigroup e => Bindable (Accumulator e) where
	Accumulator (e :*: x) >>= f = let (e' :*: b) = run $ f x in
		Accumulator $ e + e':*: b

type instance Schematic Monad (Accumulator e) u = (:*:) e <.:> u

instance Interpreted (Accumulator e) where
	type Primary (Accumulator e) a = e :*: a
	run (Accumulator x) = x

instance Monoid e => Monadic (Accumulator e) where
	lay x = TM . UT $ (zero :*:) <$> x
	wrap = TM . UT . point . run

type Accumulated e t = Adaptable (Accumulator e) t

instance Covariant u => Covariant ((:*:) e <.:> u) where
	f <$> UT x = UT $ f <$$> x

instance (Semigroup e, Applicative u) => Applicative ((:*:) e <.:> u) where
	UT f <*> UT x = UT $ k <$> f <*> x where
		k ~(u :*: g) ~(v :*: y) = u + v :*: g y

instance (Pointable u, Monoid e) => Pointable ((:*:) e <.:> u) where
	point = UT . point . (zero :*:)

instance (Semigroup e, Pointable u, Bindable u) => Bindable ((:*:) e <.:> u) where
	UT x >>= f = UT $ x >>= \(acc :*: v) -> (\(acc' :*: y) -> (acc + acc' :*: y)) <$> run (f v)

gather :: Accumulated e t => e -> t ()
gather x = adapt . Accumulator $ x :*: ()
