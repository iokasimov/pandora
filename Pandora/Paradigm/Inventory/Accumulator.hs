{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Accumulator (Accumulator (..), Accumulated, gather) where

import Pandora.Pattern.Category ((.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.UT (UT (UT), type (<.:>))

newtype Accumulator e a = Accumulator (e :*: a)

instance Covariant (Accumulator e) where
	f <$> Accumulator x = Accumulator $ f <$> x

instance Covariant_ (Accumulator e) (->) (->) where
	f -<$>- Accumulator x = Accumulator $ f -<$>- x

instance Semigroup e => Applicative (Accumulator e) where
	f <*> v = Accumulator $ k # run f # run v where
		k ~(e :*: g) ~(e' :*: w) = e + e' :*: g w

instance Monoid e => Pointable (Accumulator e) (->) where
	point = Accumulator . (zero :*:)

instance Semigroup e => Bindable (Accumulator e) (->) where
	f =<< Accumulator (e :*: x) = let e' :*: b = run $ f x in
		Accumulator $ e + e':*: b

type instance Schematic Monad (Accumulator e) = (<.:>) ((:*:) e)

instance Interpreted (Accumulator e) where
	type Primary (Accumulator e) a = e :*: a
	run ~(Accumulator x) = x
	unite = Accumulator

instance Monoid e => Monadic (Accumulator e) where
	wrap = TM . UT . point . run

type Accumulated e t = Adaptable (Accumulator e) t

instance {-# OVERLAPS #-} (Semigroup e, Applicative u) => Applicative ((:*:) e <.:> u) where
	UT f <*> UT x = UT $ k <$> f <*> x where
		k ~(u :*: g) ~(v :*: y) = u + v :*: g y

instance {-# OVERLAPS #-} (Pointable u (->), Monoid e) => Pointable ((:*:) e <.:> u) (->) where
	point = UT . point . (zero :*:)

instance {-# OVERLAPS #-} (Semigroup e, Pointable u (->), Bindable u (->)) => Bindable ((:*:) e <.:> u) (->) where
	f =<< UT x = UT $ (\(acc :*: v) -> (\(acc' :*: y) -> (acc + acc' :*: y)) -<$>- run (f v)) =<< x

gather :: Accumulated e t => e -> t ()
gather x = adapt . Accumulator $ x :*: ()
