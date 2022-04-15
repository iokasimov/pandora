module Pandora.Core.Interpreted where

import Pandora.Core.Functor (type (<), type (>))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))

infixr 2 =#-, -#=

infixl 1 <~~~~~~~~
infixl 2 <~~~~~~~
infixl 3 <~~~~~~
infixl 4 <~~~~~
infixl 5 <~~~~
infixl 6 <~~~
infixl 7 <~~
infixl 8 <~

type family Schematic (c :: (* -> * -> *) -> (* -> *) -> k) (t :: * -> *) = (r :: (* -> *) -> * -> *) | r -> t

-- TODO: Move this typeclass to Core module
class Interpreted m t where
	{-# MINIMAL run, unite #-}
	type Primary t a :: *
	run :: m < t a < Primary t a
	unite :: m < Primary t a < t a

	(<~~~~~~~~), (<~~~~~~~), (<~~~~~~), (<~~~~~), (<~~~~), (<~~~), (<~~), (<~) :: m < t a < Primary t a
	(<~~~~~~~~) = run
	(<~~~~~~~) = run
	(<~~~~~~) = run
	(<~~~~~) = run
	(<~~~~) = run
	(<~~~) = run
	(<~~) = run
	(<~) = run

	(=#-) :: (Semigroupoid m, Interpreted m u) => m < Primary t a < Primary u b -> m < t a < u b
	(=#-) f = unite . f . run

	(-#=) :: (Semigroupoid m, Interpreted m u) => m < t a < u b -> m < Primary t a < Primary u b
	(-#=) f = run . f . unite

	(<$=#-) :: (Semigroupoid m, Covariant m m j, Interpreted m u)
                => m < Primary t a < Primary u b -> m (j > t a) (j > u b)
	(<$=#-) f = (<-|-) ((=#-) f)

	--(<$$=#-) :: (Semigroupoid m, Covariant m m j, Covariant m m k, Interpreted m u)
	--	=> m (Primary t a) (Primary u b) -> m (j :. k > t a) (j :. k > u b)
	--(<$$=#-) f = (<$$>) @m @m ((=#-) f)

	--(<$$$=#-) :: (Semigroupoid m, Covariant m m j, Covariant m m k, Covariant m m l, Interpreted m u)
	--	=> m (Primary t a) (Primary u b) -> m (j :. k :. l > t a) (j :. k :. l > u b)
	--(-<$$$=#-) f = (<$$$>) @m @m @m ((=#-) f)

	(-#=$>) :: (Covariant m m j, Interpreted m u)
		=> m < t a < u b -> m (j > Primary t a) (j > Primary u b)
	(-#=$>) f = (<-|-) ((-#=) f)

	--(-#=$$>) :: (Covariant m m j, Covariant m m k, Interpreted m u)
	--	=> m (t a) (u b) -> m (j :. k > Primary t a) (j :. k > Primary u b)
	--(-#=$$>) f = (<$$>) @m @m ((-#=) f)

	--(-#=$$$>) :: (Covariant m m j, Covariant m m k, Covariant m m l, Interpreted m u)
	--	=> m (t a) (u b) -> m (j :. k :. l > Primary t a) (j :. k :. l > Primary u b)
	--(-#=$$$>) f = (<$$$>) @m @m @m ((-#=) f)

(-=:) :: (Liftable m t, Interpreted m > t u, Interpreted m > t v, Covariant m m u)
	=> m < t u a < t v b -> m < u a < Primary (t v) b
(-=:) f = run . f . lift

instance Interpreted (->) (Flip v a) where
	type Primary (Flip v a) e = v e a
	run ~(Flip x) = x
	unite = Flip

instance Interpreted (->) (Straight v e) where
	type Primary (Straight v e) a = v e a
	run ~(Straight x) = x
	unite = Straight
