module Pandora.Paradigm.Controlflow.Effect.Interpreted where

import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), (<$$>), (<$$$>), (<$$$$>))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()

infixr 2 ||=, =||

type family Schematic (c :: (* -> * -> *) -> (* -> *) -> k) (t :: * -> *) = (r :: (* -> *) -> * -> *) | r -> t

class Interpreted m t where
	{-# MINIMAL run, unite #-}
	type Primary t a :: *
	run :: m (t a) (Primary t a)
	unite :: m (Primary t a) (t a)

	(||=) :: (Semigroupoid m, Interpreted m u) => m (Primary t a) (Primary u b) -> m (t a) (u b)
	(||=) f = unite . f . run

	(=||) :: (Semigroupoid m, Interpreted m u) => m (t a) (u b) -> m (Primary t a) (Primary u b)
	(=||) f = run . f . unite

	(<$||=) :: (Semigroupoid m, Covariant m m j, Interpreted m u)
                => m (Primary t a) (Primary u b) -> m (j := t a) (j := u b)
	(<$||=) f = (<$>) ((||=) f)

	(<$$||=) :: (Semigroupoid m, Covariant m m j, Covariant m m k, Interpreted m u)
		=> m (Primary t a) (Primary u b) -> m (j :. k := t a) (j :. k := u b)
	(<$$||=) f = (<$$>) @m @m ((||=) f)

	(<$$$||=) :: (Semigroupoid m, Covariant m m j, Covariant m m k, Covariant m m l, Interpreted m u)
		=> m (Primary t a) (Primary u b) -> m (j :. k :. l := t a) (j :. k :. l := u b)
	(<$$$||=) f = (<$$$>) @m @m @m ((||=) f)

	(<$$$$||=) :: (Semigroupoid m, Covariant m m j, Covariant m m k, Covariant m m l, Covariant m m n, Interpreted m u)
		=> m (Primary t a) (Primary u b) -> m (j :. k :. l :. n := t a) (j :. k :. l :. n := u b)
	(<$$$$||=) f = (<$$$$>) @m @m @m @m ((||=) f)

	(=||$>) :: (Covariant m m j, Interpreted m u)
		=> m (t a) (u b) -> m (j := Primary t a) (j := Primary u b)
	(=||$>) f = (<$>) ((=||) f)

	(=||$$>) :: (Covariant m m j, Covariant m m k, Interpreted m u)
		=> m (t a) (u b) -> m (j :. k := Primary t a) (j :. k := Primary u b)
	(=||$$>) f = (<$$>) @m @m ((=||) f)

	(=||$$$>) :: (Covariant m m j, Covariant m m k, Covariant m m l, Interpreted m u)
		=> m (t a) (u b) -> m (j :. k :. l := Primary t a) (j :. k :. l := Primary u b)
	(=||$$$>) f = (<$$$>) @m @m @m ((=||) f)

	(=||$$$$>) :: (Covariant m m j, Covariant m m k, Covariant m m l, Covariant m m n, Interpreted m u)
		=> m (t a) (u b) -> m (j :. k :. l :. n := Primary t a) (j :. k :. l :. n := Primary u b)
	(=||$$$$>) f = (<$$$$>) @m @m @m @m ((=||) f)

(-=:) :: (Liftable m t, Interpreted m (t u), Interpreted m (t v), Covariant m m u)
	=> m (t u a) (t v b) -> m (u a) (Primary (t v) b)
(-=:) f = run . f . lift

instance Interpreted (->) (Flip v a) where
	type Primary (Flip v a) e = v e a
	run ~(Flip x) = x
	unite = Flip

instance Interpreted (->) (Straight v e) where
	type Primary (Straight v e) a = v e a
	run ~(Straight x) = x
	unite = Straight
