module Pandora.Paradigm.Controlflow.Effect.Interpreted where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>), (<$$$>), (<$$$$>)), Covariant_)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Primary.Functor.Function ()

infixr 2 ||=, =||

type family Schematic (c :: (* -> *) -> k) (t :: * -> *) = (r :: (* -> *) -> * -> *) | r -> t

class Interpreted t where
	{-# MINIMAL run, unite #-}
	type Primary t a :: *
	run :: t a -> Primary t a
	unite :: Primary t a -> t a

	(||=) :: Interpreted u => (Primary t a -> Primary u b) -> t a -> u b
	(||=) f = unite . f . run

	(=||) :: Interpreted u => (t a -> u b) -> Primary t a -> Primary u b
	(=||) f = run . f . unite

	(<$||=) :: (Covariant j, Interpreted u)
		=> (Primary t a -> Primary u b) -> j := t a -> j := u b
	f <$||= x = (f ||=) <$> x

	(<$$||=) :: (Covariant j, Covariant k, Interpreted u)
		=> (Primary t a -> Primary u b) -> j :. k := t a -> j :. k := u b
	f <$$||= x = (f ||=) <$$> x

	(<$$$||=) :: (Covariant j, Covariant k, Covariant l, Interpreted u)
		=> (Primary t a -> Primary u b) -> j :. k :. l := t a -> j :. k :. l := u b
	f <$$$||= x = (f ||=) <$$$> x

	(<$$$$||=) :: (Covariant j, Covariant k, Covariant l, Covariant m, Interpreted u)
		=> (Primary t a -> Primary u b) -> j :. k :. l :. m := t a -> j :. k :. l :. m := u b
	f <$$$$||= x = (f ||=) <$$$$> x

	(=||$>) :: (Covariant j, Interpreted u)
		=> (t a -> u b) -> j := Primary t a -> j := Primary u b
	f =||$> x = (f =||) <$> x

	(=||$$>) :: (Covariant j, Covariant k, Interpreted u)
		=> (t a -> u b) -> j :. k := Primary t a -> j :. k := Primary u b
	f =||$$> x = (f =||) <$$> x

	(=||$$$>) :: (Covariant j, Covariant k, Covariant l, Interpreted u)
		=> (t a -> u b) -> j :. k :. l := Primary t a -> j :. k :. l := Primary u b
	f =||$$$> x = (f =||) <$$$> x

	(=||$$$$>) :: (Covariant j, Covariant k, Covariant l, Covariant m, Interpreted u)
		=> (t a -> u b) -> j :. k :. l :. m := Primary t a -> j :. k :. l :. m := Primary u b
	f =||$$$$> x = (f =||) <$$$$> x

(-=:) :: (Liftable t, Interpreted (t u), Interpreted (t v), Covariant u, Covariant_ u (->) (->))
	=> (t u a -> t v b) -> u a -> Primary (t v) b
(-=:) f = run . f . lift
