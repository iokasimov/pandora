module Pandora.Pattern.Category (Category (..)) where

import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))

infixl 1 <---------
infixl 2 <--------, #
infixl 3 <-------
infixl 4 <------
infixl 5 <-----
infixl 6 <----
infixl 7 <---
infixl 8 <--

infixr 1 --------->
infixr 2 -------->
infixr 3 ------->
infixr 4 ------>
infixr 5 ----->
infixr 6 ---->
infixr 7 --->
infixr 8 -->

{- |
> When providing a new instance, you should ensure it satisfies:
> * Left identity: identity . f ≡ f
> * Right identity: f . identity ≡ f
-}

class Semigroupoid m => Category m where
	identity :: m a a

	(#) :: m (m a b) (m a b)
	(#) = identity . identity

	(<---------), (<--------), (<-------), (<------), (<-----), (<----), (<---), (<--) :: m (m a b) (m a b)
	(<---------) = identity . identity
	(<--------) = identity . identity
	(<-------) = identity . identity
	(<------) = identity . identity
	(<-----) = identity . identity
	(<----) = identity . identity
	(<---) = identity . identity
	(<--) = identity . identity

	(--------->), (-------->), (------->), (------>), (----->), (---->), (--->), (-->) :: m (m a b) (m a b)
	(--------->) = identity . identity
	(-------->) = identity . identity
	(------->) = identity . identity
	(------>) = identity . identity
	(----->) = identity . identity
	(---->) = identity . identity
	(--->) = identity . identity
	(-->) = identity . identity
