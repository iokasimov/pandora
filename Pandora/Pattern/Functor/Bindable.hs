module Pandora.Pattern.Functor.Bindable where

import Pandora.Pattern.Functor.Covariant (Covariant)

infixr 1 =========<<
infixr 2 ========<<
infixr 3 =======<<
infixr 4 ======<<
infixr 5 =====<<
infixr 6 ====<<
infixr 7 ===<<
infixr 8 ==<<
infixr 9 =<<

{- |
> When providing a new instance, you should ensure it satisfies :
> * Interchange: t >>= f = join (f <-|- t)
-}

class Covariant source source t => Bindable source t where
	(=<<) :: source a (t b) -> source (t a) (t b)

	(==<<), (===<<), (====<<), (=====<<), (======<<),
		(=======<<), (========<<), (=========<<)  :: source a (t b) -> source (t a) (t b)
	(==<<) = (=<<)
	(===<<) = (=<<)
	(====<<) = (=<<)
	(=====<<) = (=<<)
	(======<<) = (=<<)
	(=======<<) = (=<<)
	(========<<) = (=<<)
	(=========<<) = (=<<)
