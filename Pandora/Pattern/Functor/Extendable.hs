module Pandora.Pattern.Functor.Extendable where

import Pandora.Pattern.Functor.Covariant (Covariant)

infixr 1 <<=========
infixr 2 <<========
infixr 3 <<=======
infixr 4 <<======
infixr 5 <<=====
infixr 6 <<====
infixr 7 <<===
infixr 8 <<==
infixr 9 <<=

{- |
> When providing a new instance, you should ensure it satisfies:
> * Duplication interchange: (f -<-|-|--) . (identity <<=) ≡ (identity <<=) . (f <-|-)
> * Extension interchange: (f <<=) ≡ (f <-|-) . (identity <<=)
-}

class Covariant source source t => Extendable source t where
	(<<=) :: source (t a) b -> source (t a) (t b)
	
	(<<==), (<<===), (<<====), (<<=====), (<<======), (<<=======), (<<========), (<<=========)  :: source (t a) b -> source (t a) (t b)
	(<<=========) = (<<=)
	(<<========) = (<<=)
	(<<=======) = (<<=)
	(<<======) = (<<=)
	(<<=====) = (<<=)
	(<<====) = (<<=)
	(<<===) = (<<=)
	(<<==) = (<<=)
