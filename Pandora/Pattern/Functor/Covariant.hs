module Pandora.Pattern.Functor.Covariant where

import Pandora.Pattern.Semigroupoid (Semigroupoid)

infixl 4 <$>
infixl 3 -<<$$>-, -<$$>>-

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: (identity <$>) ≡ identity
> * Interpreted of morphisms: (f . g <$>) ≡ (f <$>) . (g <$>)
-}

class (Semigroupoid source, Semigroupoid target) => Covariant source target t where
	(<$>) :: source a b -> target (t a) (t b)
	
(-<$$>-) :: forall t u category a b 
	. (Covariant category category u, Covariant category category t) 
	=> category a b -> category (t (u a)) (t (u b))
(-<$$>-) s = ((<$>) ((<$>) @category @category @u s))

(-<<$$>-) :: forall t u source target a b 
	. (Covariant source source u, Covariant source target t) 
	=> source a b -> target (t (u a)) (t (u b))
(-<<$$>-) s = ((<$>) ((<$>) @source @source @u s))

(-<$$>>-) :: forall source target t u a b 
	. (Covariant source target u, Covariant target target t) 
	=> source a b -> target (t (u a)) (t (u b))
(-<$$>>-) s = ((<$>) ((<$>) @source @target @u s))

-- TODO: Figure out how to work with hidden type variables
-- to put intermediate category `between`

(-<$$$>-) :: forall t u v category a b
	. (Covariant category category t, Covariant category category u, Covariant category category v) 
	=> category a b -> category (t (u (v a))) (t (u (v b)))
(-<$$$>-) s = ((<$>) @category @category @t ((<$>) @category @category @u ((<$>) @category @category @v s)))

(-<$$$$>-) :: forall category t u v w a b
	. (Covariant category category t, Covariant category category u, Covariant category category v, Covariant category category w) 
	=> category a b -> category (t (u (v (w a)))) (t (u (v (w b))))
(-<$$$$>-) s = ((<$>) @category @category @t ((<$>) @category @category @u ((<$>) @category @category @v ((<$>) @category @category @w s))))
