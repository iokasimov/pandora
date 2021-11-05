{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Pattern.Functor.Covariant where

import Pandora.Pattern.Semigroupoid (Semigroupoid)

infixl 4 <-|-, <$>
infixl 3 <-|-|-, <$$>
infixl 4 <-|-|-|-, <$$$>

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: (identity <-|-) ≡ identity
> * Interpreted of morphisms: (f . g <-|-) ≡ (f <-|-) . (g <-|-)
-}

class (Semigroupoid source, Semigroupoid target) => Covariant source target t where
	(<-|-) :: source a b -> target (t a) (t b)

(<-|-|-) :: forall source between target t u a b
	. (Covariant source between u, Covariant between target t)
	=> source a b -> target (t (u a)) (t (u b))
(<-|-|-) s = ((<-|-) ((<-|-) @source @between @u s))

(<-|-|-|-) :: forall source between1 between2 target t u v a b
	. (Covariant source between1 v, Covariant between1 between2 u, Covariant between2 target t)
	=> source a b -> target (t (u (v a))) (t (u (v b)))
(<-|-|-|-) s = ((<-|-) @between2 @target ((<-|-) @between1 @between2 @u ((<-|-) @source @between1 @v s)))

(<$$$$>) :: forall source between1 between2 between3 target t u v w a b
	. (Covariant source between1 w, Covariant between1 between2 v, Covariant between2 between3 u, Covariant between3 target t)
	=> source a b -> target (t (u (v (w a)))) (t (u (v (w b))))
(<$$$$>) s = ((<-|-) @between3 @target @t ((<-|-) @between2 @between3 @u ((<-|-) @between1 @between2 @v ((<-|-) @source @between1 @w s))))

(<$>) :: Covariant source target t => source a b -> target (t a) (t b)
(<$>) = (<-|-)

(<$$>) :: forall source between target t u a b
	. (Covariant source between u, Covariant between target t)
	=> source a b -> target (t (u a)) (t (u b))
(<$$>) = (<-|-|-) @source @between @target

(<$$$>) :: forall source between1 between2 target t u v a b
	. (Covariant source between1 v, Covariant between1 between2 u, Covariant between2 target t)
	=> source a b -> target (t (u (v a))) (t (u (v b)))
(<$$$>) = (<-|-|-|-) @source @between1 @between2 @target
