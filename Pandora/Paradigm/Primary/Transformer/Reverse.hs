{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Reverse where

import Pandora.Pattern.Category ((.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative_ (multiply))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Functor.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Transformer.Backwards (Backwards (Backwards))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype Reverse t a = Reverse (t a)

instance Covariant t => Covariant (Reverse t) where
	f <$> Reverse x = Reverse # f <$> x

instance Covariant_ t (->) (->) => Covariant_ (Reverse t) (->) (->) where
	f -<$>- Reverse x = Reverse # f -<$>- x

instance Pointable t (->) => Pointable (Reverse t) (->) where
	point = Reverse . point

instance Extractable t (->) => Extractable (Reverse t) (->) where
	extract (Reverse x) = extract x

instance Applicative_ t (:*:) (->) (->) => Applicative_ (Reverse t) (:*:) (->) (->) where
	multiply f (Reverse x :*: Reverse y) = Reverse . multiply f $ x :*: y

instance Traversable t => Traversable (Reverse t) where
	Reverse x ->> f = Reverse <$> run (x ->> Backwards . f)

instance Distributive t (->) (->) => Distributive (Reverse t) (->) (->) where
	f -<< x = Reverse $ run . f -<< x

instance Contravariant t => Contravariant (Reverse t) where
	f >$< Reverse x = Reverse # f >$< x

instance Interpreted (Reverse t) where
	type Primary (Reverse t) a = t a
	run ~(Reverse x) = x
	unite = Reverse

instance Liftable Reverse where
	lift = Reverse

instance Lowerable Reverse where
	lower = run

instance Hoistable Reverse where
	f /|\ Reverse x = Reverse # f x
