module Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (..)) where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Paradigm.Primary.Transformer.Backwards (Backwards (Backwards))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype Reverse t a = Reverse (t a)

instance Covariant t => Covariant (Reverse t) where
	f <$> Reverse x = Reverse $ f <$> x

instance Pointable t => Pointable (Reverse t) where
	point = Reverse . point

instance Extractable t => Extractable (Reverse t) where
	extract (Reverse x) = extract x

instance Applicative t => Applicative (Reverse t) where
	Reverse f <*> Reverse x = Reverse (f <*> x)

instance Traversable t => Traversable (Reverse t) where
	Reverse x ->> f = Reverse <$> run (x ->> Backwards . f)

instance Distributive t => Distributive (Reverse t) where
	x >>- f = Reverse $ x >>- run . f

instance Contravariant t => Contravariant (Reverse t) where
	f >$< Reverse x = Reverse $ f >$< x

instance Interpreted (Reverse t) where
	type Primary (Reverse t) a = t a
	run (Reverse x) = x

instance Liftable Reverse where
	lift = Reverse

instance Lowerable Reverse where
	lower = run

instance Hoistable Reverse where
	hoist f (Reverse x) = Reverse $ f x
