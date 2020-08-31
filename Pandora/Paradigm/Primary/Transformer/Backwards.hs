module Pandora.Paradigm.Primary.Transformer.Backwards where

import Pandora.Core.Morphism ((&))
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
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run))

newtype Backwards t a = Backwards (t a)

instance Covariant t => Covariant (Backwards t) where
	f <$> Backwards x = Backwards $ f <$> x

instance Pointable t => Pointable (Backwards t) where
	point = Backwards . point

instance Extractable t => Extractable (Backwards t) where
	extract (Backwards x) = extract x

instance Applicative t => Applicative (Backwards t) where
	Backwards f <*> Backwards x = Backwards ((&) <$> x <*> f)

instance Traversable t => Traversable (Backwards t) where
	Backwards x ->> f = Backwards <$> x ->> f

instance Distributive t => Distributive (Backwards t) where
	x >>- f = Backwards $ x >>- run . f

instance Contravariant t => Contravariant (Backwards t) where
	f >$< Backwards x = Backwards $ f >$< x

instance Interpreted (Backwards t) where
	type Primary (Backwards t) a = t a
	run (Backwards x) = x

instance Liftable Backwards where
	lift = Backwards

instance Lowerable Backwards where
	lower = run

instance Hoistable Backwards where
	hoist f (Backwards x) = Backwards $ f x
