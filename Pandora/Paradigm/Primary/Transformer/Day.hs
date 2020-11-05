module Pandora.Paradigm.Primary.Transformer.Day where

import Pandora.Pattern ((.|..))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))

data Day t u a = forall b c . Day (t b) (u c) (b -> c -> a)

instance Covariant (Day t u) where
	f <$> Day tb uc g = Day tb uc (f .|.. g)

instance (Pointable t, Pointable u) => Pointable (Day t u) where
	point x = Day (point ()) (point ()) $ \_ _ -> x

instance (Applicative t, Applicative u) => Applicative (Day t u) where
	Day tb uc bcad <*> Day vb wc bca = Day ((:*:) <$> tb <*> vb) ((:*:) <$> uc <*> wc)
		$ \(b :*: b') (c :*: c') -> bcad b c $ bca b' c'

instance (Extractable t, Extractable u) => Extractable (Day t u) where
	extract (Day tb uc bcad) = bcad (extract tb) (extract uc)

instance (Extendable t, Extendable u) => Extendable (Day t u) where
	day@(Day tb uc _) =>> f = Day tb uc (\_ _ -> f day)

instance Extractable t => Lowerable (Day t) where
	lower (Day tb uc bca) = bca (extract tb) <$> uc

instance Hoistable (Day t) where
	hoist g (Day tb uc bca) = Day tb (g uc) bca
