module Pandora.Paradigm.Primary.Transformer.Jack where

import Pandora.Pattern.Category (identity, (.), ($), ($:))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-), distribute))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Primary.Functor.Function ()
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))

data Jack t a = It a | Other (t a)

instance Covariant t => Covariant (Jack t) where
	f <$> It x = It $ f x
	f <$> Other y = Other $ f <$> y

instance Covariant t => Pointable (Jack t) where
	point = It

instance Alternative t => Alternative (Jack t) where
	It x <+> _ = It x
	Other _ <+> It y = It y
	Other x <+> Other y = Other $: x <+> y

instance Avoidable t => Avoidable (Jack t) where
	empty = Other empty

instance Extractable t => Extractable (Jack t) where
	extract (It x) = x
	extract (Other y) = extract y

instance Applicative t => Applicative (Jack t) where
	It f <*> It x = It $ f x
	It f <*> Other y = Other $ f <$> y
	Other f <*> It x = Other $ ($ x) <$> f
	Other f <*> Other y = Other $ f <*> y

instance Traversable t => Traversable (Jack t) where
	It x ->> f = It <$> f x
	Other y ->> f = Other <$> y ->> f

instance Distributive t => Distributive (Jack t) where
	x >>- f = distribute $ f <$> x

instance (Pointable t, Bindable t) => Bindable (Jack t) where
	It x >>= f = f x
	Other x >>= f = Other $ x >>= jack point identity . f

instance Extendable t => Extendable (Jack t) where
	It x =>> f = It . f $ It x
	Other x =>> f = Other $ x =>> f . Other

instance Liftable Jack where
	lift = Other

instance Hoistable Jack where
	hoist _ (It x) = It x
	hoist f (Other x) = Other $ f x

instance (Setoid a, Setoid (t a)) => Setoid (Jack t a) where
	It x == It y = x == y
	Other x == Other y = x == y
	_ == _ = False

instance (Chain a, Chain (t a)) => Chain (Jack t a) where
	It _ <=> Other _ = Less
	Other _ <=> It _ = Greater
	It x <=> It y = x <=> y
	Other x <=> Other y = x <=> y

jack :: (a -> r) -> (t a -> r) -> Jack t a -> r
jack f _ (It x) = f x
jack _ g (Other y) = g y
