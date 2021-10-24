{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Jack where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (point)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Greater))

data Jack t a = It a | Other (t a)

instance Covariant (->) (->) t => Covariant (->) (->) (Jack t) where
	f <$> It x = It $ f x
	f <$> Other y = Other $ f <$> y

instance Traversable (->) (->) t => Traversable (->) (->) (Jack t) where
	f <<- It x = It <$> f x
	f <<- Other y = Other <$> f <<- y

instance (Monoidal (-->) (-->) (:*:) (:*:) t, Bindable (->) t) => Bindable (->) (Jack t) where
	f =<< It x = f x
	f =<< Other x = Other $ jack point identity . f =<< x

instance Extendable (->) t => Extendable (->) (Jack t) where
	f <<= It x = It . f $ It x
	f <<= Other x = Other $ f . Other <<= x

instance Liftable (->) Jack where
	lift = Other

instance Hoistable Jack where
	_ /|\ It x = It x
	f /|\ Other x = Other $ f x

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
