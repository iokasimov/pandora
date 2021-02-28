module Pandora.Paradigm.Primary.Object.Denumerator where

import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Equal, Greater))

data Denumerator = One | Denumerator Denumerator

instance Setoid Denumerator where
	One == One = True
	Denumerator n == Denumerator m = n == m
	_ == _ = False

instance Chain Denumerator where
	One <=> One = Equal
	One <=> Denumerator _ = Less
	Denumerator _ <=> One = Greater
	Denumerator n <=> Denumerator m = n <=> m

instance Semigroup Denumerator where
	One + m = Denumerator m
	Denumerator n + m = Denumerator (n + m)

instance Ringoid Denumerator where
	One * n = n
	Denumerator n * m = m + n * m
