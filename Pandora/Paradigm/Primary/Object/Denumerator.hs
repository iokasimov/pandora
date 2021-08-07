module Pandora.Paradigm.Primary.Object.Denumerator where

import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Equal, Greater))

data Denumerator = Single | Denumerator Denumerator

instance Setoid Denumerator where
	Single == Single = True
	Denumerator n == Denumerator m = n == m
	_ == _ = False

instance Chain Denumerator where
	Single <=> Single = Equal
	Single <=> Denumerator _ = Less
	Denumerator _ <=> Single = Greater
	Denumerator n <=> Denumerator m = n <=> m

instance Semigroup Denumerator where
	Single + m = Denumerator m
	Denumerator n + m = Denumerator (n + m)

instance Ringoid Denumerator where
	Single * n = n
	Denumerator n * m = m + n * m
