module Pandora.Paradigm.Primary.Object.Numerator where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Equal, Greater))

data Numerator = Zero | Numerator Numerator

instance Setoid Numerator where
	Zero == Zero = True
	Numerator n == Numerator m = n == m
	_ == _ = False

instance Chain Numerator where
	Zero <=> Zero = Equal
	Zero <=> Numerator _ = Less
	Numerator _ <=> Zero = Greater
	Numerator n <=> Numerator m = n <=> m

instance Semigroup Numerator where
	Zero + m = m
	Numerator n + m = Numerator $ n + m

instance Ringoid Numerator where
	Zero * _ = Zero
	Numerator n * m = m + n * m

instance Monoid Numerator where
	zero = Zero

instance Quasiring Numerator where
	one = Numerator Zero
