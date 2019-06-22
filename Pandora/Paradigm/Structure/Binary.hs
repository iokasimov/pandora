module Pandora.Paradigm.Structure.Binary (Binary, insert) where

import Pandora.Core.Morphism ((&))
import Pandora.Paradigm.Basis.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Basis.Twister (Twister ((:<)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), order)

type Binary = Twister Wye

insert :: Chain a => a -> Binary a -> Binary a
insert x (y :< End) = x <=> y & order (y :< Right (x :< End)) (y :< Right (x :< End)) (y :< Left (x :< End))
insert x (y :< Left ls) = x <=> y & order (y :< Both ls (x :< End)) (y :< Both ls (x :< End)) (y :< Left (insert x ls))
insert x (y :< Right rs) = x <=> y & order (y :< Right (insert x rs)) (y :< Right (insert x rs)) (y :< Both (x :< End) rs)
insert x (y :< Both ls rs) = x <=> y & order (y :< Both ls (insert x rs)) (y :< Both ls (insert x rs)) (y :< Both (insert x ls) rs)
