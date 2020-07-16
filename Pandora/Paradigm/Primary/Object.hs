{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Object (module Exports) where

import Pandora.Paradigm.Primary.Object.Ordering as Exports
import Pandora.Paradigm.Primary.Object.Boolean as Exports

import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))

instance Setoid Boolean where
	True == True = True
	False == False = True
	_ == _ = False

instance Chain Boolean where
	True <=> True = Equal
	True <=> False = Greater
	False <=> True = Less
	False <=> False = Equal
