module Generators where

import Elf
import Test.Tasty.QuickCheck
import Troll

instance Arbitrary Troll where
  arbitrary = do
    name <- arbitrary
    killList <- arbitrary
    return Troll {..}

instance Arbitrary QuantityKilled where
  arbitrary = do
    Positive nbKilled <- arbitrary
    return $ QuantityKilled nbKilled

instance Arbitrary Elf where
  arbitrary = do
    race <- arbitrary
    role <- arbitrary
    return Elf {..}

instance Arbitrary Race where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Role where
  arbitrary = arbitraryBoundedEnum
