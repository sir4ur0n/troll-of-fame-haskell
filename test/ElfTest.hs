module ElfTest where

import           Elf
import           Test.Tasty.HUnit

test_darkWarlock =
  testCase "A High Warlock is worth 8 points" $
  let doomShadow = Elf {race = High, role = Warlock}
   in value doomShadow @?= 8

test_darkArcherSameValueAsHighSwordsman =
  testCase "A Dark Archer is worth the same as a High Swordsman" $
  let faeor = Elf {race = High, role = Swordsman}
      doomShadow = Elf {race = Dark, role = Archer}
   in value faeor @?= value doomShadow
