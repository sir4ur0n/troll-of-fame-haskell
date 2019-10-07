module TrollTest where

import qualified Data.Map.Strict as Map
import Elf
import Test.Tasty.HUnit
import Troll as SUT

test_iGotOneHighArcher =
  testCase "Killing a High Archer is worth 4 points" $
  let aklassBefore = Troll {name = "Aklass", killList = mempty}
      highArcher = Elf {race = High, role = Archer}
      aklassAfter = SUT.iGotOne highArcher aklassBefore
   in SUT.score aklassAfter @?= 4

test_iGotOneHighPriest =
  testCase "Killing a High Priest is worth 10 points" $
  let aklassBefore = Troll {name = "Aklass", killList = mempty}
      highPriest = Elf {race = High, role = Priest}
      aklassAfter = SUT.iGotOne highPriest aklassBefore
   in SUT.score aklassAfter @?= 10

test_iGotOneDarkSwordsman =
  testCase "Killing a Dark Swordsman is worth 1 points" $
  let aklassBefore = Troll {name = "Aklass", killList = mempty}
      darkSwordsman = Elf {race = Dark, role = Swordsman}
      aklassAfter = SUT.iGotOne darkSwordsman aklassBefore
   in SUT.score aklassAfter @?= 1

test_iGotOneDarkWarlock =
  testCase "Killing a Dark Warlock is worth 4 points" $
  let aklassBefore = Troll {name = "Aklass", killList = mempty}
      darkWarlock = Elf {race = Dark, role = Warlock}
      aklassAfter = SUT.iGotOne darkWarlock aklassBefore
   in SUT.score aklassAfter @?= 4

test_oopsHeSurvivedDarkWarlock =
  testCase "When a Dark Warlock survives, then he is removed from the troll kill list" $
  let darkWarlock = Elf {race = Dark, role = Warlock}
      aklassBefore = Troll {name = "Aklass", killList = Map.fromList [(darkWarlock, 3)]}
      aklassAfter = SUT.oopsHeSurvived darkWarlock aklassBefore
   in aklassAfter @?= Troll {name = "Aklass", killList = Map.fromList [(darkWarlock, 2)]}

test_allElvesOfAKindResurrected =
  testCase "When Dark Swordsmen are resurrected, then they are removed from troll kill lists" $
  let darkSwordsman = Elf {race = Dark, role = Swordsman}
      highPriest = Elf {race = High, role = Priest}
      killList = Map.fromList [(darkSwordsman, 3), (highPriest, 42)]
      aklassBefore = Troll {name = "Aklass", killList}
      aklassAfter = SUT.allElvesOfAKindResurrected darkSwordsman aklassBefore
   in SUT.score aklassAfter @?= 420
