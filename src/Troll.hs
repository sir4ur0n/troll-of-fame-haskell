module Troll
  ( Troll(..)
  , QuantityKilled(..)
  , score
  , iGotOne
  , iGot
  , oopsHeSurvived
  , allElvesOfAKindResurrected
  ) where

import Data.Map.Strict as Map
import Data.Maybe
import Elf

data Troll =
  Troll
    { name :: String
    , killList :: Map Elf QuantityKilled
    }
  deriving (Show, Eq)

newtype QuantityKilled =
  QuantityKilled
    { getInteger :: Integer
    }
  deriving (Show, Num, Eq, Ord)

score :: Troll -> Score
score Troll {killList} = Map.foldlWithKey accumulator start killList
  where
    start = 0
    accumulator previousScore elf (QuantityKilled nbKilled) = previousScore + (value elf * nbKilled)

iGotOne :: Elf -> Troll -> Troll
iGotOne = modifyScore (+ 1)

iGot :: QuantityKilled -> Elf -> Troll -> Troll
iGot nbElves = modifyScore (+ nbElves)

oopsHeSurvived :: Elf -> Troll -> Troll
oopsHeSurvived = modifyScore (+ (-1))

allElvesOfAKindResurrected :: Elf -> Troll -> Troll
allElvesOfAKindResurrected elf Troll {..} = Troll {killList = killListWithoutResurrectedElves, ..}
  where
    killListWithoutResurrectedElves = delete elf killList

modifyScore :: (QuantityKilled -> QuantityKilled) -> Elf -> Troll -> Troll
modifyScore modifier elf Troll {..} = Troll {killList = newKillList, ..}
  where
    newKillList
      | nextNbKilled <= 0 = Map.delete elf killList
      | otherwise = Map.insert elf nextNbKilled killList
    currentNbKilled = Map.lookup elf killList
    nextNbKilled = modifier $ fromMaybe 0 currentNbKilled
