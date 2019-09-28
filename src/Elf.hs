module Elf where

data Elf =
  Elf
    { race :: Race
    , role :: Role
    }
  deriving (Eq, Show, Ord)

data Race
  = High
  | Dark
  deriving (Eq, Show, Ord, Bounded, Enum)

data Role
  = Archer
  | Swordsman
  | Priest
  | Warlock
  deriving (Eq, Show, Ord, Bounded, Enum)

value :: Elf -> Score
value Elf {..} = multiplierPerRace race * valuePerRole role

-- | Because everybody hates those smug High Elves
multiplierPerRace :: Race -> Multiplier
multiplierPerRace High = 2
multiplierPerRace Dark = 1

valuePerRole :: Role -> BaseValue
valuePerRole Archer = 3
valuePerRole Swordsman = 1
valuePerRole Priest = 5
valuePerRole Warlock = 4

type Score = Integer

type Multiplier = Integer

type BaseValue = Integer
