module TrollProp where

import           Elf
import           Generators            ()
import           Test.Tasty.QuickCheck
import           Troll                 as SUT

-- | This is an invariance property: we check that something is always true, no matter the input.
--
-- When migrating unit tests to property based tests, this is usually the easier type of property to get started. Look for any unit test you have where some values are "pretty much random" and replace them with invariance tests where these "random values" are actually injected by the library. The benefit is twofold: you make it explicit to the reader that some values are irrelevant, and the PBT library will actually check many values to ensure the value is irrelevant ;-)
test_invariance = testProperty "A troll hunting score can never be negative" $ \troll -> SUT.score troll >= 0

-- | This is an inverse property: we check that doing something and then undoing it brings back to the original state
--
-- This type of property is particularly useful when you have code that converts from a model to another (e.g. from your REST API to your domain model, or from your domain model to your DAO model) because you need to check that converting back and forth should bring back to the original value, i.e. your code does not "lose" information in the process!
test_inverse =
  testProperty "A troll killing an elf and then realizing the elf actually survived remains unchanged" $ \troll elf ->
    let trollWithAdditionalElfKilled = SUT.iGotOne elf troll
        trollRealizingHeMissedTheElf = SUT.oopsHeSurvived elf trollWithAdditionalElfKilled
     in trollRealizingHeMissedTheElf == troll

-- | This is an analogous property: we check that there are several ways from a starting value to reach and end value.
--
-- This is useful for consistency between functions! E.g. `x + x` should always be equal to `x * 2`, so we can say that these 2 functions are analogous.
--
-- Another use case for analogous properties are refactoring: instead of modifying an existing function `foo`, copy its implementation to `foo2`, then modify `foo2`, and eventually check that no matter the input `x`, `foo x == foo2 x`, which ensures we did not bring any regression during refactoring! Then you can safely remove `foo` and rename `foo2` to `foo`
test_analogy =
  testProperty "Killing N elves one by one is the same as killing N elves in a single strike" $ \troll elf (Positive nbElves) ->
    let oneByOne = replicate (fromInteger $ SUT.getInteger nbElves) elf
        trollKillingOneByOne = foldl (flip SUT.iGotOne) troll oneByOne
        trollKillingInSingleStrike = SUT.iGot nbElves elf troll
     in trollKillingOneByOne == trollKillingInSingleStrike

-- | This is an idempotence property: we check that applying a function once or many times to the successive results should lead to the same value; e.g. `f` is idempotent if `f x == f (f x) == f (f (f x)) == ...`
--
-- This is particularly useful for functions that make the input converge to a stable output, e.g. functions that cleanup form inputs (date format, put name in upper case, etc.)
test_idempotent =
  testProperty "Resurrected elves cannot be resurrected again" $ \troll elf ->
    let resurrectedOnce = SUT.allElvesOfAKindResurrected elf troll
        resurrectedTwice = SUT.allElvesOfAKindResurrected elf resurrectedOnce
     in resurrectedOnce == resurrectedTwice

-- | This is a metamorphic property: we run a function over an input and a modified version of the same input, and we check some property holds on the modified result (metamorphed sounds much cooler!).
--
-- This is pretty neat when you can tell something about your function result depending on how the input varies.
test_metamorphic =
  testProperty "When a troll kills an elf, his score should increase" $ \troll elf ->
    let scoreBefore = SUT.score troll
        trollAfterKill = SUT.iGotOne elf troll
        scoreAfter = SUT.score trollAfterKill
     in scoreAfter == scoreBefore + value elf

-- | This is an injective property: we check that different inputs must yield different results.
--
-- This is useful whenever you need to ensure an output can only be reached by a single input; e.g. a hash function, or a function that takes a person and returns its Social Security Number (imagine if 2 persons had the same SSN!)
test_injective =
  testProperty "When killing different elves, a troll should get different scores" $ \troll elf1 elf2 ->
    elf1 /=
    elf2 ==>
    let trollKilledElf1 = SUT.iGotOne elf1 troll
        trollKilledElf2 = SUT.iGotOne elf2 troll
     in trollKilledElf1 /= trollKilledElf2
