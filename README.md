# Troll of Fame - Haskell training

## Getting started
You need to have [Stack](https://docs.haskellstack.org/en/stable/README/) installed on your computer, then:
```
stack test
```

## Once upon a time...

The King of Trolls Gnonpom coded the **Troll of Fame**: a wonderful application that would help Trolls learning numbers when they are hunting.
Gnonpom was a skilled Test Driven Developer who had just released **Troll of Fame** where all tests passed.

Unfortunately he was shot by a disgusting Elf.

Here come a new King, Hurrah for the great Troll Aklass!

This time it's for real, let the elf hunting contest begin!

At the end of each battle, the trolls want to compare the number and attributes of the slain elves. And with **Troll of Fame** it should be easy... Should.

## Exercises

### Work with legacy code

You inherit an application that seems to work fine. Run `stack test` (•̀ᴗ•́)و

Read [TrollTest](./test/TrollTest.hs) and [ElfTest](./test/ElfTest.hs) as a first specification of the software.

Now uncomment the content of [ElfProp](./test/ElfProp.hs) and run tests again... Oops, seems like our unit tests were not so complete. (╥﹏╥) This is a limitation of unit tests: you don't test many values, and they are never truly random.

Now fix the implementation bug (replace `+` with `*` in the Elf value calculation) and check all tests now pass.

We will try to improve the quality of _Troll of Frame_ thanks to Property Based Testing (PBT)!

### Property testing
Property Based Testing (a.k.a. PBT) is about generating tests instead of manually writing them. Unlike unit tests where you know what goes in and what comes out (a.k.a. oracle tests), you assess properties that should always be true. The PBT library checks for arbitrary inputs that the property is true.

In Haskell, we use [QuickCheck](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html) library to write and run Property Based tests.

#### Step 1 - Configuration and Invariance
- For a simpler start, we already configured the build dependencies and created generators for `Elf` and `Troll` in the `Generators` module
- Create a Property Based Test file `TrollProp.hs`:
```haskell
module TrollProp where

import Elf
import Generators ()
import Test.Tasty.QuickCheck
import Troll as SUT

test_invariance = testProperty "A troll hunting score can never be negative" $ \troll -> -- ...
```
- Did you notice the property test takes a `Troll` as input? That's where PBT shines! The library will run this test 100 times by default, and each time will pass a random `Troll` to it. We no longer care about building input data!
- The first property test we will write aims to assess **Invariance property**: it means a property should always be true even if the input varies (e.g. the `Troll`)
- As an example, no matter the troll, his score is always >= 0 (i.e. is never negative). Write a test to check that
- Does the test pass? You can run it just like unit tests, via Intellij or in CLI with `./gradlew build`
- What would the same check with regular unit tests look like?

> 📌 Most unit tests can actually be converted to **Invariance properties**

Another invariance property would be that all High elves' value must be an even number (because they have a multiplier of `2`). Remember? This is the first property you uncommented!

#### Step 2 - Inverse
**Inverse properties** check that it's possible to transform some input to an output and back to the original input, no matter the input. This is a useful property because it guarantees some functions **don't lose information** and/or are **consistent**.

- For any `troll` and any `elf`, if the troll kills the `elf` and then realizes the elf survived, what should be the result?
- Write an **inverse property** test to check that

Testing it will ensure that `iGotOne` and `oopsHeSurvived` are consistent.

#### Step 3 - Analogy
**Analogous properties** check that there are at least 2 different ways from any input to reach an output. This is a useful property because it guarantees some functions are **consistent** (can also be useful for **refactors**) 

- For any `troll`, any `elf` and any positive `quantity` of killed elves, what should be the difference between:
  - killing a single `elf` and repeating this operation `quantity` times
  - killing in a single strike `quantity` units of `elf`?
- Write an **analogous property** test to check that

This ensures that `iGotOne` and `iGot` are consistent.

#### Step 4 - Idempotence
**Idempotent properties** check that running a function once or several times leads to exactly the same result, i.e. an idempotent function brings to a stable state from which this function becomes useless.

- For any `troll` and any `elf`, once all `elf`s have been resurrected, what should happen if these `elf`s are resurrected again?
- Write an **idempotent property** test to check that

This ensures that `allElvesOfAKindResurrected` brings the troll killing list to a stable state.

#### [Bonus] Step 5 - Metamorphism
**Metamorphic properties** check that running a function with variants of the same input should lead to equal or consistent outputs. E.g. if the input is multiplied by 2, is the output also multiplied by 2? Divided by 2? The same?

- For any `troll` and any `elf`, what should the `troll` score be compared to the score of the `troll` after killing `elf`?
- Write a **metamorphic property** test to check that

This ensures that `iGotOne` correctly increases the kill list (and thus the score) when an elf is killed.

#### [Bonus] Step 6 - Injection
**Injective properties** check that different inputs lead to different outputs, i.e. there aren't 2 different inputs that lead to the same output, i.e. each output has at most 1 input.

- For any `troll` and any 2 elves `elf1` and `elf2`, assuming `elf1` is different from `elf2`, `troll` after killing `elf1` must be different from `troll` after killing `elf2`
- Write an **injective property** test to check that

This ensures that `iGotOne` always updates the provided troll in a unique way.
