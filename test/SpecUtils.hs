module SpecUtils
  ( module Test.Hspec
  , shouldReturn
  , shouldThrow
  ) where

import Control.Monad.Except (MonadError, throwError)
import Test.Hspec hiding (shouldReturn, shouldThrow)

infix 1 `shouldReturn`, `shouldThrow`

type Err = Either String

-- FIXME Call stack
shouldReturn :: (Show a, Eq a) => Err a -> a -> Expectation
shouldReturn action expectedValue = action `shouldBe` pure expectedValue

-- FIXME Call stack
shouldThrow :: (Show a, Eq a) => Err a -> String -> Expectation
shouldThrow action expectedError = action `shouldBe` throwError expectedError

-- TODO shouldNotReturn, shouldNotThrow
