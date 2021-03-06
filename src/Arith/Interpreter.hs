{-# LANGUAGE FlexibleContexts #-}

module Arith.Interpreter
  ( evaluate
  , isBooleanValue
  , isNumericValue
  , isValue
  ) where

import Arith.Parser (Term(..))
import Control.Monad.Except (MonadError, throwError)

isValue :: Term -> Bool
isValue term = isBooleanValue term || isNumericValue term

isBooleanValue :: Term -> Bool
isBooleanValue term = case term of
  TmTrue -> True
  TmFalse -> True
  _ -> False

isNumericValue :: Term -> Bool
isNumericValue term = case term of
  TmZero -> True
  TmSucc t -> isNumericValue t
  TmPred t -> isNumericValue t
  _ -> False

evaluate :: MonadError String r => Term -> r Term
evaluate = go
  where
    go (TmIf condition thenBranch elseBranch) = case condition of
      TmTrue -> go thenBranch
      TmFalse -> go elseBranch
      _ -> go condition >>= \condition' -> if condition == condition'
        then throwError "Condition is not a boolean value"
        else go $ TmIf condition' thenBranch elseBranch
    go (TmIsZero t) | isNumericValue t = isZero <$> go t
    go (TmSucc (TmPred t)) | isNumericValue t = go t
    go (TmPred (TmSucc t)) | isNumericValue t = go t
    go (TmSucc t) | isNumericValue t = go t >>= fmap TmSucc . go
    go (TmPred t) | isNumericValue t = go t >>= fmap TmPred . go
    go t = if isValue t
      then pure t
      else throwError $ "Unevaluable term: " ++ show t
    isZero t | isNumericValue t = if t == TmZero
      then TmTrue
      else TmFalse
