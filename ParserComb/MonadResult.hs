{-# LANGUAGE DefaultSignatures #-}

{- |
Module: MonadResult
Description: A simple module exporting a typeclass that represent a monad that is used as a result
Copyright: (c) krab5, 2023
License: GPL-3
Maintainer: crab.delicieux@gmail.com

This module mainly export the `MonadResult` type class, which represent the output of some algorithm
that may be interpreted as a failure or a success.

The idea is to generalize some common usage of sum types such as `Maybe` and `Either`, and make
more general some algorithm that handle result transfer without doing too much result inquiry
(e.g., a parser).
-}
module ParserComb.MonadResult where

-- | The `MonadResult` type class. It involves @m@ being a `Monad` mainly because instances are
-- typically used in a monadic setting.
class Monad m => MonadResult m where
    -- | Test if the monad is in a failure(-type) state.
    isFailure :: m a -> Bool
    -- | Test if the monad is in a success(-type) state. By default, it simply negate the `isFailure` function.
    -- It may be useful to override that for "multi-valued logic" kind of types.
    isSuccess :: m a -> Bool
    isSuccess = not . isFailure
    -- | Extract the value inside the monad. Caller of this method should enforce that the monad
    -- is in a success(-type) state (using `isSuccess`). Conversely, this function is only called
    -- on a success monad.
    extract :: m a -> a
    -- | Extract the value inside the monad _if possible_, or return `Nothing` if impossible.
    -- If the monad is in a success(-type) state, must return a `Just`. Otherwise, must return
    -- a `Nothing`.
    extractM :: m a -> Maybe a

-- | `MonadResult` instance for `Maybe`. Implementation is straightforward and typical: `Nothing` count
-- as failure, `Just` count as success.
instance MonadResult Maybe where
    isFailure Nothing = True
    isFailure _ = False
    extract (Just x) = x
    extractM = id

-- | `MonadResult` instance for `Either b` (for all b). Implementation is typical for Haskell: `Left` is
-- considered failure, `Right` is considered success.
instance MonadResult (Either e) where
    isFailure (Left _) = True
    isFailure _ = False
    extract (Right x) = x
    extractM (Left _) = Nothing
    extractM (Right x) = Just x


