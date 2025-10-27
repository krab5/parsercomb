{-|
Module: Parsing.Result
Description: A special type for parsing results, that differentiates between no parsing and failure
Copyright: (c) krab5, 2023
License: GPL-3
Maintainer: crab.delicieux@gmail.com

This module defines the `Result` type, specialized for parsing. This type differentiates between
success, nothing parsed and actual failure, allowing for much ritcher behaviours.

The type is also embedded in a specific parser type synonym with context'd streams (see `Ctx`), which
provide useful metadatas for errors raised.
-}
module ParserComb.Parsing.Result where

import ParserComb.Parsing.Stream
import ParserComb.Parsing
import Data.Functor
import Control.Applicative
import Control.Monad
import ParserComb.MonadResult

-- | Record type holding an error message with metadata
data Error = Error { 
    error_source :: String,         -- ^ Source of the error (usually a file or user input)
    error_line :: Int,              -- ^ Line number of the error
    error_column :: Int,            -- ^ Column number of the error (the error is usually _before_ due to how 
                                    -- parsers work
    error_message :: String,        -- ^ Error message
    error_cause :: Maybe Error      -- ^ Possible cause for the error (allowing for error hierarchy, similar in
                                    -- principle to Java exceptions)
}

-- | Make an error using each components of the record (this is in case the order of the fields changes).
mkError :: String -> Int -> Int -> String -> Maybe Error -> Error
mkError src lin col msg cause = 
    Error { error_source = src, error_line = lin, error_column = col, error_message = msg, error_cause = cause }

-- | Make an error without a cause.
mkError' :: String -> Int -> Int -> String -> Error
mkError' src lin col msg = mkError src lin col msg Nothing

-- | `Show` instance for `Error`. Writes the error with its source, line, column and message, plus 
-- a \"caused by:\" then the error cause (if applicable).
instance Show Error where
    show err =
        error_source err ++ ":" ++ (show $ error_line err) ++ ":" ++ (show $ error_column err) ++ ": "
            ++ error_message err ++ (andThen $ error_cause err)
        where andThen Nothing = ""
              andThen (Just e) = ", caused by: " ++ show e

-- | The result type. The result of an algorithm (parser) may be either a success (with associated 
-- computed element), a failure (with associated error), or simply a "IOU"/nothing was parsed result.
data Result a = 
    Success a 
  | NoParse
  | Failure Error
  deriving Show

-- | `Result` is a `Functor` (classical functor implementation for sum types, I could have used
-- _derived_ but you never know when you need to tweak).
instance Functor Result where
    fmap f (Success a) = Success $ f a
    fmap _ NoParse = NoParse
    fmap _ (Failure e) = Failure e

-- | `Result` is an `Applicative`. In this implementation, `Failure` act as absorbant in priority for
-- `<*>`, then `NoParse`, then `Success`.
--
-- `*>` and `<*` are explicitly implemented, just in case.
instance Applicative Result where
    -- | `pure` is always a success
    pure x = Success x
    
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e
    NoParse <*> _ = NoParse
    _ <*> NoParse = NoParse
    Success f <*> Success x = Success $ f x

    Failure e *> _ = Failure e
    _ *> x = x
    
    _ <* Failure e = Failure e
    x <* _ = x

-- | `Result` is a `Monad`. In this implementation, `Failure` and `NoParse` act as absorbants on the
-- left of `>>=`.
instance Monad Result where
    return = pure
    Failure e >>= _ = Failure e
    NoParse >>= _ = NoParse
    Success x >>= f = f x
    (>>) = (*>)

-- | `Result` is a `MonadResult`. Implementation is fairly normal.
--
-- TODO: I have to try differentiating between failure and success, but I am not entirely
-- sure how it will work with the parser.
instance MonadResult Result where
    isFailure (Success _) = False
    isFailure _ = True
    extract (Success x) = x
    extractM (Success x) = Just x
    extractM _ = Nothing

-- | `Result` is an `Alternative`. Note here that `empty` is `NoParse`, hence it is neutral for
-- `<|>`, while `Failure` is absorbant on the left only. This means that you should be careful with it,
-- as misplacing it might ruing your parsing...
instance Alternative Result where
    empty = NoParse
    NoParse <|> x = x
    Failure e <|> _ = Failure e
    Success x <|> _ = Success x

-- | Type synonym for parsers that use `Result` as monad.
type ParserR = ParserT Result
-- | Type synonym for parsers that contex' streams and use `Result` as monad.
type ParserCtx s a r = ParserCtxT Result s a r

-- | Specialized version of `runParserCtxT` for the `Result` monad.
runParserCtx :: Stream s => ParserCtx s a r  -> (a -> Bool) -> String -> (Int,Int) -> s a -> Result (Ctx s a, r)
runParserCtx = runParserCtxT

-- | Specialized version of `execParserCtxT` for the `Result` monad.
execParserCtx :: Stream s => ParserCtx s a r  -> (a -> Bool) -> String -> (Int,Int) -> s a -> Result r
execParserCtx = execParserCtxT

-- | Specialized version of `runParserCtx` for character streams (usual newline symbol used).
runParserCtxC :: Stream s => ParserCtx s Char r -> String -> (Int,Int) -> s Char -> Result (Ctx s Char, r)
runParserCtxC = runParserCtxCT

-- | Specialized version of `execParserCtx` for character streams (usual newline symbol used)
execParserCtxC :: Stream s => ParserCtx s Char r -> String -> (Int,Int) -> s Char -> Result r
execParserCtxC = execParserCtxCT

-- | Create a parser that raises a failure. This is typically used as the last element in a `<|>` chain
-- to notify that the parsing did not work while it should have.
raise :: String -> ParserCtx s a r
raise msg = ParserT $ \s -> Failure $ mkError' (ctx_source s) (ctx_line s) (ctx_column s) msg

infix 2 `orDie`

-- | Run a parser and raise an error if it does not succeed. This is to be used in `do` notations
-- 
-- > do
-- >   r <- parseNumber `orDie` "Expecting a number here"
-- >   parseChar '+'
-- >   i <- parseNumber `orDie` "Expecting imaginary part"
-- >   parseChar 'i'
-- >   return $ Complexe r i
-- 
-- To be precise, this function creates a new error if the result of the parser is `NoParse`, and if the 
-- result is `Failure`, then it creates a new error which cause is the error of that failure.
orDie :: ParserCtx s a r -> String -> ParserCtx s a r
orDie parser msg = ParserT $ \s ->
    case runParserT parser s of
        Success x -> Success x
        NoParse -> Failure $ mkError' (ctx_source s) (ctx_line s) (ctx_column s) msg
        Failure e -> Failure $ mkError (ctx_source s) (ctx_line s) (ctx_column s) msg (Just e)




