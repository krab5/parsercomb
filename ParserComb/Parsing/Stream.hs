{-|
Module: Parsing.Stream
Description: Modelling generic streams, to be used with wrappers.
Copyright: (c) krab5, 2023
License: GPL-3
Maintainer: crab.delicieux@gmail.com

This part of the parsing library proposes a generalization of streams of symbols using typeclasses.

This allows to propose other input than simple strings for parser, and allow to write stream "wrappers",
that allow to annotate streams with context or special behaviours upon reading some symbols.

-}
module ParserComb.Parsing.Stream where

import Data.Maybe (isNothing)

-- | A class representing a certain type of polymorphic data structure for which it is
-- possible to get an element + the rest of the structure (presumably without the element).
--
-- This class effectively factorises anything that looks like a sequence (or a list). It 
-- allows for a bit of flexibility when using parsers (rather than using plain old lists).
class Stream s where
  -- | Takes the stream and try to extract an element and the remainder of the stream. If not possible,
  -- return `Nothing`.
  --
  -- This function returning `Nothing` implies that the stream is finished (end of string, end of file,
  -- etc.) or at least that no other character is available.
  uncons :: s a -> Maybe (a, s a)

-- | Tests if the stream is done, i.e. it is empty/no other symbol is available
done :: Stream s => s a -> Bool
done = isNothing . uncons

-- | Stream instance for lists (the most useful one)
instance Stream [] where
  uncons [] = Nothing
  uncons (t:q) = Just (t, q)

-- | A `Stream` wrapper that embeds contextual informations (i.e. source, line number and column number).
-- The idea is that uppon `uncons`'ing, the values move by themselves, allowing parsers to access
-- a context, mainly for error reporting.
data Ctx s a = Ctx {
    _stream :: s a,         -- ^ Wrapped stream
    _newline :: a -> Bool,  -- ^ Predicate establishing is a symbol count as a newline
    ctx_source :: String,   -- ^ Source of the stream (e.g. a file, user input, etc.)
    ctx_line :: Int,        -- ^ Line number for the current character
    ctx_column :: Int       -- ^ Column number for the current character
}

-- | `Show` instance for the wrapped stream. This is mainly for debug purposes; it does not
-- show the stream, but simply show if it is empty or not, and its contextual informations.
instance Stream s => Show (Ctx s a) where
    show c =
        "<<wrapped stream (" ++ (if done (_stream c) then "empty" else "not empty") ++ ")"
        ++ ", source: " ++ (ctx_source c)
        ++ ", line: " ++ (show $ ctx_line c) 
        ++ ", column: " ++ (show $ ctx_column c) 
        ++ ">>"

-- | `Stream` instance for the wrapped stream. This is mainly a wrapper for the `uncons` implementation
-- of the wrapped stream, but updating the coordinates depending on the uncons'd symbol.
instance Stream s => Stream (Ctx s) where
    uncons c =
        case uncons $ _stream c of
            Nothing -> Nothing
            Just (t, q) -> 
                Just (t, c { _stream = q
                           , ctx_line = ctx_line c + (if _newline c t then 1 else 0)
                           , ctx_column = if _newline c t then 1 else ctx_column c + 1 })


-- | Wrap a stream into a `Ctx`, with given starting coordinates and newline predicate.
withContext :: Stream s => (a -> Bool) -> String -> (Int, Int) -> s a -> Ctx s a
withContext isNewline source (line,column) stream =
    Ctx { _stream = stream
        , _newline = isNewline
        , ctx_source = source
        , ctx_line = line
        , ctx_column = column }

-- |  Wrap a `Char` stream into a `Ctx`, with given starting coordinates. Newline predicate is
-- simply `== '\n'`.
withContextC :: Stream s => String -> (Int,Int) -> s Char -> Ctx s Char
withContextC = withContext (== '\n')

-- | Usual starting coordinates for a context (line 1, column 1).
contextStart :: (Int,Int)
contextStart = (1,1)


-- | Wrap a symbol stream so that it ignores some symboles when reading it. Bascially, upon unconsing
-- the stream, if the head satisfies a predicate, then it is discarded and the tail is unconsed instead,
-- recursively.
--
-- This is especially practical when you want to quickly ignore a whole class of characters (e.g. white
-- spaces).
data Ignoring s a = Ignoring {
    i_ignore :: a -> Bool,          -- ^ Predicate determining what should be ignored
    i_stream :: s a                 -- ^ Wrapped stream
}

-- | Build an `Ignoring` stream from a normal stream and an exclusion predicate
ignoring :: Stream s => (a -> Bool) -> s a -> Ignoring s a
ignoring ign s = Ignoring { i_ignore = ign, i_stream = s }

-- | `Stream` instance for `Ignoring`. The unconsing checks the head to see if it should be 
-- ignored, and pursue the unconsing if needed.
instance Stream s => Stream (Ignoring s) where
    uncons igs =
        case uncons $ i_stream igs of
            Nothing -> Nothing
            Just (t, q) | i_ignore igs t -> uncons $ igs { i_stream = q }
            Just (t, q) -> Just (t, igs { i_stream = q })

-- | A convenient show instance for a stream wrapped in `Ignoring`, that shows the first symbols
-- and then some ellipses, except if the stream is small enough.
instance (Stream s, Show a) => Show (Ignoring s a) where
    show ign =
        "<<" ++ stream False 5 (i_stream ign) ++ " (ignoring some)>>"
        where stream _ 0 _ = "..."
              stream b n s = 
                  case uncons s of
                      Nothing -> if not b then "empty" else ""
                      Just (t, q) -> show t ++ stream True (n - 1) q



