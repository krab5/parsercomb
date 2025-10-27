{-|
Module: Parsing
Description: A "small" parser combinator library
Copyright: (c) krab5, 2023
License: GPL-3
Maintainer: crab.delicieux@gmail.com

This library implements a simple-yet-versatile form of parser combinators, represented basically
as state monads. Parsers are alternative monads, with `<|>` being the choice (alternative in a
grammar) and `>>=` being the sequencing of parsers.

The module also provides alternative functions and operators, for convenience. For instance,
`%>` and `%:>` exploit parsing results that are `MonadPlus` to parse and accumulate results
(which is very common when having repetitions, for instance).

The library has been made fairly generic, in particular in term of how the parsing result is
made. A "default" implementation uses the `Maybe` monad, which is good enough for simple parsing.

-}
module ParserComb.Parsing where

import ParserComb.Parsing.Stream

import Data.Functor
import Control.Applicative
import Control.Monad
import qualified Data.Char as Char

-- | The main type for the parser, or parser "transformer". The first type parameter (@m@) is the
-- monad wrapping the parsing result. It should be some kind of alternative monad or `MonadFail` in 
-- theory. This is not mandatory but will greatly impact the lib's usability.
--
-- The second type parameter (@s@) is the incoming stream. Once again, this could really be anything,
-- although a number of convenient functions work on _stream-like_ inputs (see `Stream` typeclass).
--
-- Last, the third type parameter (@r@) is the type of the outgoing result. This is the type that varies
-- the most and for which `ParserT` is a well-behave monad.
newtype ParserT m s r = ParserT { 
    runParserT :: s -> m (s, r)     -- ^ Unwrap the parser, i.e. runs it
}

-- | Execute the parser, i.e. run it and keep only the result (if available). Note that no verification
-- is done as if there is still stream to be parsed. A good grammar should explicitly have a starting
-- rule with a `end` or similar terminal, indicating the end of stream.
execParserT :: Functor m => ParserT m s r -> s -> m r
execParserT p s = snd <$> runParserT p s

-- | `Functor` instance for the parser, straightforward
instance Functor m => Functor (ParserT m s) where
  fmap f p = ParserT $ \s -> fmap (fmap f) $ runParserT p s

-- | `Applicative` instance for the parser. 
instance Monad m => Applicative (ParserT m s) where
  -- | The parser that always succeed, does not consume any character and returns the provided result.
  pure x = ParserT $ \s -> return (s, x)
  -- | The applicative lift transfers whatever is left to be read from the flux from a parser to another.
  -- (In other words, this is not parallel parsing !)
  liftA2 f2 p1 p2 = -- (a -> b -> c) -> Parser s a -> Parser s b -> Parser s c
    ParserT $ \s ->
        runParserT p1 s >>= \(s', ra) -> runParserT p2 s' >>= \(s'', rb) -> return (s'', f2 ra rb)

-- | `Monad` instance for the parser.
instance Monad m => Monad (ParserT m s) where
  return = pure
  -- | This is effectively the sequencing of parsers with value
  -- transferring, allowing to accumulate values in a contexte, e.g. :
  -- @
  --     parse1 >>= \x -> parse2 >>= \y -> parse3 >>= \z -> return (x, y, z)
  -- @
  pa >>= f = ParserT $ \s ->
      runParserT pa s >>= \(s', ra) -> runParserT (f ra) s'

-- | `Alternative` instance for the parser. 
instance (Monad m, Alternative m) => Alternative (ParserT m s) where
  -- | The parser that parses nothing and always fails
  empty = ParserT $ \s -> empty
  -- | This is lazy "parallel" parsing: if the first parser
  -- succeeds, the others are not run; otherwise, we run the next, and so on. This allows to write
  -- grammars with alternative rules, e.g. :
  -- @
  --     -- A -> 'a'
  --     -- A -> 'b'
  --     parseA = parseChar 'a' <|> parseChar 'b'
  -- @
  --
  p1 <|> p2 = ParserT $ \s -> runParserT p1 s <|> runParserT p2 s

-- | `MonadFail` instance for the parser. Useful to interrupt parsing with an error message.
-- Of course, it is more useful if the wrapping monad handles error messages meaningfully.
instance (MonadFail m) => MonadFail (ParserT m s) where
  fail x = ParserT $ \_ -> fail x

infixl 1 >>>

-- | A version of monadic sequencing (i.e., `>>`) that drops the right-hand side result and
-- keeps the left-hand side one.
(>>>) :: Monad m => ParserT m s a -> ParserT m s b -> ParserT m s a
pa >>> pb = pa >>= \x -> pb >> return x

-- | Type synonym for parsers which wrapping monad is `Maybe`.
type Parser = ParserT Maybe

-- | Run a parser with `Maybe` as its monad (`runParserT` wrapper)
runParser :: Parser s r -> s -> Maybe (s, r)
runParser = runParserT

-- | Execute a parser with `Maybe` as its monad (`execParserT` wrapper)
execParser :: Parser s r -> s -> Maybe r
execParser = execParserT

-- | A wrapper type/transformer for parsers that take context'd streams (i.e. `Stream` that have
-- been "lifted" to `Ctx`).
type ParserCtxT m s a r = ParserT m (Ctx s a) r

-- | Specialized version of `runParserT` for `ParserCtxT`, that takes a normal `Stream` and sets up
-- a `Ctx` wrapper for it.
runParserCtxT :: Stream s => ParserCtxT m s a r -> (a -> Bool) -> String -> (Int,Int) -> s a -> m (Ctx s a, r)
runParserCtxT parser isNewline source start stream = 
    runParserT parser (withContext isNewline source start stream)

-- | Specialized version of `execParserT` for `ParserCtxT`. Similar to `runParserCtxT` but drops the
-- remaining stream and keep only the result.
execParserCtxT :: (Stream s, Functor m) => ParserCtxT m s a r -> (a -> Bool) -> String -> (Int,Int) -> s a -> m r
execParserCtxT parser isNewline source start stream =
    fmap snd $ runParserCtxT parser isNewline source start stream

-- | Specialized version of `runParserCtxT` for character streams (with regular newline character for 
-- setting up the context).
runParserCtxCT :: Stream s => ParserCtxT m s Char r -> String -> (Int,Int) -> s Char -> m (Ctx s Char, r)
runParserCtxCT parser source start stream =
    runParserT parser (withContextC source start stream)

-- | Specialized version of `execParserCtxT` for character streams (with regular newline character for
-- setting up the context).
execParserCtxCT :: (Stream s, Functor m) => ParserCtxT m s Char r -> String -> (Int,Int) -> s Char -> m r
execParserCtxCT parser source start stream =
    fmap snd $ runParserCtxCT parser source start stream

-- | Create a parser from a function.
parserTOf :: (s -> m (s, r)) -> ParserT m s r
parserTOf = ParserT

-- | Same as `parserTOf` but specialized for parser with `Maybe`
parserOf :: (s -> Maybe (s, r)) -> Parser s r
parserOf = parserTOf

-- | Parser that fails if the predicate is true on the given stream, and succeeds while consuming
-- nothing otherwise.
failIf :: Alternative m => (s -> Bool) -> ParserT m s ()
failIf p = ParserT $ \s -> if p s then empty else pure (s, ())

-- | Parser that always succeeds, consumes nothing and returns `mzero` in the provided `MonadPlus`
-- instance. This is especially useful for repetition, i.e. :
-- 
-- > -- A -> /\ (empty word)
-- > -- A -> x A
-- > parseA = (parseChar 'x' >>= \x -> parseA >>= \xs -> x `mplus` xs) <|> zero
-- 
--
zero :: (Monad m, MonadPlus f) => ParserT m s (f a)
zero = return mzero

-- | Parser that succeeds iff the stream has reach its end (and does not consume anything, subsequently).
end :: (Alternative m, Stream s) => ParserT m (s a) ()
end = failIf (not . done)

-- | Jump in the incoming stream, i.e. replace the current stream by another one. This is intended for
-- redirections during parsing, and especially resetting the incoming stream (when performing simple
-- lookahead).
--
-- This parser always succeeds, consume nothing and returns `()`
jump :: Monad m => s -> ParserT m s ()
jump s = ParserT $ \_ -> return (s, ())

-- | Drop a symbol of the stream. This parser fails if the stream is done.
drop1 :: (Stream s, Alternative m) => ParserT m (s a) ()
drop1 = ParserT $ \s ->
    case uncons s of 
      Nothing -> empty
      Just (_,xs) -> pure (xs, ())

-- | Consume one symbol of the stream and return it as result (as a `MonadPlus`). Fails if the stream
-- is done.
--
-- This is intended for "transponder"-type parsers, which rewrites the input stream on their output (after
-- transformation). The result is a `MonadPlus` to allow to chain the `keep1`. The result is usually a
-- list as well in that case.
keep1 :: (Stream s, Alternative m) => ParserT m (s a) a
keep1 = ParserT $ \s ->
    case uncons s of
      Nothing -> empty
      Just (x, xs) -> pure (xs, x)

-- | Parser that consumes nothing and fails if the stream is done/empty.
notEmpty :: (Alternative m, Stream s) => ParserT m (s a) ()
notEmpty = failIf done

-- | Parser combinator that runs the given parser and then sets back the stream where it
-- was before parsing.
--
-- For instance, if the result of parsing @onexyz...@ is @1@ with remaining stream being @xyz...@, then
-- using `parseAndBack` on this parser will yield the result @1@ (result of the parser) but @onexyz...@ as
-- the remaining stream.
parseAndBack :: Monad m => ParserT m s r -> ParserT m s r
parseAndBack p1 =
    ParserT $ \s -> runParserT (p1 >>= (\x -> jump s >> return x)) s

-- | Parser combinator that repeats the given parser and combine its result using a function, in a
-- left fold fashion (@f@ applied to @z@ and result then passed down next parser).
foldlP :: (Alternative m, Monad m) => (b -> a -> b) -> b -> ParserT m s a -> ParserT m s b
foldlP f z p =
    (p >>= \x -> foldlP f (f z x) p) <|> return z

-- | Parser combinator that repeats the given parser and combine its result using a function, in a
-- right fold fashion (@f@ combines result of first parser with result of recursive call).
foldrP :: (Alternative m, Monad m) => (a -> b -> b) -> b -> ParserT m s a -> ParserT m s b
foldrP f z p =
    (p >>= \x -> foldrP f z p >>= \z' -> return (f x z')) <|> return z

-- | Repeat the given parser and accumulate the result in a `MonadPlus`. The parser succeeds if
-- the input is empty (and returns `mzero`).
repeatP :: (Alternative m, Monad m, MonadPlus f) => ParserT m s r -> ParserT m s (f r)
repeatP parser =
    (parser %:> repeatP parser) <|> return mzero

infixr 3 %>, %:>

-- | Convenient combinator for parsers which result is a `MonadPlus`, that chains the parsers and
-- `mplus` their results.
(%>) :: (Monad m, MonadPlus f) => ParserT m s (f a) -> ParserT m s (f a) -> ParserT m s (f a)
p1 %> p2 = p1 >>= \r1 -> p2 >>= \r2 -> return (r1 `mplus` r2)

-- | Specific version of `%>` where the first argument is not wrapped in the `MonadPlus` (convenient
-- for injecting an element in a list, typically).
(%:>) :: (Monad m, MonadPlus f) => ParserT m s a -> ParserT m s (f a) -> ParserT m s (f a)
p1 %:> p2 = (fmap pure p1) %> p2

infixl 7 `ptimes`

-- | Repeat a parser a (positive or null) number of times and return the parsed sequence, in order.
ptimes :: (Monad m, MonadPlus f) => ParserT m s a -> Int -> ParserT m s (f a)
ptimes p 0 = return mzero
ptimes p n | n > 0 = p %:> (ptimes p (n - 1))

-- | Test the head of the stream using the given predicate and succeeds if its true, but consume
-- nothing. Fails if the stream is empty.
testP :: (Alternative m, Stream s) => (a -> Bool) -> ParserT m (s a) ()
testP p = ParserT $ \s ->
    case uncons s of
        Just (x, _) | p x -> pure (s, ())
        _ -> empty

-- | Parser that succeeds if the head of the stream satisfies the given predicate. If it does not or if
-- the stream is done, it fails. This returns the consumed symbol.
parseP :: (Alternative m, Stream s) => (a -> Bool) -> ParserT m (s a) a
parseP p = ParserT $ \s ->
    case uncons s of
      Just (x, xs) | p x -> pure (xs, x)
      _ -> empty

-- | Same as `parseP` but ignores the parsed symbol (and returns `()`).
parseP_ :: (Alternative m, Stream s) => (a -> Bool) -> ParserT m (s a) ()
parseP_ p = ParserT $ \s ->
    case uncons s of
      Just (x, xs) | p x -> pure (xs, ())
      _ -> empty

-- | Parse until the given predicate is true, and outputs the accumulated parsed characters.
parseUntil :: (Alternative m, Monad m, MonadPlus f, Stream s) => (a -> Bool) -> ParserT m (s a) (f a)
parseUntil p =
    (parseP (not . p) %:> parseUntil p) <|> return mzero

-- | Dual of `parseUntil`, parse the stream while the predicate is true, and accumulate the parsing
-- result in a `MonadPlus`.
parseWhile :: (Alternative m, Monad m, MonadPlus f, Stream s) => (a -> Bool) -> ParserT m (s a) (f a)
parseWhile p =
    (parseP p %:> parseWhile p) <|> return mzero

parseSubExp :: (Alternative m, Monad m, MonadPlus f, Stream f, Stream s) => (a -> Bool) -> ParserT m (f a) b -> ParserT m (s a) b
parseSubExp delim subp =
    ParserT $ \s -> runParserT (parseUntil delim) s >>= \(s', r) ->
        runParserT subp r >>= \(ssub, rsub) ->
            case uncons ssub of
              Just _ -> empty
              _ -> return (s', rsub)

-- | Specialized version of `testP` for `Eq` instances.
testChar :: (Alternative m, Stream s, Eq a) => a -> ParserT m (s a) ()
testChar c = testP (== c)

-- | Parser that succeeds if the head of the stream is the given symbol, and fails otherwise or if the
-- stream is done.
parseChar :: (Alternative m, Stream s, Eq a) => a -> ParserT m (s a) a
parseChar c = parseP (== c)

-- | Same as `parseChar` but drops the symbol and returns `()`.
parseChar_ :: (Alternative m, Stream s, Eq a) => a -> ParserT m (s a) ()
parseChar_ c = parseP_ (== c)

-- | Parser that succeeds if the first elements of the stream correspond to the given sequence of
-- symbols. If the stream is empty (or not long enough) or if the sequence is not right, it fails.
--
-- The returned result correspond to each character of the sequence that was parsed successfully, wrapped
-- in the `MonadPlus` result.
parseSeq :: (Monad m, Alternative m, Eq a, Foldable t, Stream s, MonadPlus f) => t a -> ParserT m (s a) (f a)
parseSeq = foldl (\acc x -> acc %> (pure <$> parseChar x)) zero

-- | Same as `parseSeq` but drops the result and only return `()`.
parseSeq_ :: (Monad m, Alternative m, Eq a, Foldable t, Stream s) => t a -> ParserT m (s a) ()
parseSeq_ = foldl (\acc x -> acc >> parseP_ (== x)) (return ())

-- | Parse exactly one blank space (and drop it).
parseSpace :: (Alternative m, Stream s) => ParserT m (s Char) ()
parseSpace = parseP_ (Char.isSpace)

-- | Parse a sequence of blank spaces (at least one) and drop the result.
parseSpaces :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) ()
parseSpaces = parseSpace >> (parseSpaces <|> return ())

-- | Parse a sequence of blank spaces (zero or more)
parseSpaces0 :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) ()
parseSpaces0 = parseSpaces <|> return ()

-- | Parse a normal blank space (U+20)
parseSpace' :: (Alternative m, Stream s) => ParserT m (s Char) ()
parseSpace' = parseP_ (== ' ')

-- | Parse a sequence of normal blank spaces (at least one)
parseSpaces' :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) ()
parseSpaces' = parseSpace' >> (parseSpaces' <|> return ())

-- | Parse a sequence of normal blank spaces (can be 0)
parseSpaces0' :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) ()
parseSpaces0' = parseSpaces' <|> return ()

-- | Parse a newline
parseNewline :: (Alternative m, Monad m, Stream s) => ParserT m (s Char) ()
parseNewline = parseChar_ '\n' <|> parseSeq_ "\n\r" <|> parseSeq_ "\r\n"

-- | Parse one digit and return the corresponding `Int`. Based on `Char.isDigit`, so only works
-- in base 10
parseDigit :: (Alternative m, Stream s) => ParserT m (s Char) Int
parseDigit = Char.digitToInt <$> (parseP Char.isDigit)

-- | Parse one hexadecimal digit and return the corresponding `Int`. Based on `Char.isHexDigit`,
-- so recognize any character amon '0'..'9', 'a'..'f', 'A'..'F'
parseHexDigit :: (Alternative m, Stream s) => ParserT m (s Char) Int
parseHexDigit = Char.digitToInt <$> (parseP Char.isHexDigit)

-- | Parse a full number (and return its value). A number in this parser is simply a sequence of
-- digits with no further constraints. Number is assumed to be in base 10.
parseNumber :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) Int
parseNumber =
    ((parseChar_ '-' >> return (\x -> -x)) <|> (parseChar_ '+' >> return id) <|> (parseAndBack (parseDigit) >> return id))
        >>= \f -> parseDigit >>= parsenum >>= (return . f)
    where parsenum x = (parseDigit >>= \y -> parsenum (10 * x + y)) <|> return x

parseHexNumber :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) Int
parseHexNumber =
    ((parseChar_ '-' >> return (\x -> -x)) <|> (parseChar_ '+' >> return id) <|> (parseAndBack (parseDigit) >> return id))
        >>= \f -> parseDigit >>= parsenum >>= (return . f)
    where parsenum x = (parseDigit >>= \y -> parsenum (16 * x + y)) <|> return x

-- | Parse a type based on its textual representation given by its `Show` typeclass intsance.
parseShow :: (Monad m, Alternative m, Show x, Stream s) => x -> ParserT m (s Char) x
parseShow x = parseSeq_ (show x) >> return x





