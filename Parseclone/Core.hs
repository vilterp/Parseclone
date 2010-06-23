module Parseclone.Core where

type Parser a b = ParseSt a -> ParseResult a b
type ParseResult a b = Either (ParseError a) (b, ParseSt a)
data ParseSt a = ParseSt [a] Position deriving (Show)
data Position = Position Offset SourceName deriving (Show)
type Offset = Int
type SourceName = String
data ParseError a = ExpectedButFound Position a a
                  | ExpectedMsgButFound Position String a
                  | ExpectedButEOI Position a
                  | ExpectedMsgButEOI Position String
                  | FailureMsg Position String
                  | ExtraInput [a] Position
                  deriving (Show)

run :: Parser a b -> [a] -> SourceName -> ParseResult a b
run p xs sn = p $ ParseSt xs (Position 0 sn)

runTest :: Parser a b -> [a] -> ParseResult a b
runTest p as = run p as "<runTest input>"

-- error stuff

getPosition :: ParseError a -> Position
getPosition (ExpectedButFound p _ _) = p
getPosition (ExpectedMsgButFound p _ _) = p
getPosition (ExpectedButEOI p _) = p
getPosition (ExpectedMsgButEOI p _) = p
getPosition (FailureMsg p _) = p

getMsg :: (Show a) => ParseError a -> String
getMsg (ExpectedButFound _ e f) = "expected " ++ (show e) ++ " but found " ++ (show f)
getMsg (ExpectedMsgButFound _ e f) = "expected " ++ e ++ " but found " ++ (show f)
getMsg (ExpectedButEOI _ e)  = "expected " ++ (show e) ++ " but found end of input"
getMsg (ExpectedMsgButEOI _ s) = "expected " ++ s ++ " but found end of input"
getMsg (FailureMsg _ msg) = msg
getMsg (ExtraInput i _) = "extra input: " ++ (show i)

isUnexpectedEOI :: ParseError a -> Bool
isUnexpectedEOI (ExpectedButEOI _ _) = True
isUnexpectedEOI (ExpectedMsgButEOI _ _) = True
isUnexpectedEOI _ = False

-- Primitives

literalP :: String -> (a -> Bool) -> Parser a a
literalP msg f = \st -> parseLitP f st
                 where parseLitP _ (ParseSt [] off) = Left $ ExpectedMsgButEOI off msg
                       parseLitP f (ParseSt (x:xs) pos@(Position o sn)) | f x       = Right (x, ParseSt xs (Position (o+1) sn))
                                                                        | otherwise = Left $ ExpectedMsgButFound pos msg x

literal :: (Show a, Eq a) => a -> Parser a a
literal x = literalP (show x) (== x)

exceptLiterals :: (Show a, Eq a) => String -> [a] -> Parser a a
exceptLiterals msg [] = error "must give some exceptions to exceptLiterals"
exceptLiterals msg xs = literalP ("any " ++ msg ++ " except " ++ (mkList xs)) (\e -> not $ elem e xs)
                        where mkList [] = undefined
                              mkList [x] = show x
                              mkList [x, y] = (show x) ++ " or " ++ (show y)
                              mkList (x:xs) = (show x) ++ ", " ++ (mkList xs)

exceptLiteral :: (Show a, Eq a) => String -> a -> Parser a a
exceptLiteral msg x = exceptLiterals msg [x]

success :: b -> Parser a b
success x = \st -> Right (x, st)

failure :: String -> Parser a b
failure msg = \(ParseSt _ off) -> Left $ FailureMsg off msg

-- First-Order Combinators

eitherOr :: Parser a b -> Parser a b -> Parser a b
eitherOr p1 p2 = \st -> case p1 st of
                          res@(Right (r, st')) -> res
                          Left _ -> p2 st

wrap :: Parser a b -> (b -> c) -> Parser a c
wrap p f = \st -> case p st of
                    Right (b, st') -> Right $ (f b, st')
                    Left e -> Left e

wrapWithPos :: Parser a b -> (b -> Position -> c) -> Parser a c
wrapWithPos p f = \st@(ParseSt _ pos) -> case p st of
                                           Right (r, st') -> Right (f r pos, st')
                                           Left e -> Left e

andThen :: Parser a b -> Parser a c -> Parser a (b, c)
andThen p1 p2 = \st -> case p1 st of 
                         Left e -> Left e
                         Right (res1, st') -> case p2 st' of
                                                Left e -> Left e
                                                Right (res2, st'') -> Right ((res1, res2), st'')

phrase :: Parser a b -> Parser a b
phrase p = \st -> case p st of
                    Left e -> Left e
                    Right (res, st') -> case st' of
                                           (ParseSt [] _) -> Right (res, st')
                                           (ParseSt xs pos) -> Left $ ExtraInput xs pos

-- Second-Order Combinators

opt :: Parser a b -> Parser a (Maybe b)
opt p = eitherOr (wrap p Just) (success Nothing)

rep :: Parser a b -> Parser a [b]
rep p = eitherOr (rep1 p) (success [])

rep1 :: Parser a b -> Parser a [b]
rep1 p = (p `andThen` (rep p)) `wrap` \(x, y) -> x:y

oneOf :: (Show a, Eq a) => String -> [a] -> Parser a a
oneOf msg [] = failure msg
oneOf msg (x:xs) = (literal x) `eitherOr` (oneOf msg xs)

oneOfP :: String -> [Parser a b] -> Parser a b
oneOfP msg ps = foldr eitherOr (failure msg) ps

inSeq :: [Parser a b] -> Parser a [b]
inSeq [] = error "no parsers supplied to inSeq"
inSeq (p:[]) = p `wrap` (:[])
inSeq (p:ps) = p `andThen` (inSeq ps) `wrap` \(b, bs) -> b:bs

literalSeq :: (Show a, Eq a) => [a] -> Parser a [a]
literalSeq xs = inSeq $ map literal xs

singleAndMulti :: (Show a, Eq a) => String -> [[a]] -> [a] -> Parser a [a]
singleAndMulti msg mc sc = (oneOfP msg parsers)
                           where parsers = map literalSeq $ mc ++ (map (:[]) sc)
