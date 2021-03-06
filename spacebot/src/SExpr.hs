module SExpr ( SExpr(..)
             , ToSExpr(..)
             , car, car'
             , cdr, cdr'
             , nth
             , cons
             , list
             , list1
             , parseSExpr
             , modulate
             , demodulate
  ) where

import Text.Parsec
import Data.Functor

class ToSExpr a where
  toSExpr :: a -> SExpr

instance ToSExpr Integer where
  toSExpr = Int

data SExpr = Int !Integer
           | Var !String
           | Nil
           | Cons !SExpr !SExpr
           deriving (Eq, Ord)

instance Show SExpr where
  showsPrec _ (Int n) = shows n
  showsPrec _ (Var s) = showString s
  showsPrec _ Nil = showString "nil"
  showsPrec _ (Cons a b) = showChar '(' . shows a . go b
    where go Nil        = showChar ')'
          go (Cons c d) = showChar ' ' . shows c . go d
          go atom       = showString " . " . shows atom . showChar ')'

car :: SExpr -> SExpr
car (Cons x _) = x
car x          = error ("CAR of " ++ show x)

car' :: SExpr -> Maybe SExpr
car' (Cons x _) = Just x
car' _          = Nothing

cdr :: SExpr -> SExpr
cdr (Cons _ x) = x
cdr x          = error ("CDR of " ++ show x)

cdr' :: SExpr -> Maybe SExpr
cdr' (Cons _ x) = Just x
cdr' _          = Nothing

nth :: Int -> SExpr -> SExpr
nth i (Cons x xs) | i <= 0    = x
                  | otherwise = nth (i - 1) xs
nth i x = error ("NTH " ++ show i ++ " of " ++ show x)
 
cons :: SExpr -> SExpr -> SExpr
cons = Cons

list :: [SExpr] -> SExpr
list = foldr Cons Nil

list1 :: [SExpr] -> SExpr -> SExpr
list1 xs x = foldr Cons x xs

type Parser a = Parsec String () a

parseSExpr :: String -> Either ParseError SExpr
parseSExpr = parse expr "<stdin>"

expr :: Parser SExpr
expr =  number
    <|> (string "nil" $> Nil)
    <|> pcons
    <|> pvar

pvar :: Parser SExpr
pvar = do name <- many1 alphaNum
          return $ Var name

number :: Parser SExpr
number = do sign <- option '+' (char '-')
            cs <- many1 digit
            let n = read cs
            case sign of
              '+' -> return (Int n)
              _   -> return (Int (-n))

whitespace :: Parser ()
whitespace = skipMany space

whitespace1 :: Parser ()
whitespace1 = skipMany1 space

pcons :: Parser SExpr
pcons = do _ <- char '('
           _ <- whitespace
           xs <- expr `sepEndBy1` whitespace1
           end <- optionMaybe (do char '.'; whitespace1; expr)
           _ <- whitespace
           _ <- char ')'
           case end of
             Nothing -> return (list xs)
             Just x  -> return (list1 xs x)

modulate :: SExpr -> String
modulate (Int n) = modulateN n
modulate Nil = "00"
modulate (Var x) = error ("cant modulate Var " ++ x)
modulate (Cons a b) = "11" ++ modulate a ++ modulate b

modulateN :: Integer -> String
modulateN n | n < 0     = "10" ++ go 1 0 (-n)
            | otherwise = "01" ++ go 1 0 n
  where
    go lim nbits n | n < lim   = '0' : binary nbits n
                   | otherwise = '1' : go (16 * lim) (nbits + 4) n
    binary dig n = let s = toBinary n in
                   replicate (dig - length s) '0' ++ s

toBinary :: Integer -> String
toBinary n | n == 0    = ""
           | otherwise = let (n',bit) = quotRem n 2 in
                         toBinary n' ++ (if bit == 0 then "0" else "1")

fromBinary :: String -> Integer
fromBinary s = go 0 s
  where
    go n "" = n
    go n ('0':s) = go (2 * n) s
    go n ('1':s) = go (2 * n + 1) s

demodulate :: String -> Either String SExpr
demodulate s = case go s of
                 (e, "") -> Right e
                 (e, rest) -> Left ("not fully demodulated: " ++ rest)
  where
    go ('0':'0':s) = (Nil, s)
    go ('0':'1':s) = num s
    go ('1':'0':s) = let (Int n,s') = num s in (Int (-n),s')
    go ('1':'1':s) = let (a,s') = go s
                         (b,s'') = go s'
                     in (Cons a b, s'')
    num s = num1 0 s
    num1 nbits ('0':s) = let (n,s') = splitAt nbits s in (Int (fromBinary n), s')
    num1 nbits ('1':s) = num1 (nbits+4) s
