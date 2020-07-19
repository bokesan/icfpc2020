module SExpr ( SExpr(..)
             , car, car'
             , cdr, cdr'
             , nth
             , cons
             , list
             , list1
             , parseSExpr
  ) where

import Text.Parsec
import Data.Functor

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
