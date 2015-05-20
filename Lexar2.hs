module Main
       where
import System.IO
import Data.List

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "Please enter the string: "
  name <- getLine
  let str = decompose name
  print str


{-
This part is dealing with assertion language
-}
type VARLIST = [String]

data AOP = ADD | MUL | SUB | DIV  -- arithmetic operators +, -, *, /, %

instance Show AOP where
  show ADD = "+"
  show SUB = "-"
  show MUL = "*"
  show DIV = "/"

  
data BOP = EQU | LET | GRT | NEQ -- boolean operators =, <, >, ~(not equal)

instance Show BOP where
  show EQU = "="
  show LET = "<"
  show GRT = ">"
  show NEQ = "!="

data LOP = IMP | OR | AND | SIM | SCO | EXI | UNI | NOT | BRAC deriving (Enum, Eq) -- logical operators &, |, !, ->, 

instance Show LOP where
  show IMP = "->"
  show OR  = "||"
  show AND = "&&"
  show SIM = "-*"
  show SCO = "**"
  show EXI = "^E"
  show UNI = "^A"
  show NOT = "!"
  show BRAC = "("


data EXP = V Int
         | S String
         | C EXP AOP EXP deriving (Show)
                                 
data BEXP = B Bool
          | E EXP BOP EXP
          | N LOP BEXP
          | L BEXP LOP BEXP
          | Q LOP VARLIST BEXP deriving (Show)


-- expression -> number of ( brackets encountered -> current operator to search
getop :: String -> Int -> LOP -> (String, LOP, String)
getop [] 0 op = ([], op, []) -- go for exp
getop [] n _ = ([], NOT, [])
getop (x:xs) n BRAC
  | x == '(' && (last xs) == ')' && n == 0 = ([], BRAC, init xs) -- getop (init xs) 0 op -- correct it
  | x == ' ' = getop xs n BRAC
  | otherwise = (x:xs, BRAC, [])
getop ('(':xs) n op = let (a,b,c) = getop xs (n+1) op
                      in ('(':a, b, c)
getop (')':xs) n op = let (a,b,c) = getop xs (n-1) op
                      in (')':a, b, c)
getop (x:y:xs) n op
  | (show op) == [x]   && n == 0 = ([], op, y:xs)
  | (show op) == x:[y] && n == 0 = ([], op, xs)
  | otherwise                    = let (a, b, c) = getop (y:xs) n op
                                   in (x:a, b, c)


getop' ::  String -> LOP -> (String, LOP, String)
getop' s op = getop s 0 op 

firstop :: LOP
firstop = IMP


decompose :: String -> BEXP
decompose s = decompose' (getop' s firstop)


decompose' :: (String, LOP, String) -> BEXP
decompose' (a, BRAC, b)
  | b == [] = B True
  | otherwise = decompose' (getop' b firstop)
decompose' (a, lop, []) = decompose' (getop' a (succ lop))
decompose' (a, lop, b) 
  | lop == NOT = N NOT (decompose' (getop' b (succ lop)))
  | lop == UNI || lop == EXI =  let (c, d) = getvars b
                                in Q lop c (decompose' (getop' d (succ lop)))
  | otherwise = L (decompose' (getop' a (succ lop))) lop (decompose' (getop' b lop))


-- Collects all the variables and the rest of the string
getvars :: String -> (VARLIST, String)
getvars [] = ([], "")
getvars xxs@('(':xs) = ([""] , xxs)
getvars (',':xs) = let (a, b) = getvars xs
                    in ("":a, b)
getvars (' ':xs) = getvars xs
getvars (x:xs) = let (a, b) = getvars xs
                  in ((x:(head a)):(tail a), b)

