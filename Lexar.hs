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
data VTYPE = VAL | SYM deriving (Show)
data VAR = VAR {
  vtype :: VTYPE,
  name :: String,
  value :: Int
} deriving (Show)
type VARLIST = [String]
type NUM = Int
data OP = ADD | MUL | SUB | DIV deriving (Show) -- arithmetic operators +, -, *, /, %
--data BOP = EQ | LT | GT | NEQ deriving (Show) -- boolean operators =, <, >, ~(not equal)
data LOP = NOT| AND | OR | IMP | SEPCON | SEPIMP | UNIV | EXIST | BRAC | EQ | LT | GT | NEQ deriving (Show) -- logical operators &, |, !, ->, 


data EXP = S  -- an expression can be just a number
         | AJust VTYPE Int VAR -- an expression can be variable and it may have a number
         | Nexp VTYPE Int OP EXP -- -v or +v
         | Cexp VTYPE Int EXP OP EXP deriving (Show)   -- v+v, v*v, etc

data BEXP = S String  -- True
          | E VTYPE Bool String BOP String -- e=e, e<e
          | M VTYPE Bool LOP BEXP  -- !BEXP
          | C VTYPE Bool BEXP LOP BEXP -- BEXP && BEXP, BEXP || BEXP, BEXP -> BEXP, BEXP ** BEXP, BEXP -* BEXP
          | Q VTYPE Bool LOP VARLIST BEXP -- Existencial quantifier
          deriving (Show)

type BRACS = Int

data TR = L String | Node TR LOP TR deriving (Show)

getop :: String -> BRACS -> LOP -> (String, LOP, String)
getop [] 0 op = ([], op, []) -- go for exp
getop [] n _ = ([], NOT, [])
getop (x:xs) n BRAC
  | x == '(' && (last xs) == ')' && n == 0 = ([], BRAC, init xs) -- getop (init xs) 0 op -- correct it
  | x == ' ' = getop xs n BRAC
  | otherwise = (x:xs, BRAC, [])
getop ('=':xs) 
getop ('(':xs) n op = let (a,b,c) = getop xs (n+1) op
                      in ('(':a, b, c)
getop (')':xs) n op = let (a,b,c) = getop xs (n-1) op
                      in (')':a, b, c)
getop ('&':xs) 0 AND = ([], AND, xs)
getop ('|':xs) 0 OR = ([], OR, xs)
getop ('-':'>':xs) 0 IMP = ([], IMP, xs)
getop ('*':'*':xs) 0 SEPCON = ([], SEPCON, xs)
getop ('-':'*':xs) 0 SEPIMP = ([], SEPIMP, xs)
getop ('^':'A':xs) 0 UNIV = ([], UNIV, xs)
getop ('^':'E':xs) 0 EXIST = ([], EXIST, xs)
getop ('!':xs) 0 NOT = ([], NOT, xs)
getop (x:xs) n op = let (a, b, c) = getop xs n op
                      in (x:a, b, c)

getop' :: String -> LOP -> (String, LOP, String)
getop' s op = getop s 0 op 

firstop :: LOP
firstop = IMP

decompose :: String -> BEXP
decompose s = decompose' (getop' s firstop)

decompose' :: (String, LOP, String) -> BEXP
decompose' (a, IMP, b)
  | b == [] = decompose' (getop' a OR)
  | otherwise = Cbexp SYM False (decompose' (getop' a OR)) IMP (decompose' (getop' b IMP))
decompose' (a, OR, b)
  | b == [] = decompose' (getop' a AND)
  | otherwise = Cbexp SYM False (decompose' (getop' a AND)) OR (decompose' (getop' b OR))
decompose' (a, AND, b)
  | b == [] = decompose' (getop' a SEPIMP)
  | otherwise = Cbexp SYM False (decompose' (getop' a SEPIMP)) AND (decompose' (getop' b AND))
decompose' (a, SEPIMP, b)
  | b == [] = decompose' (getop' a SEPCON)
  | otherwise = Cbexp SYM False (decompose' (getop' a SEPCON)) SEPIMP (decompose' (getop' b SEPIMP))
decompose' (a, SEPCON, b)
  | b == [] = decompose' (getop' a NOT) -- S a
  | otherwise = Cbexp SYM False (decompose' (getop' a NOT)) SEPCON (decompose' (getop' b SEPCON))
decompose' (a, NOT, b)
  | b == [] = decompose' (getop' a EXIST)
  | otherwise = Mbexp SYM False NOT (decompose' (getop' b EXIST))
decompose' (a, EXIST, b)
  | b == [] = decompose' (getop' a UNIV)
  | otherwise =
      let (c, d) = getvars b
      in Qbexp SYM False EXIST c (decompose' (getop' d UNIV))
decompose' (a, UNIV, b)
  | b == [] = decompose' (getop' a BRAC)
  | otherwise =
      let (c, d) = getvars b
      in Qbexp SYM False UNIV c (decompose' (getop' d BRAC))
decompose' (a, BRAC, b)
  | b == [] = S a
  | otherwise = decompose' (getop' b firstop)

-- Collects all the variables and the rest of the string
getvars :: String -> (VARLIST, String)
getvars [] = ([], "")
getvars xxs@('(':xs) = ([""] , xxs)
getvars (',':xs) = let (a, b) = getvars xs
                    in ("":a, b)
getvars (' ':xs) = getvars xs
getvars (x:xs) = let (a, b) = getvars xs
                  in ((x:(head a)):(tail a), b)

