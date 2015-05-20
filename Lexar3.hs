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

data OP = IMP | OR | AND | SIM | SCO | EXI | UNI | NOT | EQU | LET | GRT | NEQ | MAP | BRAC | ADD | SUB | MUL | DIV  deriving (Enum, Eq) -- arithmetic operators +, -, *, /, %

instance Show OP where
  show ADD = "+"
  show SUB = "-"
  show MUL = "*"
  show DIV = "/"
  show EQU = "="
  show LET = "<"
  show GRT = ">"
  show NEQ = "!="
  show IMP = "->"
  show OR  = "||"
  show AND = "&&"
  show SIM = "-*"
  show SCO = "**"
  show EXI = "^E"
  show UNI = "^A"
  show NOT = "!"
  show MAP = "=>"
  show BRAC = "("


data EXP = V Int
         | S String
         | C EXP OP EXP

instance Show EXP where
  show (V a) = "(" ++ (show a) ++ ")"
  show (S a) = "(" ++ a ++ ")"
  show (C a b c) = "(" ++ (show a) ++ (show b) ++ (show c) ++ ")"
                                 
data BEXP = EMPTY
          | B Bool
          | E EXP OP EXP
          | N OP BEXP
          | L BEXP OP BEXP
          | Q OP VARLIST BEXP
instance Show BEXP where
  show EMPTY = "emp"
  show (B b) = show b
  show (N a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (E a b c) = "(" ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
  show (L a b c) = "(" ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
  show (Q a b c) = "(" ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"

-- expression -> number of ( brackets encountered -> current operator to search
getop :: String -> Int -> OP -> (String, OP, String)
getop [] 0 op = ([], op, []) -- go for exp
getop (x:[]) 0 op = ([x], op, [])
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
  | (show op) == x:[y] && n == 0 = ([], op, xs)
  | (show op) == [x] && (uniqueop (x:[y]) firstop) && n == 0 = ([], op, y:xs)
  | otherwise                    = let (a, b, c) = getop (y:xs) n op
                                   in (x:a, b, c)

uniqueop :: String -> OP -> Bool
uniqueop x DIV = x /= (show DIV)
uniqueop x z = ((show z) /= x) &&  (uniqueop x (succ z))

getop' ::  String -> OP -> (String, OP, String)
getop' s op = getop s 0 op 

firstop :: OP
firstop = IMP


decompose :: String -> BEXP
decompose s = decompose' (getop' s firstop)


decompose' :: (String, OP, String) -> BEXP
decompose' ("emp", _, []) = EMPTY
decompose' (a, BRAC, b)
  | b == [] = B (read a)
  | otherwise = decompose' (getop' b firstop)
decompose' (a, lop, []) = decompose' (getop' a (succ lop))
decompose' (a, lop, b) 
  | lop == NOT = N NOT (decompose' (getop' b (succ lop)))
  | lop `elem` [EQU, NEQ, LET, GRT] = E (decomexp (getop' a ADD)) lop (decomexp (getop' b ADD))
  | lop == UNI || lop == EXI =  let (c, d) = getvars b
                                in Q lop c (decompose' (getop' d (succ lop)))
  | otherwise = L (decompose' (getop' a (succ lop))) lop (decompose' (getop' b lop))


isnumber :: String -> Bool
isnumber [] = False
isnumber (x:[])
  | x >= '0' && x <= '9' = True
  | otherwise = False
isnumber (x:xs)
  | x >= '0' && x <= '9' = isnumber xs
  | otherwise = False
                
decomexp :: (String, OP, String) -> EXP
decomexp (a, BRAC, b)
  | b == [] && (isnumber a) = V (read a :: Int)
  | b == [] = S a
  | otherwise = decomexp (getop' b ADD)
decomexp (a, DIV, b)
  | b == [] = decomexp (getop' a BRAC)
  | otherwise = C (decomexp (getop' a BRAC)) DIV (decomexp (getop' b DIV))
decomexp (a, op, []) = decomexp (getop' a (succ op))
decomexp (a, op, b) = C (decomexp (getop' a (succ op))) op (decomexp (getop' b op))


-- Collects all the variables and the rest of the string
getvars :: String -> (VARLIST, String)
getvars [] = ([], "")
getvars xxs@('(':xs) = ([""] , xxs)
getvars (',':xs) = let (a, b) = getvars xs
                    in ("":a, b)
getvars (' ':xs) = getvars xs
getvars (x:xs) = let (a, b) = getvars xs
                  in ((x:(head a)):(tail a), b)


aeval :: Int -> OP -> Int -> Int
aeval x ADD y = x + y
aeval x SUB y = x - y
aeval x MUL y = x * y
aeval 0 DIV y = 0
aeval x DIV y 
  | x < y = 0
  | otherwise = (aeval (x-y) DIV y) + 1

expeval :: EXP -> EXP
expeval (V x) = V x
expeval (S x) = S x
expeval (C (S "") SUB (V x)) = V (-x)                   -- "" - x = -x
expeval (C (V x) op (V y)) = V (aeval x op y)           -- eval
expeval (C (S x) MUL (V y)) = C (V y) MUL (S x)
expeval (C x op y) = let a = expeval x
                         b = expeval y
                        in
                         case (a,b) of
                          ((V z), (V w)) -> V (aeval z op w)
                          _ -> C a op b

-- 4*(x-y) = 4x - 4y

beval :: Int -> OP -> Int -> Bool
beval a EQU b = a == b
beval a LET b = a < b
beval a GRT b = a > b
beval a NEQ b = (a /= b)

  
bexpeval :: BEXP -> BEXP
bexpeval (B b) = B b
bexpeval (E a op b) = let c = expeval a
                          d = expeval b
                      in
                       case (c,d) of
                        ((V w), (V z)) -> B (beval w op z)
                        _ -> E c op b
bexpeval (N NOT (B True)) = B False
bexpeval (N NOT (B False)) = B True
bexpeval (L a IMP b) = let c = bexpeval a
                           d = bexpeval b
                      in
                       case (c,d) of
                        ((B False), _) -> B True
                        ((B True), _)  -> d
                        _ -> L c IMP d 
bexpeval (L a AND b) = let c = bexpeval a
                           d = bexpeval b
                      in
                       case (c,d) of
                        ((B True), _) -> d
                        (_, (B True))  -> c
                        ((B False), (B False)) -> B False
                        _ -> L c AND d 
bexpeval (L a OR b) = let c = bexpeval a
                          d = bexpeval b
                      in
                       case (c,d) of
                        ((B False), _) -> d
                        (_, (B False))  -> c
                        ((B True), (B True)) -> B True
                        _ -> L c OR d 
bexpeval (Q op vl (B a)) = B a
bexpeval a = a 


sample1 = "True && False"
sample2 = "2=1+1 || x=y"
sample3 = "False -> x=x+1 && (yy=xx+zz -> z=1+x)"


{-
leval :: Bool -> OP -> Bool -> Bool
leval a IMP b
  | a == False = True
  | b == False = False
  | otherwise = True

-}
