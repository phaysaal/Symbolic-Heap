module Common(
  AP_EXP(..),
  AP_PURE(..),
  AP_VARS,
  unionfv,
  fv_exp,
  parse_exp,
  substitute_exp,
  isvar
  )
       where

--import Data.String.Utils

type AP_VARS = [String]

data AP_EXP = V String | N Integer | P AP_EXP AP_EXP
instance Show AP_EXP where
  show (V s) = s
  show (N n) = show n
  show (P a b) = (show a) ++ ('+':(show b))

instance Eq AP_EXP where
  (==) (V a) (V b) = a == b
  (==) (N a) (N b) = a == b
  (==) (P a b) (P c d) = (a == c && b == d) || (a == d && b == c)
  (==) a b = False
  (/=) a b = not (a == b)

data AP_PURE = B Bool | Equal AP_EXP AP_EXP | Not AP_PURE
instance Show AP_PURE where
  show (B b) = show b
  show (Not a) = "!(" ++ (show a) ++ ")"
  show (Equal a b) = (show a) ++ "=" ++ (show b)
  
instance Eq AP_PURE where
  (==) (B a) (B b) = a == b
  (==) (Equal a b) (Equal c d) = ((a == c) && (b == d)) || ((a == d) && (b == c))
  (==) (Not a) (Not b) = a == b
  (==) (Equal a b) (B c) = (a == b) && c
  (==) (B a) (Equal b c) = a && (b == c)
  (==) a b = False
  (/=) a b = not (a == b)
  

unionfv :: AP_VARS -> AP_VARS -> AP_VARS
unionfv [] [] = []
unionfv [] b = b
unionfv a [] = a
unionfv (x:xs) (y:ys) = if x == y
                        then x:(unionfv xs ys)
                        else if x > y
                             then x:(unionfv xs (y:ys))
                             else y:(unionfv (x:xs) ys)
                                  

parse_exp_cons :: [String] -> AP_EXP
parse_exp_cons (x:[])
  | isvar x = V x
  | otherwise = N (read x)
parse_exp_cons (x:xs) = P (parse_exp_cons [x]) (parse_exp_cons xs)

parse_exp :: String -> AP_EXP
parse_exp xs = let ys = split '+' xs
               in parse_exp_cons ys
  

isvar :: String -> Bool
isvar s = case reads s :: [(Integer, String)] of
  [(_, "")] -> False
  _ -> True

{-
isvar "" = False
isvar (x:xs) 
  | (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') = True
  | otherwise = isvar xs
-}

split :: Char -> String -> [String]
split _ "" = [""]
split y (x:xs) = let (z:zs) = split y xs
                 in
                  if x == y
                  then "":z:zs
                  else (x:z):zs

fv_exp :: AP_EXP -> AP_VARS
fv_exp (V a) = [a]
fv_exp (N a) = []
fv_exp (P a b) = unionfv (fv_exp a) (fv_exp b)

-- fv_exp x = filter (\y->isvar y) (split '+' x)

{-
isinit :: String -> String -> String
isinit xs "" = xs
isinit "" _ = ""
isinit (x:xs) (y:ys)
  | x == y = isinit xs ys
  | otherwise = ""

subs_str :: String -> String -> String -> String
subs_str "" a b = ""
subs_str xs a b = let ys = isinit xs a
                  in
                   if ys == ""
                   then (head xs):(subs_str (tail xs) a b)
                   else b ++ (subs_str ys a b)
-}

substitute_exp :: AP_EXP -> String -> AP_EXP -> AP_EXP
substitute_exp c@(V x) a b
  | x == a = b
  | otherwise = c
substitute_exp c@(N x) _ _ = c
substitute_exp c@(P x y) a b = P (substitute_exp x a b) (substitute_exp y a b)


{-
substitute_exp a b c = if a == b
                       then
                         c
                       else
                         a
-}
{-
unionfv :: [String] -> [String] -> [String]
unionfv [] [] = []
unionfv [] b = b
unionfv a [] = a
unionfv (x:xs) b = if x `elem` b
                   then unionfv xs b
                   else x:(unionfv xs b)

-}
