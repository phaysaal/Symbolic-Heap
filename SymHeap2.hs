module Main
       where

import System.IO

main = do
  name <- getLine
  let str = decompose name
  print str

{-
Vars ::= String (variables)
E ::= nil | Vars (expressions) 
P ::= E=E | !P (simple pure formula) 
PI ::= True | P | PI & PI (pure formula) 
Fields ::= f|fN (fields)
RO ::= fN:E|RO,RO (record expressions) 
S ::= E=>[RO] (simple spatial formula)
SI ::= emp | S | SI * SI (spatial formula)
G ::= PI '|' SI (symbolic heap)

-}

{-
data A_EXP = Nil | Vars String deriving (Show)
data A_PURE = Equal A_EXP A_EXP | Not A_PURE deriving (Show)
data A_FORMULA = B Bool | Pure A_PURE | And A_FORMULA A_FORMULA deriving (Show)
data A_RECORD = Field String A_EXP | Record A_RECORD A_RECORD deriving (Show)
data A_CELL = MapsTo A_EXP A_RECORD deriving (Show)
data A_SPATIAL = Emp | Cell A_CELL | Heap A_SPATIAL A_SPATIAL deriving (Show)
data A_ASSERTION = Root A_FORMULA A_SPATIAL | ERROR deriving (Show)
-}

data A_NODE = Root | Heap | Pure | Simple | And | Not | Exp | Equal | Nil | Cell | MapsTo | Record | Field | Emp  deriving (Show)
data A_ASST = M A_NODE | T Bool | S String | U A_NODE A_ASST | B A_NODE A_ASST A_ASST  deriving (Show)

symbols :: [String]
symbols = ["nil", "!", "&", "=", "f", ":", "=>", "[", "]", "emp", "*", "|", ","]

get_prefix_terminal :: String -> (String, String)
get_prefix_terminal s = gpt s 3
                        where
                          gpt :: String -> Int -> (String, String)
                          gpt s 0 = ("", s)
                          gpt q k = if (take k s) `elem` symbols
                                    then
                                      ((take k s), (drop k s))
                                     else
                                      gpt s (k-1)

match_prefix :: String -> String -> (String, String)
match_prefix s key = let (a,b) = get_prefix_terminal s
                     in
                      if key == a
                      then
                        (a,b)
                      else
                        ("", a++b)


decompose x = parse_assertion x Root

-- String to split, Key, Bracket -> (pre, post)

splitFirst :: String -> String -> (String, String)
splitFirst a b = splitFirst' a b 0
                 
splitFirst' :: String -> String -> Int -> (String, String)
splitFirst' [] _ _ = ("","")
splitFirst' ('(':xs) key n = let (a,b) = splitFirst' xs key (n+1)
                             in ('(':a, b)
splitFirst' (')':xs) key n = let (a,b) = splitFirst' xs key (n-1)
                             in (')':a, b)
splitFirst' xs key 0 = let (a, b) = get_prefix_terminal xs
                       in
                        if a == key
                        then
                          ("", b)
                        else if a == ""
                             then
                               let (c, d) = splitFirst' (tail b) key 0
                               in ((head b):c, d)
                             else
                               let (c, d) = splitFirst' b key 0
                               in (a ++ c, d)
splitFirst' (x:xs) key n = let (y, z) = splitFirst' xs key n
                           in ((x:y), z)


remove_outer_bracket :: String -> String
remove_outer_bracket ('(':xs) = let (a, b) = rob xs 0 False
                                in
                                 if b
                                 then
                                   a
                                 else
                                   '(':a
                                 
  where
    rob :: String -> Int -> Bool -> (String, Bool)
    rob (')':[]) 0 m = ("", True)
    rob (')':[]) n m = (")", False)
    rob ('(':xs) n False = let (a,b) = rob xs (n+1) True
                           in ('(':a, b)
    rob (')':xs) n True =  let (a, b) = rob xs (n-1) True
                           in (')':a, b)
    rob (')':xs) n False = let (a, b) = rob xs n False
                           in (')':a, b)
    rob (x:xs) n b = let (a, c) = (rob xs n b)
                     in (x:a, c)
remove_outer_bracket xs = xs

parse_assertion :: String -> A_NODE -> A_ASST
parse_assertion str Root = let st = remove_outer_bracket (filter (\x->x /= ' ') str)
                               (a, b) = splitFirst st "|"
                               c = parse_assertion a Pure
                               d = parse_assertion b Heap
                           in B Root c d
parse_assertion "False" Pure = T False
parse_assertion "True" Pure = T True
parse_assertion str Pure = let st = remove_outer_bracket str
                               (a, b) = splitFirst st "&"
                           in if b == ""
                              then 
                                U Pure (parse_assertion a Simple)
                              else
                                B And (parse_assertion a Pure) (parse_assertion b Pure)
parse_assertion ('!':xs) Simple = U Not (parse_assertion xs Simple)
parse_assertion str Simple = let st = remove_outer_bracket str
                                 (a, b) = splitFirst st "="
                             in
                              B Equal (parse_assertion a Exp) (parse_assertion b Exp)
parse_assertion "nil" Exp = M Nil
parse_assertion xs Exp = S xs
parse_assertion "emp" Heap = M Emp
parse_assertion str Heap = let st = remove_outer_bracket str
                               (a, b) = splitFirst st "*"
                               c = parse_assertion a Cell
                           in 
                            if b == ""
                            then 
                              U Cell c 
                            else
                              B Heap (U Cell c) (parse_assertion b Heap)
parse_assertion str Cell = let st = remove_outer_bracket str
                               (a,b) = splitFirst st "=>"
                           in
                            B MapsTo (parse_assertion a Exp) (parse_assertion b Record)
parse_assertion ('[':xs) Record = let st = remove_outer_bracket xs
                        in parse_assertion (init st) Record
parse_assertion xs Record = let (b, c) = splitFirst xs ","
                                (d, e) = splitFirst b ":"
                                f = B Field (S d) (parse_assertion e Exp)
                            in
                             if c == ""
                             then
                               f
                             else
                               B Record f (parse_assertion c Record)
                     
{-
decompose :: String -> A_ASSERTION
decompose str = parse_symbolic_heap str ""

parse_symbolic_heap :: String -> String -> A_ASSERTION
parse_symbolic_heap str pre = 
  ATree
  (parse_pure_formula (reverse pre ""))
  (parse_spatial_formula post "")
parse_symbolic_heap (x:post) pre = parse_symbolic_heap post (x:pre)


splitMe :: String -> String -> [String]
splitMe [] _ = [""]
splitMe (x:xs) (x:[]) = let y = splitMe xs (x:[])
                        in ("":y)
splitMe (x:y:xs) (x:y:[]) = let y = splitMe xs (x:y:[])
                            in ("":y)
splitMe (x:xs) z = let (y:ys) = splitMe xs z
                   in ((x:y):ys)


-- outer bracket removal


parse_pure_formula :: String -> A_FORMULA
parse_pure_formula ("True") = True
parse_pure_formula ("true") = True
parse_pure_formula str = let l = splitFirst str "&"
                         in 


57010
01819-415421
yarayarasadi
sadi2321991


-}
