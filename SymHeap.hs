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


data A_EXP = Nil | Vars String deriving (Show)
data A_PURE = Equal A_EXP A_EXP | Not A_PURE deriving (Show)
data A_FORMULA = B Bool | Pure A_PURE | And A_FORMULA A_FORMULA deriving (Show)
data A_RECORD = Field String A_EXP | Record A_RECORD A_RECORD deriving (Show)
data A_CELL = MapsTo A_EXP A_RECORD deriving (Show)
data A_SPATIAL = Emp | Cell A_CELL | Heap A_SPATIAL A_SPATIAL deriving (Show)
data A_ASSERTION = Root A_FORMULA A_SPATIAL | ERROR deriving (Show)


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


decompose x = parse_symbolic_heap x

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

{-
trim :: String -> String
trim xs = trim' xs False
  where
    trim' :: String -> Bool -> String
    trim' "" _ = ""
    trim' (' ':xs) False = let a = trim' xs False
                           in
                            if a == ""
                            then
                              a
                            else
                              ' ':a
    trim' (' ':xs) True = ' ':(trim' xs True)
    trim' (x:xs) b = x:(trim' xs b)
-}  

parse_symbolic_heap :: String -> A_ASSERTION
parse_symbolic_heap str = let st = remove_outer_bracket (filter (\x->x /= ' ') str)
                              (a, b) = splitFirst st "|"
                              c = parse_pure_formula a
                              d = parse_spatial_formula b
                          in 
                              Root c d

parse_pure_formula :: String -> A_FORMULA
parse_pure_formula "False" = B False
parse_pure_formula "True" = B True
parse_pure_formula str = let st = remove_outer_bracket str
                             (a, b) = splitFirst st "&"
                         in 
                          if b == ""
                          then 
                            Pure (parse_simple_pure a)
                          else
                            And (parse_pure_formula a) (parse_pure_formula b)

parse_simple_pure :: String -> A_PURE
parse_simple_pure ('!':xs) = Not (parse_simple_pure xs)
parse_simple_pure str = let st = remove_outer_bracket str
                            (a, b) = splitFirst st "="
                        in
                         Equal (parse_expression a) (parse_expression b)

parse_expression :: String -> A_EXP
parse_expression "nil" = Nil
parse_expression xs = Vars xs

parse_spatial_formula :: String -> A_SPATIAL
parse_spatial_formula "emp" = Emp
parse_spatial_formula str = let st = remove_outer_bracket str
                                (a, b) = splitFirst st "*"
                                c = parse_a_cell a
                            in 
                             if b == ""
                             then 
                               Cell c 
                             else
                               Heap (Cell c) (parse_spatial_formula b)

parse_a_cell :: String -> A_CELL
parse_a_cell str = let st = remove_outer_bracket str
                       (a,b) = splitFirst st "=>"
                   in
                    MapsTo (parse_expression a) (parse_record b)

parse_record :: String -> A_RECORD
parse_record ('[':xs) = let st = remove_outer_bracket xs
                        in parse_record (init st)
parse_record xs = let 
                      (b, c) = splitFirst xs ","
                      (d, e) = splitFirst b ":"
                      f = Field (d) (parse_expression e)
                  in
                   if c == ""
                   then
                     f
                   else
                     Record f (parse_record c)
                           
                            
                         

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
