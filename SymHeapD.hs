module SymHeapD
       (decompose_assertion,
        A_ASSERTION,
        A_SPATIAL,
        A_FORMULA,
        A_CELL,
        A_RECORD,
        ass_and,
        substitute,
        assertion_fv,
        parse_a_cell)
       where

import System.IO
import Common

{-
main = do
  name <- getLine
  let str = decompose_assertion name
  print str
-}
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


-- type AP_EXP = String


type A_FORMULA = [AP_PURE]
type A_RECORD = [(String, AP_EXP)]
type A_CELL = (AP_EXP, A_RECORD)
type A_SPATIAL = [A_CELL]
type A_ASSERTION = (A_FORMULA, A_SPATIAL)


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


decompose_assertion x = parse_symbolic_heap x

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

parse_symbolic_heap :: String -> A_ASSERTION
parse_symbolic_heap str = let st = remove_outer_bracket (filter (\x->x /= ' ') str)
                              (a, b) = splitFirst st "|"
                              c = parse_pure_formula a
                              d = parse_spatial_formula b
                          in 
                              (c, d)

parse_pure_formula :: String -> A_FORMULA
parse_pure_formula "" = []
parse_pure_formula str = let st = remove_outer_bracket str
                             (a, b) = splitFirst st "&"
                         in 
                          (parse_simple_pure a):(parse_pure_formula b)

parse_simple_pure :: String -> AP_PURE
parse_simple_pure ("true") = B True
parse_simple_pure ("false") = B False
parse_simple_pure ('!':xs) = Not (parse_simple_pure xs)
parse_simple_pure str = let st = remove_outer_bracket str
                            (a, b) = splitFirst st "="
                        in
                         Equal (parse_exp a) (parse_exp b)

{-
parse_expression :: String -> AP_EXP
parse_expression ("nil") = ""
parse_expression xs = xs
-}

parse_spatial_formula :: String -> A_SPATIAL
parse_spatial_formula "" = []
parse_spatial_formula "emp" = []
parse_spatial_formula str = let st = remove_outer_bracket str
                                (a, b) = splitFirst st "*"
                                c = parse_a_cell a
                            in 
                             c : (parse_spatial_formula b)

parse_a_cell :: String -> A_CELL
parse_a_cell str = let st = remove_outer_bracket str
                       (a,b) = splitFirst st "=>"
                   in
                    ((parse_exp a), (parse_record b))

parse_record :: String -> A_RECORD
parse_record "" = []
parse_record ('[':xs) = if (last xs) == ']'
                        then parse_record (init xs)
                        else parse_record xs
parse_record xs = let 
                      (b, c) = splitFirst xs ","
                      (d, e) = splitFirst b ":"
                  in
                    (d, (parse_exp e)): (parse_record c)
                   

substitute :: A_ASSERTION -> String -> AP_EXP -> A_ASSERTION
substitute (a, b) var exp = let c = substitute_formula a var exp
                                d = substitute_symheap b var exp
                            in
                             (c, d)

substitute_formula :: A_FORMULA -> String -> AP_EXP -> A_FORMULA
substitute_formula [] _ _ = []
substitute_formula (x:xs) var exp = (substitute_pure x var exp):(substitute_formula xs var exp)

substitute_pure :: AP_PURE -> String -> AP_EXP -> AP_PURE
substitute_pure (B b) _ _ = B b  
substitute_pure (Equal a b) var exp = Equal (substitute_exp a var exp) (substitute_exp b var exp)
substitute_pure (Not a) var exp = Not (substitute_pure a var exp) 

substitute_symheap :: A_SPATIAL -> String -> AP_EXP -> A_SPATIAL
substitute_symheap x _ _ = x

substitute_cell :: A_CELL -> String -> AP_EXP -> A_CELL
substitute_cell (a,b) var exp = (substitute_exp a var exp, substitute_record b var exp)

substitute_record :: A_RECORD -> String -> AP_EXP -> A_RECORD
substitute_record [] _ _ = []
substitute_record ((a, b):xs) var exp = (a, substitute_exp b var exp):(substitute_record xs var exp) 


{-
The following definition may need to be extended if construction of exp is changed in future
-}

ass_and :: A_ASSERTION -> AP_PURE -> A_ASSERTION
ass_and (a, b) c = (c:a, b)


assertion_fv :: A_ASSERTION -> [String]
assertion_fv (a, b) = unionfv (fv_formula a) (fv_symheap b)

fv_formula :: A_FORMULA -> [String]
fv_formula [] = []
fv_formula (x:xs) = unionfv (fv_pure x) (fv_formula xs)

fv_pure :: AP_PURE -> [String]
fv_pure (B b) = []
fv_pure (Equal a b) = unionfv (fv_exp a) (fv_exp b)

fv_symheap :: A_SPATIAL -> [String]
fv_symheap [] = []
fv_symheap (x:xs) = unionfv (fv_cell x) (fv_symheap xs)

fv_cell :: A_CELL -> [String]
fv_cell (a, b) = unionfv (fv_exp a) (fv_record b)

fv_record :: A_RECORD -> [String]
fv_record [] = []
fv_record ((a,b):xs) = unionfv (fv_exp b) (fv_record xs)

