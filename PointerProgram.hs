module PointerProgram
       (decompose_program,
        cut_at,
        program_fv,
        P_PROGRAM(..)
       )
       where

import System.IO
import Common

{-
main = do
  inp <- getLine
  let out = tolatex (decompose_program inp)
  putStrLn out
-}

-- data P_EXP = Var String | Nil deriving (Show)
-- data P_COMP = Equal AP_EXP AP_EXP | Not P_COMP deriving (Show)
--data P_ASS = P_ASS String AP_EXP deriving (Show)
data P_PROGRAM = Empty
            | Assignment String AP_EXP P_PROGRAM -- variable, expression
            | Lookup String AP_EXP String P_PROGRAM -- variable, expression, field
            | Mutation AP_EXP String AP_EXP P_PROGRAM -- expression, field, expression
            | New String P_PROGRAM
            | Dispose AP_EXP P_PROGRAM
            | If AP_PURE P_PROGRAM P_PROGRAM P_PROGRAM
              deriving (Show)


decompose_program :: String -> P_PROGRAM
decompose_program xs = parse_program (filter (\x->x /= ' ' && x /= '\t' && x /= '\n') xs)


splitAtSC :: String -> (String, String)
splitAtSC "" = ("", "")
splitAtSC (';':xs) = ("", xs)
splitAtSC (x:xs) = let (a, b) = splitAtSC xs
                   in (x:a, b)
{-
  where
    splitAtSC' :: String -> Int -> (String, String)
    splitAtSC' [] _ = ("","")
    splitAtSC' (';':xs) 0 = ("", xs)
    splitAtSC' (x:xs) n
      | x == '{' = let (a, b) = splitAtSC' xs (n+1)
                   in (x:a, b)
      | x == '}' = let (a, b) = splitAtSC' xs (n-1)
                   in (x:a, b)
      | otherwise = let (a, b) = splitAtSC' xs x
                    in (x:a, b)
-}

drop_semicolon :: String -> String
drop_semicolon [] = []
drop_semicolon (';':xs) = xs
drop_semicolon xs = xs
                       
parse_program :: String -> P_PROGRAM
parse_program "" = Empty
parse_program ('n':'e':'w':'(':xs) = let (x,y) = cut_at ('(':xs) '(' ')' 0 
                                     in New x (parse_program (drop_semicolon y))
parse_program ('d':'i':'s':'p':'o':'s':'e':'(':xs) = let (x,y) = cut_at  ('(':xs) '(' ')' 0
                                                     in Dispose (parse_exp x) (parse_program (drop_semicolon y))
parse_program ('i':'f':'(':xs) = let (a, b, c, d) =  splitForIf ('(':xs)
                                 in If (parse_condition a "") (parse_program b) (parse_program c) (parse_program (drop_semicolon d))
parse_program xs = let (a, b) = splitAtSC xs -- a is the current, b is rest
                       (c, d) = splitAtAss a -- c is before assignment
                   in
                    if ('>' `elem` c)
                    then   -- mutation
                      let (e, f) = splitAtMaps c
                      in Mutation (parse_exp e) f (parse_exp d) (parse_program b)
                    else if ('>' `elem` d)   
                         then   -- lookup
                           let (e, f) = splitAtMaps d
                           in Lookup c (parse_exp e) f (parse_program b)
                         else   -- assignment
                           Assignment c (parse_exp d) (parse_program b)



parse_condition :: String -> String -> AP_PURE
parse_condition "True" "" = B True
parse_condition "False" "" = B False
parse_condition ('!':xs) "" = Not (parse_condition xs "")
parse_condition ('=':xs) ys = Equal (parse_exp (reverse ys)) (parse_exp xs)
parse_condition (x:xs) ys = parse_condition xs (x:ys)


splitAtMaps :: String -> (String, String)
splitAtMaps "" = ("", "")
splitAtMaps ('-':'>':xs) = ("", xs)
splitAtMaps (x:xs) = let (a, b) = splitAtMaps xs
                     in (x:a, b)

splitAtAss :: String -> (String, String)
splitAtAss "" = ("", "")
splitAtAss (':':'=':xs) = ("", xs)
splitAtAss (x:xs) = let (a, b) = splitAtAss xs
                    in (x:a, b)
                       

splitForIf :: String -> (String, String, String, String)
splitForIf xs = let (a, b) = cut_at xs '(' ')' 0
                    (c, d) = cut_at b '{' '}' 0
                    (e, f) = cut_at d '{' '}' 0
                in (a, c, e, f)

{-
cut_at :: String -> Char-> Char -> Int -> (String, String)
cut_at (x:xs) l r n = if n == 0 && x == l
                      then cut_at xs l r 1
                      else if n == 1 && x == r
                           then ("", xs)
                           else if x == l
                                then let (a, b) = cut_at xs l r (n+1)
                                     in ('(':a, b)
                                else if x == r
                                     then let (a, b) = cut_at xs l r (n-1)
                                          in ('(':a, b)
                                     else let (a, b) = cut_at xs l r n
                                          in ('(':a, b)
-}      

cut_at :: String -> Char-> Char -> Int -> (String, String)
cut_at (x:xs) l r 0
  | x == l = cut_at xs l r 1
  | otherwise = cut_at xs l r 0
cut_at (x:xs) l r 1
  | x == r = ("", xs)
  | otherwise = let (a, b) = cut_at xs l r 1
                in (x:a, b)
cut_at (x:xs) l r n
  | x == l = let (a, b) = cut_at xs l r (n+1)
             in (l:a, b)
  | x == r = let (a, b) = cut_at xs l r (n-1)
             in (r:a, b)
  | otherwise = let (a, b) = cut_at xs l r n
                in (x:a, b)

{-
tolatex :: P_PROGRAM -> String
tolatex p = "\\tikz[font=\footnotesize,
grow=right, level 1/.style={sibling distance=6em},
level 2/.style={sibling distance=1em}, level distance=5cm]
\\node" ++ (drop 4 (tolatex' p)) ++ ";" 
-}

tolatex :: P_PROGRAM -> String
tolatex p = "\\tikz[grow=right,level 1/.style={sibling distance=6cm},level 2/.style={sibling distance=3in}, level distance=2cm]\\node" ++ (drop 4 (tolatex' p)) ++ ";" 

tolatex' :: P_PROGRAM -> String
tolatex' (Empty) = "node {Skip}"
tolatex' (Assignment var exp pr) = "node {Ass} child { node{" ++ var ++ ":=" ++ (show exp) ++ "}} child {" ++ (tolatex' pr) ++ "}"
tolatex' (Lookup var pointer field pr) = "node {Lookup} child {node {" ++ var ++ ":=" ++ (show pointer) ++ "$->$" ++ field ++ "}} child {" ++ (tolatex' pr) ++ "}"
tolatex' (Mutation pointer field exp pr) = "node {Mutation} child {node {" ++ (show pointer) ++ "$->$" ++ field ++ ":=" ++ (show exp) ++ "}} child {" ++ (tolatex' pr) ++ "}"
tolatex' (New var pr) = "node {New} child {node {new(" ++ var ++ ")}} child {" ++ (tolatex' pr) ++ "}"
tolatex' (Dispose exp pr) = "node {Dispose} child {node {dispose(" ++ (show exp) ++ ")}} child {" ++ (tolatex' pr) ++ "}"
tolatex' (If comp pa pb pr) = "node {;} child{ node {if} child {node {" ++ (show comp) ++ "}} child {" ++ (tolatex' pa) ++ "} child {" ++ (tolatex' pb) ++ "}} child {" ++ (tolatex' pr) ++ "}" 

program_fv :: P_PROGRAM -> [String]
program_fv (Empty) = []
program_fv (Assignment var exp pr) = unionfv (unionfv [var] (fv_exp exp)) (program_fv pr)
program_fv (Lookup var exp field pr) = unionfv (unionfv [var] (fv_exp exp)) (program_fv pr)
program_fv (Mutation exp1 field exp2 pr) = unionfv (unionfv (fv_exp exp1) (fv_exp exp2)) (program_fv pr)
program_fv (New var pr) = unionfv [var] (program_fv pr)
program_fv (Dispose exp pr) = unionfv (fv_exp exp) (program_fv pr)
program_fv (If b p1 p2 pr) = unionfv (unionfv (unionfv (fv_comp b) (program_fv p1)) (program_fv p2)) (program_fv pr)

fv_comp :: AP_PURE -> [String]
fv_comp (B _) = [] 
fv_comp (Not a) = fv_comp a
fv_comp (Equal x1 x2) = unionfv (fv_exp x1) (fv_exp x2)


