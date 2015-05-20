module Main
       where

import System.IO

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")
  

signum x =
  if x < 0
     then -1
  else if x > 0
       then 1
       else 0
            
f x =
  case x of
   0 -> 1
   1 -> 5
   2 -> 2
   _ -> -1

g x =
  case x of { 0 -> 1; 1 -> 5 ; 2 -> 2 ; _ -> 1 }

roots a b c =
  ((-b + sqrt(b*b - 4*a*c)) / (2*a),
   (-b - sqrt(b*b - 4*a*c)) / (2*a))

rootsa a b c =
  let det = sqrt (b*b - 4*a*c)
  in ((-b + det) / (2*a),
      (-b - det) / (2*a))

my_length [] = 0
my_length (x:xs) = 1 + my_length xs

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node 'P'
    ( Node 'O'
      ( Node 'L'
        ( Node 'N' Empty Empty)
        ( Node 'T' Empty Empty)
      )
      Empty
    )
    ( Node 'R' Empty Empty)

printtree :: Tree Char -> Int
printtree Empty = 1
printtree (Node a b c) = 1 + printtree b + printtree c
                         




               
