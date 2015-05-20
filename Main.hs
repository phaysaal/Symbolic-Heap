module Main
       where

import PointerTriple
import SymHeapE

main = do
  check_it
  --line <- getLine
  --let tree = PointerTriple.decompose_triple line
  --print tree

check_it = check input


check [] = do
  putStrLn "End"
check (x:xs) = do
  let y = entails (decompose_triple x)
  let z = pr y
  putStrLn (x ++ " ===> \n")
  putStrLn (z ++ "\n\n")
  check xs
  

pr ((x,y):[]) = (ass_to_string x) ++ "\n -> \n" ++ (ass_to_string y) 
pr ((x,y):xs) = (ass_to_string x) ++ "\n -> \n" ++ (ass_to_string y) ++ "\n and \n" ++ (pr xs)

input = ["{x=a}x:=b;{x=b}",
         "{x=a}y:=b;{x=a & y=b}",
         "{x=a}x:=x;{x=a}",
         "{x=a}x:=b;x:=c;{x=c}",
         "{x=a}x:=x+1;{x=a+1}",
         "{x=a}x:=x+1;x:=x+1;{x=a+1+1}",
         "{x=a}       x:=x+1;        y:=x+1;        {x=a+1 & y=a+1+1}",
         "{| x=>[id:a,name:b]}y:=x->id;{y=a| x=>[id:a,name:b]}",
         -- new
         "{|}new(x);{|x=>[]}",
         "{|x=>[a:y,b:z]}new(w);{|x=>[a:y,b:z]*w=>[]}",
         "{p=q|x=>[a:y,b:z]}new(w);{p=q|x=>[a:y,b:z]*w=>[]}",
         -- lookup
         "{p=q | x=>[a:y,b:z]}w:=x->b;{p=q & w=z | x=>[a:y,b:z]}",
         "{p=q | x=>[a:y,b:s]}w:=x->c;{p=q & w=z | x=>[a:y,b:s,c:z]}",
         "{p=q | q=>[a:e]*x=>[a:y,b:s]}w:=x->c;{p=q & w=z | q=>[a:e]*x=>[a:y,b:s,c:z]}",
         "{p=q | q=>[a:e]*x=>[a:y,b:z]}w:=p->a;{p=q & w=e | q=>[a:e]*x=>[a:y,b:z]}",
         "{p=q | r=>[a:e]*x=>[a:y,b:z]}w:=p->a;{False|}",
         -- mutation
         "{p=q | x=>[a:y,b:z]}x->b := f;{p=q | x=>[a:y,b:f]}",
         "{p=q | x=>[a:y,b:s]}x->c := g;{p=q | x=>[a:y,b:s,c:g]}",
         "{p=q | q=>[a:e]*x=>[a:y,b:s]}x->c := p; x->b := q;{p=q | q=>[a:e]*x=>[a:y,b:q,c:p]}",
         "{p=q | q=>[a:e]*x=>[a:y,b:z]}p->a:=12;q->b:=30;{p=q | q=>[a:12,b:30]*x=>[a:y,b:z]}",
         "{p=q | q=>[a:e]*x=>[a:y,b:z]}p->a:=12;r->b:=30;{p=q | q=>[a:12,b:30]*x=>[a:y,b:z]}",
         --dispose
         "{p=q | x=>[a:y,b:s]}dispose(x);{p=q | }",
         "{p=q | q=>[a:e]*x=>[a:y,b:s]}dispose(x);{p=q | q=>[a:e]}",
         "{p=q | q=>[a:e]*x=>[a:y,b:z]}dispose(x);dispose(q);{p=q | }",
         "{p=q | q=>[a:e]*x=>[a:y,b:z]}dispose(p);x->b:=30;{p=q | x=>[a:y,b:30]}",
         "{a=b & z=0|tree(w)}new(x);x->l:=z;x->r:=0;x->v:=a;new(y);{a=b & z=0 | y => [] * x => [l:z, r:0, v:a] * tree(w)}",
         "{a=b & z=0|tree(a)}b->l:=z;{a=b & z=0 | b => [l:z, r:zb] * tree(za) * tree(zb)}",
         "{a=z & z=0|tree(b)}z->l:=z;{a=b & z=0 | tree(b)}",
         "{a=b|q=>[]}if (a=b) {x:=y;}{x:=z;new(z);}{a=b & x=y|q=>[]}",
         "{a=b|q=>[]}if (a=c) {x:=y;}{x:=z;new(z);}{a=b & x=z|q=>[]*z=>[]}",
         "{v=v' | e=>[a:v'']}v:=e->a{v=v'' | e'=>[a:v'']}"
        ]
