module PointerTriple
       (entails,validate,decompose_triple,T_TRIPLE,fresh_variable,cut_at)
       where

import SymHeapE
import PointerProgram
import Common

type T_TRIPLE = (A_ASSERTION, P_PROGRAM, A_ASSERTION)

decompose_triple :: String -> T_TRIPLE
decompose_triple xs = parse_triple (filter (\x->x /= ' ') xs)

parse_triple :: String -> T_TRIPLE
parse_triple xs = let (a, b) = cut_at xs '{' '}' 0
                      (c, d) = cut_at (reverse b) '}' '{' 0
                  in
                   ((decompose_assertion a),
                    (decompose_program (reverse d)),
                    (decompose_assertion (reverse c)))

validate_assertion :: A_ASSERTION -> A_ASSERTION -> Bool
validate_assertion a b = (a == b)

--


free_variables :: A_ASSERTION -> P_PROGRAM -> A_ASSERTION -> [String]
free_variables a p b = let afv = assertion_fv a
                           pfv = program_fv p
                           bfv = assertion_fv b
                       in
                        unionfv afv pfv

get_fresh :: String -> String
get_fresh "" = ""
get_fresh ('z':xs) = let ys = get_fresh xs
                     in
                      if ys == ""
                      then "za"
                      else 'z':ys
get_fresh (x:xs) = [(succ x)]


fresh_variable :: [String] -> String
fresh_variable [] = "a"
fresh_variable (x:_) = get_fresh x

assign :: A_ASSERTION -> String -> AP_EXP -> [String] -> (A_ASSERTION, [String])
assign ass x e fvs = let x' = fresh_variable fvs
                         e' = substitute_exp e x (V x')
                         ass' = substitute ass x (V x')
                         ass1 = ass_and ass' (Equal (V x) e')
                     in (ass1, x':fvs)



{-
evals :: T_TRIPLE -> (A_ASSERTION, A_ASSERTION, Bool)
evals (pre, pr, post) = ()
-}

lookup_h :: A_RECORD -> A_FIELD -> AP_VARS -> (A_RECORD, AP_EXP, AP_VARS)
lookup_h [] f fvs = let x' = fresh_variable fvs
                        in ([(f, V x')], (V x'), x':fvs)
lookup_h ((g, e):xs) f fvs
  | g == f = ((g, e):xs, e, fvs)
  | otherwise = let (a, b, c) = lookup_h xs f fvs
                in ((g, e):a, b, c)
{-
lookup_h p f fvs = let (b, a) = inrecord p f
                   in
                    if b
                    then (p, a)
                    else let x' = fresh_variable fvs
                         in ((f,(V x')):p, (V x'))
  where
    inrecord :: A_RECORD -> String -> (Bool, AP_EXP)
    inrecord [] _ = (False, (V ""))
    inrecord ((a,b):xs) c
      | a == c = (True, b)
      | otherwise = inrecord xs c
-}
{-
lookup_cell :: A_SPATIAL -> AP_EXP -> String -> [String] -> (A_SPATIAL, Maybe AP_EXP)
lookup_cell [] _ _ _ = ([], Nothing)
lookup_cell ((U pointer' record):xs) pointer field fvs
  | pointer' == pointer = let (record', exp, fvs') = lookup_h record field fvs  -- record' :: A_RECORD
                          in ((U pointer' record'):xs, Just exp)
  | otherwise = let (spatial', exp) = lookup_cell xs pointer field fvs
                in ((U pointer' record):spatial', exp)
lookup_cell (x:xs) pointer field fvs = let (a, b) = lookup_cell xs pointer field fvs
                                       in (x:a, b)
-}
mutate :: A_RECORD -> A_FIELD -> AP_EXP -> A_RECORD
mutate [] f e = [(f,e)]
mutate ((a,b):xs) f e
  | a == f = (f,e):xs
  | otherwise = let ys = mutate xs f e
                in (a,b):ys
    
{-
mutate_cell :: A_SPATIAL -> AP_EXP -> String -> AP_EXP -> Maybe A_SPATIAL
mutate_cell [] _ _ _ = Nothing
mutate_cell ((U pointer' record):xs) pointer field exp
  | pointer == pointer' = let record' = mutate record field exp
                          in Just ((U pointer' record'):xs)
  | otherwise = let r = mutate_cell xs pointer field exp
                in case r of
                (Just xs') -> Just ((U pointer' record):xs')
                Nothing -> Nothing
mutate_cell (x:xs) pointer field exp = let r = mutate_cell xs pointer field exp
                                       in case r of
                                       (Just xs') -> Just (x:xs')
                                       Nothing -> Nothing

dispose_cell :: A_SPATIAL -> AP_EXP -> A_SPATIAL
dispose_cell [] _ = []
dispose_cell ((U exp' f):xs) exp
  | exp' == exp = xs
  | otherwise = (U exp'  f):(dispose_cell xs exp)
dispose_cell (x:xs) exp = x:(dispose_cell xs exp)
-}

-- Search an expression in equalities in a formula and return its equaled expression
find_eq :: A_FORMULA -> AP_EXP -> Maybe (AP_EXP)
find_eq [] e = Nothing
find_eq ((Equal a b):xs) e
  | a == e = Just b
  | b == e = Just a
  | otherwise = find_eq xs e
find_eq (x:xs) e = find_eq xs e

predicate_replace :: A_CELL -> A_PTR -> AP_VARS -> (A_SPATIAL, AP_VARS)
--predicate_replace [] _ _ fvs = ([], fvs)
predicate_replace (U a b) d fvs = ([(U d b)], fvs)
predicate_replace (L a b) d fvs = let x' = fresh_variable fvs
                                  in ([(U d [("n", (V x'))]),(L (V x') b)], x':fvs)
predicate_replace (T a) d fvs = let x' = fresh_variable fvs
                                    y' = fresh_variable (x':fvs)
                                in ([(U d [("l",(V x')),("r",(V y'))]),(T (V x')),(T (V y'))], y':x':fvs)
                                

rearrange :: A_ASSERTION -> A_PTR -> AP_VARS -> Maybe (A_ASSERTION, AP_VARS)
rearrange (p,h) e fvs = let eqd = find_eq p e -- e isn't found, Search for equality to switch
                        in case eqd of
                        (Just a) -> let (x', xs') = split_heap h a -- a = e, search for a in heap
                                    in case x' of
                                    (Just (T y')) -> let bb = find_eq p y' -- NEED NORMALIZATION
                                                     in case bb of
                                                     (Just (N 0)) -> Nothing
                                                     _ -> let (zs', fvs') = predicate_replace (T y') e fvs
                                                          in Just ((p, zs' ++ xs'), fvs') -- a is found,
                                    (Just (L y' z')) -> if y' == z'
                                                        then Nothing
                                                        else
                                                          let (zs', fvs') = predicate_replace (L y' z') e fvs
                                                          in Just ((p, zs' ++ xs'), fvs') -- a is found,
                                    _ -> Nothing
                        _ -> Nothing
{-
rearrange (formula, heap) ptr fvs = let a = find_eq formula ptr -- try switch 
                                    in
                                     case a of
                                      (Just b) -> let (heap', fvs') = predicate_replace heap ptr b fvs -- Found switch and apply
                                                  in Just ((formula, heap'), fvs')
                                      Nothing ->  
-}

split_heap :: A_SPATIAL -> A_PTR -> (Maybe A_CELL, A_SPATIAL)
split_heap [] _ = (Nothing, [])
split_heap ((U a b):xs) e
  | a == e = (Just (U a b), xs)
  | otherwise = let (p, q) = split_heap xs e
                in (p, (U a b):q)
split_heap ((T a):xs) e
  | a == e = (Just (T a), xs)
  | otherwise = let (p, q) = split_heap xs e
                in (p, (T a):q)
split_heap ((L a b):xs) e
  | a == e = (Just (L a b), xs)
  | otherwise = let (p, q) = split_heap xs e
                in (p, (L a b):q)



-- precondition, program, fv, postcondition -> list of entailments
validate :: A_ASSERTION -> P_PROGRAM -> AP_VARS -> A_ASSERTION -> [(A_ASSERTION, A_ASSERTION)] 
validate pre (Empty) _ post = [(pre, post)] -- validate_assertion pre post
validate pre (Assignment var exp pr) fvs post = let (pre', fvs') = assign pre var exp fvs
                                                in
                                                 validate pre' pr fvs' post
validate (p,h) (Lookup x e f pr) fvs post = let (xx, xs) = split_heap h e -- Search the heap by original address
                                            in case xx of
                                            (Just (U ptr rc)) -> let (rc', fF, fvs') = lookup_h rc f fvs -- e is found, get modified record and lookuped value 
                                                                     x' = fresh_variable fvs'
                                                                     ex = Equal (V x) (substitute_exp fF x (V x'))
                                                                     ass = substitute (p, (U ptr rc'):xs) x (V x')
                                                                 in validate (ass_and ass ex) pr (x':fvs) post
                                            Nothing -> let r = rearrange (p,h) e fvs
                                                       in case r of
                                                       (Just (r', fvs')) -> validate r' (Lookup x e f pr) fvs' post
                                                       Nothing -> validate ([B True],[]) Empty fvs ([B False],[])
validate (p,h) (Mutation e1 f e2 pr) fvs post = let (x, xs) = split_heap h e1 -- Search the heap by original address
                                                in case x of
                                                (Just (U ptr rc)) -> let rc' = mutate rc f e2 -- e is found, get modified record and lookuped value 
                                                                     in validate (p, (U ptr rc'):xs) pr fvs post
                                                _ -> let r = rearrange (p,h) e1 fvs
                                                     in case r of
                                                     (Just (r', fvs')) -> validate r' (Mutation e1 f e2 pr) fvs post
                                                     Nothing -> validate ([B True],[]) Empty fvs ([B False],[])
validate (p,h) (Dispose e pr) fvs post = let (x, xs) = split_heap h e
                                         in case x of
                                         (Just (U ptr rc)) -> validate (p, xs) pr fvs post
                                         _ -> let r = rearrange (p,h) e fvs
                                              in case r of
                                              (Just (r', fvs')) -> validate r' (Dispose e pr) fvs post
                                              Nothing -> validate ([B True],[]) Empty fvs ([B False],[])
validate pre (New x pr) fvs post = let x' = fresh_variable fvs
                                       (a, b) = substitute pre x (V x')
                                   in
                                    validate (a, (U (V x) []):b) pr (x':fvs) post
validate (p,h) (If b p1 p2 pr) fvs post = (validate (ass_and (p,h) b) (compose p1 pr) fvs post) ++ (validate (ass_and (p,h) (Not b)) (compose p2 pr) fvs post)


compose :: P_PROGRAM -> P_PROGRAM -> P_PROGRAM
compose Empty p = p
compose (Assignment var exp pr) p = Assignment var exp (compose pr p)
compose (Lookup x e f pr) p = Lookup x e f (compose pr p)
compose (Mutation e1 f e2 pr) p = Mutation e1 f e2 (compose pr p)
compose (Dispose e pr) p = Dispose e (compose pr p)
compose (New x pr) p = New x (compose pr p)
compose (If b p1 p2 pr) p = If b p1 p2 (compose pr p)

entails :: T_TRIPLE -> [(A_ASSERTION, A_ASSERTION)]
entails (a, p, b) = let fvs = free_variables a p b
                    in (validate a p fvs b)


{-
validate (p,h) (Lookup x e f pr) fvs post = let (h', ff') = lookup_cell h e f fvs  -- lookup(ro,f) = (ro',F) => Sigma * E->[ro'] = h' = Sigma'
                                            in
                                             case ff' of
                                             Nothing -> let (r', fvs') = rearrange (p,h) e fvs
                                                        in validate r' (Lookup x e f pr) fvs' post
                                             (Just f') -> let x' = fresh_variable fvs           -- fresh x' 
                                                              f1 = substitute_exp f' x (V x')       -- F[x'/x] where F=f' (f'::AP_EXP, x::String, )
                                                              (r,g) = substitute (p, h') x (V x')   -- (p, h')[x'/x]
                                                              r' = ass_and (r,g) (Equal (V x) f1) -- x=F[x'/x] & (...)[x'/x]
                                                          in validate r' pr (x':fvs) post
-}
