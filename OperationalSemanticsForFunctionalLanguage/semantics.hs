--------------------------------------------------------------------      -- 001
-- Operational Semantics for a Functional Language with side effects      -- 002
--------------------------------------------------------------------      -- 003
                                                                          -- 004
                                                                          -- 005
-- variables are just names                                               -- 006
type Var = String                                                         -- 007
                                                                          -- 008
                                                                          -- 009
-- locations are just numbers (addresses)                                 -- 010
type Location = Int                                                       -- 011
                                                                          -- 012
                                                                          -- 013
-- a Memory maps locations to Expressions                                 -- 014
type Mem = [(Location, Exp)]                                              -- 015
                                                                          -- 016
                                                                          -- 017
-- find a free location in a memory                                       -- 018
free :: Mem -> Location                                                   -- 019
free [] = 1                                                               -- 020
free ((l,_):m') = (max l (free m')) + 1                                   -- 021
                                                                          -- 022
bind :: Location -> Exp -> Mem -> Mem                                     -- 023
bind l e m = (l,e):m                                                      -- 024
                                                                          -- 025
query :: Location -> Mem -> Exp                                           -- 026
query l ((l',e):m) = if (l == l') then e else query l m                   -- 027
                                                                          -- 028
                                                                          -- 029
data Op = OpAdd | OpSub | OpMul | OpDiv                                   -- 030
  deriving Show                                                           -- 031
                                                                          -- 032
                                                                          -- 033
-- Abstract Syntax Tree for Expressions                                   -- 034
data Exp = ExpK Integer          -- constants                             -- 035
         | ExpVar Var            -- free variables                        -- 036
         | ExpVarL Location      -- bounded variables                     -- 037
         | ExpAg Var Exp         -- var := e (free variable)              -- 038
         | ExpAgL Location Exp   -- l := e  (bounded variable)            -- 039
         | ExpArith Exp Op Exp   -- e1 op e2                              -- 040
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 -- 041
         | ExpApp Exp Exp        -- e1 e2                                 -- 042
         | ExpLambda Var Exp     -- \x -> e                               -- 043
  deriving Show                                                           -- 044
                                                                          -- 045
                                                                          -- 046
isValue :: Exp -> Bool                                                    -- 047
isValue (ExpK _) = True                                                   -- 048
isValue (ExpLambda _ _) = True                                            -- 049
isValue _ = False                                                         -- 050
                                                                          -- 051
subs :: Exp -> Var -> Location -> Exp                                     -- 052
subs e@(ExpK n) _ _ = e                                                   -- 053
subs e@(ExpVarL l) _ _ = e                                                -- 054
subs e@(ExpVar x) y l = if (x == y) then ExpVarL l else e                 -- 055
subs (ExpAg x e) y l = if (x == y) then ExpAgL l e' else ExpAg x e'       -- 056
  where e' = subs e y l                                                   -- 057
subs (ExpAgL l e) y l' = ExpAgL l (subs e y l')                           -- 058
subs (ExpArith e1 op e2) y l = ExpArith (subs e1 y l) op (subs e2 y l)    -- 059
subs (ExpIf e1 e2 e3) y l = ExpIf (subs e1 y l) (subs e2 y l) (subs e3 y l)-- 060
subs (ExpApp e1 e2) y l = ExpApp (subs e1 y l) (subs e2 y l)              -- 061
subs e@(ExpLambda x b) y l = if (x == y) then e else ExpLambda x b'       -- 062
  where b' = subs b y l                                                   -- 063
                                                                          -- 064
                                                                          -- 065
step :: Exp -> Mem -> (Exp, Mem)                                          -- 066
                                                                          -- 067
step (ExpVarL l) m = (query l m, m)                                       -- 068
                                                                          -- 069
step (ExpAgL l v) m | isValue v = (v, bind l v m)                         -- 070
                                                                          -- 071
step (ExpAgL l e) m = (ExpAgL l e', m')                                   -- 072
  where (e', m') = step e m                                               -- 073
                                                                          -- 074
step (ExpArith (ExpK n) op (ExpK n')) m = (ExpK (doop op n n'), m)        -- 075
  where doop OpAdd = (+)                                                  -- 076
        doop OpSub = (-)                                                  -- 077
        doop OpMul = (*)                                                  -- 078
        doop OpDiv = div                                                  -- 079
                                                                          -- 080
step (ExpArith v op e) m | isValue v = (ExpArith v op e', m')             -- 081
  where (e', m') = step e m                                               -- 082
                                                                          -- 083
step (ExpArith e1 op e2) m = (ExpArith e1' op e2, m')                     -- 084
  where (e1', m') = step e1 m                                             -- 085
                                                                          -- 086
step (ExpIf (ExpK n) et ee) m = if (n /= 0) then (et, m) else (ee, m)     -- 087
                                                                          -- 088
step (ExpIf e et ee) m = (ExpIf e' et ee, m')                             -- 089
  where (e', m') = step e m                                               -- 090
                                                                          -- 091
step (ExpApp (ExpLambda var e) v) m                                       -- 092
             | isValue v = (subs e var l, bind l v m)                     -- 093
  where l = free m                                                        -- 094
                                                                          -- 095
step (ExpApp v e) m | isValue v = (ExpApp v e', m')                       -- 096
  where (e', m') = step e m                                               -- 097
                                                                          -- 098
step (ExpApp e1 e2) m = (ExpApp e1' e2, m')                               -- 099
  where (e1', m') = step e1 m                                             -- 100
                                                                          -- 101
step e _ = error ("stuck expression " ++ show e)                          -- 102
                                                                          -- 103
                                                                          -- 104
eval :: Exp -> Mem -> Exp                                                 -- 105
eval e m = if isValue e then e else eval e' m'                            -- 106
  where (e', m') = step e m                                               -- 107
                                                                          -- 108
                                                                          -- 109
---------------------------------------------------------------------------- 110
-------------------------------------------------------------------       -- 111
-- examples                                                               -- 112
                                                                          -- 113
comma :: Exp -> Exp -> Exp                                                -- 114
comma e1 e2 = ExpApp (ExpLambda "_" e2) e1                                -- 115
                                                                          -- 116
letin :: Var -> Exp -> Exp -> Exp                                         -- 117
letin v e1 e2 = ExpApp (ExpLambda v e2) e1                                -- 118
                                                                          -- 119
-- let f = \x -> x + 10 in let x = 15 in (x := f(x); x)                   -- 120
t1 = letin "f" (ExpLambda "x" (ExpArith (ExpVar "x") OpAdd (ExpK 10)))    -- 121
       (letin "x" (ExpK 15)                                               -- 122
          ((ExpAg "x" (ExpApp (ExpVar "f") (ExpVar "x"))) `comma` (ExpVar "x")))-- 123
                                                                          -- 124
                                                                          -- 125
-- (\x -> xx)(\x -> xx)                                                   -- 126
t2 = ExpApp x x                                                           -- 127
  where x = ExpLambda "x" (ExpApp (ExpVar "x") (ExpVar "x"))              -- 128
                                                                          -- 129
                                                                          -- 130
main = print(eval t1 [])                                                  -- 131