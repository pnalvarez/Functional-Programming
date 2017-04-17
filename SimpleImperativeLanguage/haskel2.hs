-----------------------------                                             -- 001
-- Simple Imperative Language                                             -- 002
-----------------------------                                             -- 003
                                                                          -- 004
                                                                          -- 005
-- variables are just names                                               -- 006
type Var = String                                                         -- 007
                                                                          -- 008
-- values are always integers (for now)                                   -- 009
type Value = Integer                                                      -- 010
                                                                          -- 011
-- a Memory maps variables to Values                                      -- 012
type Mem = Var -> Value                                                   -- 013
                                                                          -- 014
                                                                          -- 015
-- auxiliary function to map Values to Booleans                           -- 016
isTrue :: Value -> Bool                                                   -- 017
isTrue i = (i /= 0)                                                       -- 018
                                                                          -- 019
                                                                          -- 020
-- An empty memory                                                        -- 021
emptyMem :: Mem                                                           -- 022
emptyMem v = error ("invalid access to variable '" ++ v ++ "'")           -- 023
                                                                          -- 024
-- update the value of a variable in a memory                             -- 025
update :: Var -> Value -> Mem -> Mem                                      -- 026
update var val m = \v -> if v == var then val else m v                    -- 027
                                                                          -- 028
                                                                          -- 029
-- fixed-point operator                                                   -- 030
fix :: (a -> a) -> a                                                      -- 031
fix f = x                                                                 -- 032
  where x = f x                                                           -- 033
--------------------------------------------------------------------      -- 034
-- Abstract Syntax Tree for Expressions                                   -- 035
data Exp = ExpK Integer          -- constants                             -- 036
         | ExpVar Var            -- variables                             -- 037
         | ExpAdd Exp Exp        -- e1 + e2                               -- 038
         | ExpSub Exp Exp        -- e1 - e2                               -- 039
         | ExpMul Exp Exp        -- e1 * e2                               -- 040
         | ExpDiv Exp Exp        -- e1 / e2                               -- 041
         | ExpNeg Exp            -- -e                                    -- 042
                                                                          -- 043
-- Evaluates an expression in a given memory                              -- 044
evalExp :: Exp -> Mem -> Value                                            -- 045
                                                                          -- 046
evalExp (ExpK i) m = i                                                    -- 047
evalExp (ExpVar v) m = m v                                                -- 048
evalExp (ExpAdd e1 e2) m = (evalExp e1 m) + (evalExp e2 m)                -- 049
evalExp (ExpSub e1 e2) m = (evalExp e1 m) - (evalExp e2 m)                -- 050
evalExp (ExpMul e1 e2) m = (evalExp e1 m) * (evalExp e2 m)                -- 051
evalExp (ExpDiv e1 e2) m = (evalExp e1 m)  `div` (evalExp e2 m)           -- 052
evalExp (ExpNeg e) m = -(evalExp e m)                                     -- 053
                                                                          -- 054
                                                                          -- 055
----------------------------------------------------------------------    -- 056
-- Abstract Syntax Tree for Statements (commands)                         -- 057
data Cmd = CmdAsg Var Exp            -- assignment (var = exp)            -- 058
         | CmdIf Exp Cmd Cmd         -- if exp then c1 else c2            -- 059
         | CmdSeq Cmd Cmd            -- c1; c2                            -- 060
         | CmdWhile Exp Cmd          -- while e do c                      -- 061
         | CmdSkip                   -- do nothing                        -- 062
                                                                          -- 063
evalCmd :: Cmd -> Mem -> Mem                                              -- 064
                                                                          -- 065
evalCmd (CmdSkip) m = m                                                   -- 066
evalCmd (CmdSeq c1 c2) m = evalCmd c2 (evalCmd c1 m)                      -- 067
evalCmd (CmdIf e ct ce) m =                                               -- 068
  if isTrue(evalExp e m)                                                  -- 069
    then (evalCmd ct m) else (evalCmd ce m)                               -- 070
evalCmd (CmdAsg v e) m = update v (evalExp e m) m                         -- 071
                                                                          -- 072
evalCmd (CmdWhile e c) m = w m                                            -- 073
  where w = \m -> (if isTrue(evalExp e m) then w (evalCmd c m) else m)    -- 074
                                                                          -- 075
                                                                          -- 076
-------------------------------------------------------------------       -- 077
-------------------------------------------------------------------       -- 078
-- example                                                                -- 079
                                                                          -- 080
-- y = 10; x = 1; while y do  x = x * y; y = y - 1                        -- 081
cmd1 = CmdSeq (CmdSeq (CmdAsg "y" (ExpK 10))                              -- 082
                      (CmdAsg "x" (ExpK 1)))                              -- 083
              (CmdWhile (ExpVar "y")                                      -- 084
                        (CmdSeq (CmdAsg "x" (ExpMul (ExpVar "x") (ExpVar "y")))-- 085
                                (CmdAsg "y" (ExpSub (ExpVar "y") (ExpK 1)))))-- 086
                                                                          -- 087
                                                                          -- 088
-------------------------------------------------------------------       -- 089
-- code to show the final value of "x" after running "cmd1" on            -- 090
-- an initially empty memory                                              -- 091
                                                                          -- 092
finalMem = evalCmd cmd1 emptyMem                                          -- 093
                                                                          -- 094
main = print (finalMem "x")                                               -- 095
                           