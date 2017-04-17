exists :: [a] -> (a -> Bool) -> Bool                                      -- 006
exists l f = or (map f l)                                                 -- 007
                                                                          -- 008
                                                                          -- 009
data RE = REEmpty                                                         -- 010
        | REEpsilon                                                       -- 011
        | REAny                                                           -- 012
        | REChar Char                                                     -- 013
        | RESeq RE RE                                                     -- 014
        | REOr RE RE                                                      -- 015
        | REKleene RE                                                     -- 016
                                                                          -- 017
                                                                          -- 018
eval :: RE -> String -> Bool                                              -- 019
                                                                          -- 020
eval REEmpty s = False                                                    -- 021
                                                                          -- 022
eval REEpsilon s = (s == [])                                              -- 023
                                                                          -- 024
eval REAny s = (length s == 1)                                            -- 025
                                                                          -- 026
eval (REChar c) s = (s == [c])                                            -- 027
                                                                          -- 028
eval (REOr e e') s = eval e s || eval e' s                                -- 029
                                                                          -- 030
eval (RESeq e e') s = exists [0..length s] f                              -- 031
  where f i = eval e (take i s) && eval e' (drop i s)                     -- 032
                                                                          -- 033
eval (REKleene e) s = (s == []) || exists [1..length s] f                 -- 034
  where f i = eval e (take i s) && eval (REKleene e) (drop i s)           -- 035
                                                                          -- 036
----------------------------------------------                            -- 037
-- example: e1 = (b*)a                                                    -- 038
e1 = RESeq (REKleene (REChar 'b')) (REChar 'a')                           -- 039
                                                                          -- 040
main = print(eval  e1 "bbbbba")                                           -- 041
                                                                          -- 042