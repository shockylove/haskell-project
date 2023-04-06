-- Xiaoke Li
-- xl5u20@soton.ac.uk
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021

import Challenges
import Data.List (intersect, subsequences, (\\))

-- run tests for tests
tests :: IO ()
tests = do
  putStr $ "----------------- Challenge 1 -----------------\n" ++ t1
  putStr $ "----------------- Challenge 2 -----------------\n" ++ t2
  putStr $ "----------------- Challenge 3 -----------------\n" ++ t3
  putStr $ "----------------- Challenge 4 -----------------\n" ++ t4
  putStr $ "----------------- Challenge 5 -----------------\n" ++ t5
  putStr $ "----------------- Challenge 6 -----------------\n" ++ t6


  -- Print result
  if times == 55 then putStr "\n\n\n############### Congratulations ###############\n############## All tests passed! ##############\n\n\n" else putStr $ "\n\n\n################### No! ##################\n############### " ++ show times ++ " tests passed! ##############\n\n\n"

  -- Print copyright
  putStr "\n-------------- Design by Xiaoke --------------\n----------------- in Southampton --------------\n-------------- Hope you like it! --------------\n\n\n"
  where
    t1 = test1 exp1
    t2 = test2 exp2
    t3 = test3 exp3
    t4 = test4 exp4
    t5 = test5 exp5
    t6 = test6 exp6
    -- Count the number of Passed
    times = length $ filter (== "Passed") $ words (t1  ++ t2 ++ t3 ++ t4 ++ t5 ++ t6) 




-- challenge 1
test1 :: [(Int, Atoms, [EdgePos], Interactions)] -> String
test1 [] = []
test1 ((a, b, c, d) : xs)
  | null $ calcBBInteractions a b c \\ d = "\n" ++ "calcBBInteractions " ++ show a ++ " " ++ show b ++ show c ++ "\n" ++ "Test Passed" ++ "\n" ++ test1 xs
  | otherwise = "\n" ++ "calcBBInteractions " ++ show a ++ " " ++ show b ++ show c ++ "\n" ++ "Answer is " ++ show d ++ "\n" ++ test1 xs

-- challenge 2
test2 :: [(Int, Interactions, Atoms)] -> String
test2 [] = []
test2 ((a, b, c) : xs)
  | null $ solveBB a b \\ c = "\n" ++ "solveBB " ++ show a ++ " " ++ show b ++ "\n" ++ "Test Passed" ++ "\n" ++ test2 xs
  | otherwise = "\n" ++ "solveBB " ++ show a ++ " " ++ show b ++ "\n" ++ "Answer is " ++ show c ++ "\n" ++ test2 xs

-- challenge 3
test3 :: [(LamExpr, String)] -> String
test3 [] = []
test3 ((a, b) : xs)
  | (prettyPrint a) == b = "\n" ++ "prettyPrint " ++ show a ++ "\n" ++ "Test Passed" ++ "\n" ++ test3 xs ++ "Passed"
  | otherwise = "\n" ++ "prettyPrint " ++ show a ++ "\n" ++ "Answer is " ++ b ++ "\n" ++ prettyPrint a ++ test3 xs

-- challenge 4
test4 :: [(String, Maybe ArithExpr)] -> String
test4 [] = []
test4 ((a, b) : xs)
  | parseArith a == b = "\n" ++ "parseArith " ++ a ++ "\n" ++ "Test Passed" ++ "\n" ++ test4 xs
  | otherwise = "\n" ++ "parseArith " ++ a ++ "\n" ++ "Answer is " ++ show b ++ "\n" ++ test4 xs

-- challenge 5
test5 :: [(ArithExpr, LamExpr)] -> String
test5 [] = []
test5 ((a, b) : xs)
  | churchEnc a == b = "\n" ++ "churchEnc " ++ show a ++ "\n" ++ "Test Passed" ++ "\n" ++ test5 xs
  | otherwise = "\n" ++ "churchEnc " ++ show a ++ "\n" ++ "Answer is " ++ show b ++ "\n" ++ test5 xs

test6 :: [(ArithExpr,(Int,Int))] -> String
test6 [] = []
test6 ((a, b) : xs)
  | compareArithLam a == b = "\n" ++ "compareArithLam " ++ show a ++ "\n" ++ "Test Passed" ++ "\n" ++ test6 xs
  | otherwise = "\n" ++ "compareArithLam " ++ show a ++ "\n" ++ "Answer is " ++ show b ++ "\n" ++ test6 xs

-- cases for 1
exp1 :: [(Int, Atoms, [EdgePos], Interactions)]
exp1 = 
    [ -- example in PFD
        (8, [(2,3),(7,3),(4,6),(7,8)], [(North,1),(North,5),(East,7),(South,7),(West,3),(West,1)], [ ((North,1),Path (West,2)), ((North,5),Path (East,5)), ((East,7),Path (East,4)), ((South,7),Absorb), ((West,1),Path (East,1)), ((West,3),Absorb)]),
        -- from wiki
        (8, [(4, 6), (6, 6)], edges8, [((North, 1), Path (South, 1)), ((North, 2), Path (South, 2)), ((North, 3), Path (West, 5)), ((North, 4), Absorb), ((North, 5), Reflect), ((North, 6), Absorb), ((North, 7), Path (East, 5)), ((North, 8), Path (South, 8)), ((East, 1), Path (West, 1)), ((East, 2), Path (West, 2)), ((East, 3), Path (West, 3)), ((East, 4), Path (West, 4)), ((East, 5), Path (North, 7)), ((East, 6), Absorb), ((East, 7), Path (South, 7)), ((East, 8), Path (West, 8)), ((South, 1), Path (North, 1)), ((South, 2), Path (North, 2)), ((South, 3), Path (West, 7)), ((South, 4), Absorb), ((South, 5), Reflect), ((South, 6), Absorb), ((South, 7), Path (East, 7)), ((South, 8), Path (North, 8)), ((West, 1), Path (East, 1)), ((West, 2), Path (East, 2)), ((West, 3), Path (East, 3)), ((West, 4), Path (East, 4)), ((West, 5), Path (North, 3)), ((West, 6), Absorb), ((West, 7), Path (South, 3)), ((West, 8), Path (East, 8))]),
        (8, [(6, 2), (6, 7), (8, 7)], edges8, [((North, 1), Path (South, 1)), ((North, 2), Path (South, 2)), ((North, 3), Path (South, 3)), ((North, 4), Path (South, 4)), ((North, 5), Path (West, 1)), ((North, 6), Absorb), ((North, 7), Path (East, 1)), ((North, 8), Absorb), ((East, 1), Path (North, 7)), ((East, 2), Absorb), ((East, 3), Reflect), ((East, 4), Path (West, 4)), ((East, 5), Path (West, 5)), ((East, 6), Reflect), ((East, 7), Absorb), ((East, 8), Reflect), ((South, 1), Path (North, 1)), ((South, 2), Path (North, 2)), ((South, 3), Path (North, 3)), ((South, 4), Path (North, 4)), ((South, 5), Path (West, 8)), ((South, 6), Absorb), ((South, 7), Reflect), ((South, 8), Absorb), ((West, 1), Path (North, 5)), ((West, 2), Absorb), ((West, 3), Path (West, 6)), ((West, 4), Path (East, 4)), ((West, 5), Path (East, 5)), ((West, 6), Path (West, 3)), ((West, 7), Absorb), ((West, 8), Path (South, 5))]),
        (8, [(3, 3), (6, 2), (6, 7)], edges8, [((North, 1), Path (South, 1)), ((North, 2), Path (West, 2)), ((North, 3), Absorb), ((North, 4), Absorb), ((North, 5), Path (West, 1)), ((North, 6), Absorb), ((North, 7), Path (East, 1)), ((North, 8), Path (South, 8)), ((East, 1), Path (North, 7)), ((East, 2), Absorb), ((East, 3), Path (East, 6)), ((East, 4), Path (South, 4)), ((East, 5), Path (West, 5)), ((East, 6), Path (East, 3)), ((East, 7), Absorb), ((East, 8), Path (South, 7)), ((South, 1), Path (North, 1)), ((South, 2), Path (West, 4)), ((South, 3), Absorb), ((South, 4), Path (East, 4)), ((South, 5), Path (West, 8)), ((South, 6), Absorb), ((South, 7), Path (East, 8)), ((South, 8), Path (North, 8)), ((West, 1), Path (North, 5)), ((West, 2), Path (North, 2)), ((West, 3), Absorb), ((West, 4), Path (South, 2)), ((West, 5), Path (East, 5)), ((West, 6), Absorb), ((West, 7), Absorb), ((West, 8), Path (South, 5))]),
        (8, [(3, 3), (6, 3), (3, 6), (6, 6)], edges8, [((North, 1), Path (South, 1)), ((North, 2), Path (West, 2)), ((North, 3), Absorb), ((North, 4), Path (North, 5)), ((North, 5), Path (North, 4)), ((North, 6), Absorb), ((North, 7), Path (East, 2)), ((North, 8), Path (South, 8)), ((East, 1), Path (West, 1)), ((East, 2), Path (North, 7)), ((East, 3), Absorb), ((East, 4), Path (East, 5)), ((East, 5), Path (East, 4)), ((East, 6), Absorb), ((East, 7), Path (South, 7)), ((East, 8), Path (West, 8)), ((South, 1), Path (North, 1)), ((South, 2), Path (West, 7)), ((South, 3), Absorb), ((South, 4), Path (South, 5)), ((South, 5), Path (South, 4)), ((South, 6), Absorb), ((South, 7), Path (East, 7)), ((South, 8), Path (North, 8)), ((West, 1), Path (East, 1)), ((West, 2), Path (North, 2)), ((West, 3), Absorb), ((West, 4), Path (West, 5)), ((West, 5), Path (West, 4)), ((West, 6), Absorb), ((West, 7), Path (South, 2)), ((West, 8), Path (East, 8))]),
        (8, [(3, 3), (4, 5), (5, 3)], edges8, [((North, 1), Path (South, 1)), ((North, 2), Path (West, 2)), ((North, 3), Absorb), ((North, 4), Reflect), ((North, 5), Absorb), ((North, 6), Path (East, 2)), ((North, 7), Path (South, 7)), ((North, 8), Path (South, 8)), ((East, 1), Path (West, 1)), ((East, 2), Path (North, 6)), ((East, 3), Absorb), ((East, 4), Path (South, 6)), ((East, 5), Absorb), ((East, 6), Path (South, 5)), ((East, 7), Path (West, 7)), ((East, 8), Path (West, 8)), ((South, 1), Path (North, 1)), ((South, 2), Path (West, 4)), ((South, 3), Path (West, 6)), ((South, 4), Absorb), ((South, 5), Path (East, 6)), ((South, 6), Path (East, 4)), ((South, 7), Path (North, 7)), ((South, 8), Path (North, 8)), ((West, 1), Path (East, 1)), ((West, 2), Path (North, 2)), ((West, 3), Absorb), ((West, 4), Path (South, 2)), ((West, 5), Absorb), ((West, 6), Path (South, 3)), ((West, 7), Path (East, 7)), ((West, 8), Path (East, 8))]),
        -- special example by me
        (3, [(2, 3), (3, 3)], edges3, [((North, 1), Path (West, 2)), ((North, 2), Absorb), ((North, 3), Absorb), ((East, 1), Path (West, 1)), ((East, 2), Reflect), ((East, 3), Absorb), ((South, 1), Reflect), ((South, 2), Absorb), ((South, 3), Absorb), ((West, 1), Path (East, 1)), ((West, 2), Path (North, 1)), ((West, 3), Absorb)]),
        (2, [(1, 1), (1, 2)], edges2, [((North, 1), Absorb), ((North, 2), Reflect), ((East, 1), Absorb), ((East, 2), Absorb), ((South, 1), Absorb), ((South, 2), Reflect), ((West, 1), Absorb), ((West, 2), Absorb)]),
        (2, [(1, 1), (2, 2)], edges2, [((North, 1), Absorb), ((North, 2), Reflect), ((East, 1), Reflect), ((East, 2), Absorb), ((South, 1), Reflect), ((South, 2), Absorb), ((West, 1), Absorb), ((West, 2), Reflect)]),
        (3, [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3), (3, 1)], edges3, [((North, 1), Absorb), ((North, 2), Absorb), ((North, 3), Absorb), ((East, 1), Absorb), ((East, 2), Reflect), ((East, 3), Absorb), ((South, 1), Absorb), ((South, 2), Absorb), ((South, 3), Reflect), ((West, 1), Absorb), ((West, 2), Absorb), ((West, 3), Absorb)]),
        (3, [(1, 1), (3, 1)], edges3, [((North, 1), Absorb), ((North, 2), Reflect), ((North, 3), Absorb), ((East, 1), Absorb), ((East, 2), Reflect), ((East, 3), Path (West, 3)), ((South, 1), Absorb), ((South, 2), Reflect), ((South, 3), Absorb), ((West, 1), Absorb), ((West, 2), Reflect), ((West, 3), Path (East, 3))]),
        (3, [(1, 1), (2, 2), (3, 3)], edges3, [((North, 1), Absorb), ((North, 2), Reflect), ((North, 3), Path (East, 1)), ((East, 1), Path (North, 3)), ((East, 2), Reflect), ((East, 3), Absorb), ((South, 1), Path (West, 3)), ((South, 2), Reflect), ((South, 3), Absorb), ((West, 1), Absorb), ((West, 2), Reflect), ((West, 3), Path (South, 1))])
    ]
        where
            edges2 = [(North,1),(North,2),(East,1),(East,2),(South,1),(South,2),(West,1),(West,2)]
            edges3 = [(North,1),(North,2),(North,3),(East,1),(East,2),(East,3),(South,1),(South,2),(South,3),(West,1),(West,2),(West,3)]
            edges8 = [(North,1),(North,2),(North,3),(North,4),(North,5),(North,6),(North,7),(North,8),(East,1),(East,2),(East,3),(East,4),(East,5),(East,6),(East,7),(East,8),(South,1),(South,2),(South,3),(South,4),(South,5),(South,6),(South,7),(South,8),(West,1),(West,2),(West,3),(West,4),(West,5),(West,6),(West,7),(West,8)]

-- cases for 2
exp2 :: [(Int, Interactions, Atoms)]
exp2 =
  [ (8, [((North, 1), Path (West, 2)), ((North, 5), Path (East, 5)), ((East, 7), Path (East, 4)), ((South, 7), Absorb), ((West, 1), Path (East, 1)), ((West, 3), Absorb)], [(2, 3), (7, 3), (4, 6), (7, 8)])
  ]

-- cases for 3
exp3 :: [(LamExpr, String)]
exp3 =
  [ 
    -- example in PFD
    (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)), "(\\x0->x0) \\x0 -> x0"),
    (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))), "\\x0-> x0 \\x0 -> x0"),
    (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1))), "x2 \\x0-> \\x1 -> x0"),
    (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))), "\\x0 -> \\x0 -> x0 \\x0 -> \\x1 -> x0 ")
  ]

--   test3e :: [(LamExpr, String)]
-- test3e =
--     [ -- example in PFD
--     (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)), "(\\x0 -> x0) \\x0 -> x0"),
--     (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))), "\\x0 -> x0 \\x0 -> x0"),
--     (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1))), "x2 \\x0 -> \\x1 -> x0"),
--     (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))), "\\x0-> \\x0-> x0 \\x0-> \\x1-> x0")
--     -- special example by me
--     --(LamApp (LamApp (LamVar 3) (LamVar 1)) (LamAbs 2 (LamVar 1)), "(x3 x1) \\x2 -> x1"),
--     --(LamApp (LamAbs 2 (LamApp (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamAbs 2 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))))) (LamVar 1))) (LamAbs 2 (LamApp (LamAbs 4 (LamAbs 3 (LamApp (LamVar 4) (LamAbs 4 (LamAbs 3 (LamApp (LamVar 3) (LamAbs 3 (LamAbs 2 (LamApp (LamVar 3) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamAbs 2 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))))))))))))))) (LamVar 1))), "(\\x2 -> 2 x1) \\x2 -> (\\x4 -> \\x3 x4 \\x4 -> \\x3 x3 \\x3 -> \\x2 x3 2) x1"),
--     --(LamAbs 3 (LamAbs 3 (LamApp (LamVar 2) (LamAbs 3 (LamAbs 2 (LamApp (LamVar 3) (LamAbs 3 (LamAbs 2 (LamApp (LamVar 2) (LamAbs 7 (LamAbs 6 (LamApp (LamVar 6) (LamAbs 6 (LamAbs 2 (LamApp (LamVar 2) (LamAbs 5 (LamAbs 3 (LamApp (LamVar 3) (LamAbs 4 (LamAbs 2 (LamApp (LamVar 2) (LamAbs 3 (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))))))))))))))))))))))))), "\\x3 -> \\x3 x2 \\x3 -> \\x2 x3 6")
--     ]

-- cases for 4
exp4 :: [(String, Maybe ArithExpr)]
exp4 =
  [ 
    -- examples in PFD
    ("1 + 2", Just (Add (ArithNum 1) (ArithNum 2))),
    ("(+1) 2", Just (SecApp (Section (ArithNum 1)) (ArithNum 2))),
    ("2 (+1)", Nothing),
    ("(+1) (+2) 3", Just (SecApp (Section (ArithNum 1)) (SecApp (Section (ArithNum 2)) (ArithNum 3))))
  ]

-- cases for 5
exp5 :: [(ArithExpr, LamExpr)]
exp5 =
  [ 
    -- examples in PFD
    ((ArithNum 2), LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))),
    ((SecApp (Section (ArithNum 1)) (ArithNum 1)), LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1)))))
  ]

exp6 :: [(ArithExpr,(Int,Int))]
exp6 = 
  [
    --Examples in PFD
    (ArithNum 4,(0,0)),
    (Add (ArithNum 4) (ArithNum 5),(1,6)),
    (Add (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3),(2,12)),
    (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3),(2,28)),
    --(2,19)
    --Example by me
    (Mul (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (ArithNum 5)) (ArithNum 3),(3,36))
    --(3,26)
  ]


--   ghci> compareArithLam (ArithNum 4)
-- (0,0)
-- ghci> compareArithLam (Add (ArithNum 4) (ArithNum 5) )
-- (1,6)
-- ghci> compare(Add (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3))
-- compare          compareArithLam
-- ghci> compareArithLam (Add (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3))
-- (2,12)
-- ghci> compareArithLam (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3))
-- (2,28)
-- "(((+1) 4) + 5) * 3"
-- Mul (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (ArithNum 5)) (ArithNum 3)
-- (3,36)