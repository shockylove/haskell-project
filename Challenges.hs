{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2022
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),ArithExpr(..),
                   calcBBInteractions,solveBB,prettyPrint,
                   parseArith,churchEnc,innerRedn1,innerArithRedn1,compareArithLam) where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Maybe

-- Imort the standard library not provided by module
import Data.Set (fromList, toList)
import Data.Function (on)


instance NFData ArithExpr

instance NFData LamExpr

instance NFData Marking

instance NFData Side



-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ]
-- top left is (1,1) , bottom right is (N,N) where N is size of grid
type Pos = (Int, Int)
-- entry/exit point,int range is 1 to N where N is size of grid
type EdgePos = ( Side , Int )
data Side = North | East | South | West
            deriving (Show, Eq, Ord, Generic)

data Marking = Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)
  

{- | 'calcBBInteractions' @n atoms edges@ returns a sets of length @n@ with @x@ as the value of
-}
calcBBInteractions :: Int -> Atoms -> [EdgePos] -> Interactions
calcBBInteractions n atoms entries 
    | null entries = error "Empty edgePositions"
    | n <= 0 = error "n should be positive"
   -- | ( maximum (map fst atoms) > n ) || ( maximum (map snd atoms) > n ) || (maximum (map snd entries) > n) || ( n <= 0 ) = error "out of range error occurs"
    | otherwise = [calcSingleInteraction entry | entry <- entries]
    where
        -- Calculate single interaction
        calcSingleInteraction :: EdgePos -> ( EdgePos, Marking )
        calcSingleInteraction edgePos = (edgePos, initial edgePos)

        -- Generate initial position and direction
        initial :: EdgePos -> Marking
        initial (side,num)
            | side == North = gonext ((num, 0), South)
            | side == South = gonext ((num, n+1), North)
            | side == West  = gonext ((0, num), East)
            | side == East  = gonext ((n+1, num), West)
            | otherwise = error "direction error"
        
        -- Predict next position and direction
        nextP :: (Pos,Side) -> (Pos, Side)
        nextP ((x,y), dir)
            | dir == North = ((x,y-1), North)
            | dir == South = ((x,y+1), South)
            | dir == West  = ((x-1,y), West)
            | dir == East  = ((x+1,y), East)
            | otherwise = error "direction error"
        
        -- go to next position
        gonext :: (Pos, Side) -> Marking
        gonext (pos, dir)
            -- Absorb
            | absCon = Absorb
            -- Edge Reflection
            | sideRefCon && not absCon  = Reflect
            -- Absorb on side
            | absOnSideCon = Absorb
            -- Double Reflection
            | length doubleRefCondition > 1  = Reflect
            -- Path
            | fst nextPos == 0 = Path (West, snd nextPos)
            | fst nextPos == n + 1 = Path (East, snd nextPos)
            | snd nextPos == 0 = Path (North, fst nextPos)
            | snd nextPos == n + 1 = Path (South, fst nextPos)
            -- Reflected
            | or [ True | (x, y) <- atoms, ( (x -1, y + 1) == nextPos && dir == North) || (nextPos == (x - 1, y -1) && dir == South)] = gonext (nextPos, West)
            | or [ True | (x, y) <- atoms, ( (x + 1, y -1) == nextPos && dir == South) || (nextPos == (x + 1, y + 1) && dir == North)] = gonext (nextPos, East)
            | or [ True | (x, y) <- atoms, ( (x -1, y - 1) == nextPos && dir == East) || (nextPos == (x + 1, y - 1) && dir == West)] = gonext (nextPos, North)
            | or [ True | (x, y) <- atoms, ( (x -1, y + 1) == nextPos && dir == East) || (nextPos == (x + 1, y + 1) && dir == West)] = gonext (nextPos, South)
            -- keep going
            | otherwise = gonext (nextP (pos, dir))
                where
                    nextPos = fst $ nextP (pos, dir)
                    nextDir = snd $ nextP (pos, dir)
                    absCon = nextPos `elem` atoms
                    sideRefCon = or [ True | (x, y) <- atoms, ((nextPos == (x + 1, y) || nextPos == (x -1, y)) && (y == 1 || y == n) && (dir == North || dir == South)) || ((x == 1 || x == n) && (dir == West || dir == East) && (nextPos == (x, y + 1) || nextPos == (x, y -1)) )] 
                    absOnSideCon = or [ True | (x,y) <- atoms, nextPos == (x + 1, y) || nextPos == (x - 1, y) || nextPos == (x, y + 1)|| nextPos == (x, y -1)]
                    doubleRefCondition = [ True | (x,y) <- atoms, nextPos == (x + 1, y + 1) || nextPos == (x - 1, y + 1) || nextPos == (x + 1, y - 1)|| nextPos == (x - 1, y -1)] 



-- Challenge 2
-- Find atoms in a Black Box
-- https://literateprograms.org/eight_queens_puzzle__haskell_.html
-- https://gist.github.com/tronje/cf1ec8f05b854ff488760dce396a1f5f
-- https://wiki.haskell.org/99_questions/Solutions/26


-- Backtracking Strategy
solveBB :: Int -> Interactions -> Atoms 
solveBB n interactions = go [(0, [], allLocations)] 1 
    where
    -- Incremental atom number
    go [] m = go  [(m, [], allLocations)] (m + 1)

    -- m atoms have been placed，Try every situation
    go ((0, placed, remainingLocations) : rest) m
      | calcBBInteractions n placed (map fst interactions) == interactions = placed
      | otherwise  = go rest m

    -- Place m atoms
    go ((i, placed, remainingLocations) : rest) m
                   = go (expandPlacements i placed remainingLocations ++ rest) m

    -- Expand placements
    expandPlacements i placed [] = []
    expandPlacements i placed (location:remaining) = 
      [( i-1,location:placed, remaining), ( i, placed, remaining)]

    allLocations = [(x, y) | x <- [1 .. n], y <- [1 .. n]]


-- Challenge 3
-- Pretty Printing Lambda with Alpha-Normalisation 
-- https://stackoverflow.com/questions/40316605/implementing-alpha-equivalence-in-haskell

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int 
                deriving (Eq, Show, Read)


prettyPrint :: LamExpr -> String
prettyPrint = pretty . goANF


-- get free variables list
freeVars :: LamExpr -> [Int]
freeVars (LamVar x) = [x]
freeVars (LamAbs x e) = filter (/= x) (freeVars e)
freeVars (LamApp e1 e2) = rmdups (freeVars e1 ++ freeVars e2)
    where
        rmdups :: Eq a => [a] -> [a]
        rmdups [] = []
        rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- conver lambda expression to alpha normal form
goANF :: LamExpr -> LamExpr
goANF = go []
    where
    go :: [(Int,Int)] -> LamExpr -> LamExpr
    go env (LamApp e1 e2) = LamApp (go env e1) (go env e2)
    go env (LamVar x) = case lookup x env of
        Just ind -> LamVar ind
        Nothing -> LamVar x
    go env (LamAbs x exp) = LamAbs newIndex (go ((x,newIndex):env) exp)
        where
        takenIndex = findUsed (delete x $ freeVars exp) env
        newIndex = head ([0..] \\ takenIndex)

-- print formate
pretty :: LamExpr -> String
pretty (LamVar x) = "x" ++ show x
pretty (LamApp e1@(LamAbs _ _) e2) = "(" ++ pretty e1 ++ ") " ++ pretty e2
pretty (LamApp e1 e2) = pretty e1 ++ " " ++ pretty e2
pretty (LamAbs x exp) = "\\" ++ "x" ++ show x ++ "->" ++ pretty exp

-- find allocated index
findIn :: Int -> [(Int, Int)] -> Int
findIn x mapping = case lookup x mapping of
    Just y -> y
    Nothing -> error "element not found in mapping"

-- find used index
findUsed :: [Int] -> [(Int, Int)] -> [Int]
findUsed xs mapping = [ a | x <- xs, let a = findIn x mapping]


-- Challenge 4 
-- Parsing Arithmetic Expressions
-- Expr ::= Expr1 "*" Expr | Expr1
-- Expr1 ::= Expr2 "+" Expr1 | Expr2
-- Expr2 ::= Num | Section Expr2 | "(" Expr ")" 
-- Section ::= "(" "+" Expr ")"
-- Num ::= Digits
-- Digits ::= Digit | Digits Digit
-- Digit ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr 
               | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int
    deriving (Show,Eq,Read)
    

parseArith :: String -> Maybe ArithExpr
parseArith s = case parse arithMulExpr s of
    [] -> Nothing
    [(res, "")] -> Just res
    _ -> Nothing

--Expr ::= Expr1 "*" Expr | Expr1
arithMulExpr :: Parser ArithExpr
arithMulExpr = do
    e1 <- token arithAddExpr
    symbol "*"
    e2 <- token arithMulExpr
    return (Mul e1 e2) 
    <|> arithAddExpr

-- Expr1 ::= Expr2 "+" Expr1 | Expr2
arithAddExpr :: Parser ArithExpr
arithAddExpr = do
    e1 <- token arithAtomExpr
    symbol "+"
    e2 <- token arithAddExpr
    return (Add e1 e2) 
    <|> arithAtomExpr

-- Expr2 ::= Num | Section Expr2 | "(" Expr ")"
arithAtomExpr :: Parser ArithExpr
arithAtomExpr = (ArithNum <$> natural) <|> arithSectionApp <|> arithParenExpr

-- Section ::= "(" "+" Expr ")"
arithSectionApp :: Parser ArithExpr
arithSectionApp = do
                    secExpr <- arithSection
                    mulExpr <- token arithMulExpr
                    pure $ SecApp secExpr mulExpr

-- Section ::= "(" "+" Expr ")"
arithSection :: Parser ArithExpr
arithSection = do
                symbol "("
                symbol "+"
                mulExpr <- token arithMulExpr
                symbol ")"
                pure $ Section mulExpr

-- "(" Expr ")"
arithParenExpr :: Parser ArithExpr
arithParenExpr = do
                symbol "("
                expr <- arithMulExpr
                symbol ")"
                pure expr

--Num ::= Digits
arithNum :: Parser ArithExpr
arithNum = ArithNum <$> natural


-- Challenge 5
-- Church Encoding of arithmetic 

churchEnc :: ArithExpr -> LamExpr 
churchEnc (Add e1 e2) = LamApp (LamApp cAdd (churchEnc e1)) (churchEnc e2)
churchEnc (Mul e1 e2) = LamApp (LamApp cMul (churchEnc e1)) (churchEnc e2)
churchEnc (Section e) = LamApp cAdd (churchEnc e)
churchEnc (SecApp e1 e2) = LamApp (churchEnc e1) (churchEnc e2)
churchEnc (ArithNum n) = cNum n


-- n is λf->λx->f(f (...n times...fx)...))
cNum :: Int -> LamExpr
cNum n = case n < 0 of
    True -> error "n < 0"
    False ->  LamAbs f (LamAbs x (foldr LamApp (LamVar x) (replicate n (LamVar f))))
        where
            f = 0
            x = 1

-- λm->λn->( λf->λx->mf(nfx) )
cAdd :: LamExpr
cAdd = LamAbs m (LamAbs n (LamAbs f (LamAbs x (LamApp (LamApp (LamVar m) (LamVar f)) (LamApp (LamApp (LamVar n) (LamVar f)) (LamVar x))))))
    where
        m = 0
        n = 1
        f = 2
        x = 3
   
-- λm->λn->( λf->λx->m (nf)x)
cMul :: LamExpr
cMul = LamAbs m (LamAbs n (LamAbs f (LamAbs x (LamApp (LamApp (LamVar m) (LamApp (LamVar n) (LamVar f))) (LamVar x)))))
    where
        m = 0
        n = 1
        f = 2
        x = 3

-- Challenge 6
-- Compare Innermost Reduction for Arithmetic and its Church Encoding
-- Add (ArithNum n) (ArithNum m) ------> ArithNum ( n + m )
-- Mul (ArithNum n) (ArithNum m) ------> ArithNum ( n * m )
-- SecApp (Sec0on (ArithNum n)) (ArithNum m) -----> ArithNum ( n + m )


-- The Dir data structure represents the direction to go when traversing an expression tree, either GoLeft or GoRight.
data Dir = GoLeft | GoRight deriving Show
type Path = [Dir]

-- The Redex type is a tuple of a Path, a function to apply to an expression, and a redex.
type Redex expr redex = (Path, expr -> expr, redex)

-- The Redex_Lam type is a Redex for LamExpr.
type Redex_Lam = Redex LamExpr (Int, LamExpr, LamExpr)

-- The Redex_Arith type is a Redex for ArithExpr.
type Redex_Arith = Redex ArithExpr (Int -> Int -> Int, Int, Int)

-- (\x -> e1) e2
-- performs a single beta reduction step on a lambda calculus expression. 
-- It takes an integer x, a LamExpr e1, and a LamExpr e2, and returns the result of substituting e2 for x in e1.
beta :: Int -> LamExpr -> LamExpr -> LamExpr
beta x e1 e2 = case e1 of
    LamApp el er       -> LamApp (beta x el e2) (beta x er e2)
    LamAbs binder body
      | binder == x               -> e1
      | binder `elem` freeVars e2 -> LamAbs binder' (beta x (beta binder body (LamVar binder')) e2)
      | otherwise                 -> LamAbs binder (beta x body e2)
      where 
        binder' = head [x | x <- [0..], x `notElem` fvs]
        fvs = freeVars e2 ++ freeVars body
    LamVar y
      | x == y    -> e2
      | otherwise -> e1

-- getRedexes_Lam takes a LamExpr and returns a list of all possible redexes in the expressio
getRedexes_Lam :: LamExpr -> [Redex_Lam]
getRedexes_Lam expr = case expr of
    LamApp e1 e2       -> redexes0 ++ redexes1 ++ redexes2
      where
        redexes0 = case e1 of
            LamAbs binder body -> pure ([], id, (binder, body, e2))
            _                  -> []
        redexes1 = getRedexes_Lam e1 >>= \(path, continue, redex) ->
            pure (GoLeft : path, (`LamApp` e2) . continue, redex)
        redexes2 = getRedexes_Lam e2 >>= \(path, continue, redex) ->
            pure (GoRight : path, LamApp e1 . continue, redex)
    LamAbs binder body -> getRedexes_Lam body >>= \(path, continue, redex) ->
        pure (GoRight : path, LamAbs binder . continue, redex)
    LamVar _           -> []

-- getRedexes_Arith  takes an ArithExpr and returns a list of all possible redexes in the expression
getRedexes_Arith :: ArithExpr -> [Redex_Arith]
getRedexes_Arith (Add e1 e2) = redexes0 ++ redexes1 ++ redexes2 where
    redexes0 = case (e1, e2) of
        (ArithNum x, ArithNum y) -> pure ([], id, ((+), x, y))
        _                        -> []
    redexes1 = getRedexes_Arith e1 >>= \(path, continue, redex) ->
        pure (GoLeft : path, (`Add` e2) . continue, redex)
    redexes2 = getRedexes_Arith e2 >>= \(path, continue, redex) ->
        pure (GoRight : path, Add e1 . continue, redex)
getRedexes_Arith (Mul e1 e2) = redexes0 ++ redexes1 ++ redexes2 where
    redexes0 = case (e1, e2) of
        (ArithNum x, ArithNum y) -> pure ([], id, ((*), x, y))
        _                        -> []
    redexes1 = getRedexes_Arith e1 >>= \(path, continue, redex) ->
        pure (GoLeft : path, (`Mul` e2) . continue, redex)
    redexes2 = getRedexes_Arith e2 >>= \(path, continue, redex) ->
        pure (GoRight : path, Mul e1 . continue, redex)
getRedexes_Arith (Section e1) = getRedexes_Arith e1 >>= \(path, continue, redex) ->
    pure (GoLeft : path, Section . continue, redex)
getRedexes_Arith (SecApp e1 e2) = redexes0 ++ redexes1 ++ redexes2 where
    redexes0 = case (e1, e2) of
        (Section (ArithNum x), ArithNum y) -> pure ([], id, ((+), x, y))
        _                                  -> []
    redexes1 = getRedexes_Arith e1 >>= \(path, continue, redex) ->
        pure (GoLeft : path, (`SecApp` e2) . continue, redex)
    redexes2 = getRedexes_Arith e2 >>= \(path, continue, redex) ->
        pure (GoRight : path, SecApp e1 . continue, redex)
getRedexes_Arith (ArithNum _) = []

-- chooseRedex takes two Redexes and returns the one with a shorter Path length.
chooseRedex :: Redex expr redex -> Redex expr redex -> Redex expr redex
chooseRedex redex1@(path1, _, _) redex2@(path2, _, _)
  | left_inner path1 path2 = redex1
  | otherwise              = redex2
  where
    left_inner (GoLeft : _) (GoRight : _) = True
    left_inner (GoRight : _) (GoLeft : _) = False
    left_inner (_ : xs) (_ : ys)          = left_inner xs ys
    left_inner [] (_ : _)                 = False
    left_inner (_ : _) []                 = True
    left_inner [] []                      = True

chooseRedexFromRedexes :: [Redex expr redex] -> Maybe (Redex expr redex)
chooseRedexFromRedexes []                 = Nothing
chooseRedexFromRedexes (redex : redexes)  = pure (foldr chooseRedex redex redexes)

-- Compare Innermost Reduction for Arithmetic and its Churc
innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 expr = do
    (_, continue, (binder, body, arg)) <- chooseRedexFromRedexes (getRedexes_Lam expr)
    pure . continue $ beta binder body arg

-- the (leftmost) innermost reduction strategy
innerArithRedn1 :: ArithExpr -> Maybe ArithExpr
innerArithRedn1 expr = do
    (_, continue, (op, arg1, arg2)) <- chooseRedexFromRedexes (getRedexes_Arith expr)
    pure . continue . ArithNum $ op arg1 arg2

getSteps :: (a -> Maybe a) -> a -> Int
getSteps f x = case f x of
    Nothing     -> 0
    Just x'     -> 1 + getSteps f x'

compareArithLam :: ArithExpr -> (Int, Int)
compareArithLam expr = (getSteps innerArithRedn1 expr, getSteps innerRedn1 expr') where
    expr' = churchEnc expr

