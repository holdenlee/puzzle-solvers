{-# OPTIONS

  -XRank2Types
  -XImpredicativeTypes
  -XTupleSections
#-}

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Either
--import Data.Either.Lens
import Control.Lens
import Data.List
import Data.Char
import System.Environment
import System.Directory  
import System.IO
import Data.Maybe
import Data.Tuple

import Text.Printf
import Control.Exception
import System.CPUTime

import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import qualified Data.Set as S
--import qualified Data.MultiMap as MM
import Data.Bifunctor

import Debug.Trace

import Utilities
import Search

type Coordinate = (Int, Int)

type Board = [[[Int]]]

data Group = Row | Col | Block deriving (Eq, Ord, Show)

--in NumIn Group Int Int, first is group number, second is number
data EntryIndex = Grid Coordinate | NumIn Group Int Int deriving (Eq, Ord,Show)

--type ConstraintGraph a = M.Map a [a]

--type SConstraintGraph = ConstraintGraph Coordinate

--type ConstraintMap a i = M.Map (a, a) (i -> i -> Bool)

--type SConstraintMap = ConstraintMap EntryIndex Int

type UnaryConstraint a i = (a, i -> Bool)

type BinaryConstraint a i = ((a,a),(i->i->Bool))

data ConstraintProblem a i = ConstraintProblem {_unarys :: M.Map a (i -> Bool),
                                                _binarys :: M.Map a (M.Map a (i->i->Bool)),
                                                _possibs :: M.Map a [i]}

type Sudoku = ConstraintProblem EntryIndex Int

makeLenses ''ConstraintProblem

filterC :: [a] -> (a -> Bool) -> (Bool, [a])
filterC li f =  case li of
                 [] -> (False, [])
                 x:rest ->
                   if f x
                      --do add in x, don't set change counter
                   then second (x:) $ filterC rest f
                      --don't add in x, do set change counter
                   else first (const True) $ filterC rest f

--assume symmetric --show!
ac3' :: (Ord a, Show a, Show i) => ([(a,a)], ConstraintProblem a i) -> Maybe (ConstraintProblem a i)
ac3' (pairs, cp) =
  case pairs of
   [] -> Just cp
   (from,to):rest ->
     let
       ConstraintProblem u b ps = cp 
       --check everything in to for consistency
       --function i -> i -> Bool
       f = (lookupe (to, from) b) `debug` ("TF:" ++ (show (to, from, from `M.member` lookup2 to b)))
       toPossibs = lookup2 to ps `debug` ("T:" ++ (show (to, to `M.member` ps)))
       fromPossibs = lookup2 from ps `debug` ("F:" ++ (show (from, from `M.member` ps)))
       (changed, newToPossibs) = filterC toPossibs 
         (\j ->
           let
             --what in the list of fromPossibs are compatible with this j\in toPossibs?
             validFromPossibs = filter (f j) fromPossibs
           in
            not $ null validFromPossibs) 
       toNbrs = nbrs to b
     in
      if changed
      then
        if --trace (show ("newToPossibs", to, newToPossibs)) $ 
          null newToPossibs
        then Nothing
        else ac3' (rest ++ map (to,) toNbrs,
                   cp & (possibs . at to) .~ (Just newToPossibs))
      else ac3' (rest,cp)



--show!
ac3 :: (Ord a, Show a, Show i) => ConstraintProblem a i -> Maybe (ConstraintProblem a i)
ac3 cp = 
  let
    ConstraintProblem u b _ = cp
    vars = M.keys u
    --lookup variable in unary constraints
    --filter using that constraint
    cp' = for vars cp (\v cp0 -> cp0 & possibs . at v %~ fmap (filter (fromMaybe (const True) $ M.lookup v u)))
  in
   runAC3 vars cp'

--show!
runAC3 :: (Ord a, Show a, Show i) => [a] -> ConstraintProblem a i -> Maybe (ConstraintProblem a i)
runAC3 vars cp =
  let
    b = _binarys cp
    --lookup variable in unary constraints
    --filter using that constraint
    worklist = concatMap (\x -> map (x,) $ nbrs x b) vars
  in
   ac3' (worklist, cp) -- `debug` (show worklist)

{-
for :: [a] -> b -> (a -> b -> b) -> b
for li x0 f = foldl (flip f) x0 li
   
lookup2 :: (Ord a) => a -> M.Map a b -> b
lookup2 x h = fromJust (M.lookup x h)
  -}
--show!
lookupe :: (Ord a, Show a) => (a,a) -> M.Map a (M.Map a b) -> b
lookupe (x1,x2) = lookup2 x2 . lookup2 x1

nbrs :: (Ord a) => a -> M.Map a (M.Map a b) -> [a]
nbrs x m =
  case M.lookup x m of
   Nothing -> []
   Just m2 -> M.keys m2

subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize k li = if k==0
                     then [[]]
                     else
                       case li of
                        [] -> []
                        h:rest -> (map (h:) $ subsetsOfSize (k-1) rest) ++
                                  (subsetsOfSize k rest)

allDifferent' :: (Eq i) => [a] -> [BinaryConstraint a i] -- = ((a,a),(i->i->Bool))
allDifferent' li = map (,(/=)) $ map (\l -> (l!!0, l!!1)) $ subsetsOfSize 2 li

allDifferent = addSwapConstraints. allDifferent'

consistency' :: Coordinate -> [BinaryConstraint EntryIndex Int]
consistency' (i,j) =
  [((Grid (i,j), NumIn grp (fst $ loc (i,j)) n),
    \x y -> if y == snd (loc (i,j)) then x==n else True) |
   (grp,loc) <- [(Row, rowLoc),(Col, colLoc),(Block, blockLoc)],
   n <- [1..9]]
  --example
  --((Grid (0,1), NumIn Row 1 3), \x y -> if y==1 then x==3)

consistency = addSwapConstraints . consistency'

addSwapConstraints :: [BinaryConstraint a i] -> [BinaryConstraint a i]
addSwapConstraints x = x ++ map (bimap swap flip) x

rowLoc :: Coordinate -> (Int, Int)
rowLoc = id

colLoc = swap

blockLoc (x,y) =
  let
    (xq,xr) = x `quotRem` 3
    (yq,yr) = y `quotRem` 3
    f i j = 3*i + j
  in
   (f xq yq, f xr yr)

initCP :: (Ord a) => [(a, [i])] -> ConstraintProblem a i
initCP as = ConstraintProblem {_unarys = M.empty,
                                  _binarys = M.empty,
                                  _possibs = M.fromList as}
                                    --M.fromList $ map (,is) as}

addUnarys :: (Ord a) => [UnaryConstraint a i] -> ConstraintProblem a i -> ConstraintProblem a i
addUnarys li = unarys %~ (insertMultiple li)

--unsafe version of at --show!
at2 :: (Ord a, Show a) => a -> Lens' (M.Map a b) b
at2 k = lens (lookup2 k) (flip $ M.insert k)

--show!
addBinarys :: (Ord a, Show a) => [BinaryConstraint a i] -> ConstraintProblem a i -> ConstraintProblem a i
addBinarys li cp0 =
  let
    li2 = groupBy (\(x,_) (y,_) -> x==y) $ map (\((x,y),z) -> (x,(y,z))) li
  in
   for li2 cp0 (\plist cp ->
                 case plist of
                  (x,_):_ -> cp & binarys . at x %~ (\m -> Just $ ((case m of
                                                                    Nothing -> M.empty
                                                                    Just m' -> m') & insertMultiple (map snd plist)))
                  --this will not arise, groups are not empty
                  _ -> cp)

--imap :: (Int -> a -> a) -> [a] -> [a]
--imap f li = zipWith f [0..] li

isDigit' :: Char -> Bool
isDigit' = (`elem` "123456789")

readSudoku :: [String] -> [[Maybe Int]]
readSudoku = (map.map) (\x -> if isDigit' x then Just (digitToInt x) else Nothing)

getUnary :: [[Maybe Int]] -> [UnaryConstraint EntryIndex Int]
getUnary = map (second (==)) . catMaybes . concat . imap (\i -> imap (\j -> fmap (Grid (i,j),)))

denum :: [[a]] -> [[(Coordinate, a)]]
denum = imap (\i -> imap (\j -> ((i,j),)))

fileToString :: String -> IO String
fileToString inputF = 
    do
      handle <- openFile inputF ReadMode
      hGetContents handle

row :: Int -> [EntryIndex]
row i = map (Grid.(i,)) [0..8]

col i = map (Grid.(,i)) [0..8]

block i = let (q, r) = i `quotRem` 3
          in map Grid $ (,) <$> [(3*q)..(3*q+2)] <*> [(3*r)..(3*r+2)]

[rowD, colD, blockD] = map (\grp -> \i -> map (NumIn grp i) [1..9]) [Row, Col, Block] 

initSudoku :: Sudoku
initSudoku = initCP (map (,[1..9]) [Grid (x,y) | x<-[0..8], y<-[0..8]] ++
                     map (,[0..8]) [NumIn grp x y | grp <- [Row, Col, Block], x<-[0..8], y<-[1..9]])
             & (addBinarys $ concatMap allDifferent [grp n | grp <- [row, col, block, rowD, colD, blockD], n <- [0..8]])
             & (addBinarys $ concatMap consistency [(x,y)| x<-[0..8], y<-[0..8]])

sudokuToList :: Sudoku -> [[[Int]]]
sudokuToList s = (map.map) (\c -> lookup2 (Grid c) $ _possibs s) (map (\i -> map (i,) [0..8]) [0..8])

showSudoku :: Sudoku -> String
showSudoku s = unlines $ (map.map) (\c -> case lookup2 (Grid c) $ _possibs s of
                                           [x] -> intToDigit x
                                           _ -> ' ') (map (\i -> map (i,) [0..8]) [0..8])

showVerbose :: Sudoku -> String
showVerbose s = unlines $ map show $ sudokuToList s

splitOn :: EntryIndex -> Sudoku -> [Sudoku]
splitOn c s =
  let
    ps = lookup2 c (_possibs s)
  in
   map (\i -> s & (possibs . at2 c) .~ [i]) ps

childSudokus' :: EntryIndex -> Sudoku -> [Sudoku]
childSudokus' c s = catMaybes . map (runAC3 [c]) $ splitOn c s

isDone :: Sudoku -> Bool
isDone = (all.all) ((==1).length) . sudokuToList

--assume not.isDone?
childSudokus :: Sudoku -> [Sudoku]
childSudokus s =
  let
    bd = sudokuToList s
    i = fst ((sortBy (\(_,x) (_,y) -> compare (length x) (length y)) $ filter (\(x,y) -> length y > 1) $ concat $ denum bd)!!0)
  in
   if isDone s then [s] else childSudokus' (Grid i) s

solve :: Sudoku -> Maybe Sudoku
solve s = bFS (maybeToList $ ac3 s) childSudokus isDone

solveD :: Sudoku -> Maybe Sudoku
solveD s = dFS (maybeToList $ ac3 s) childSudokus isDone

main = time $ do
  args <- getArgs
  str <- fileToString $ args !! 0
  let u = getUnary $ readSudoku $ lines str
  let s = initSudoku & addUnarys u
  let Just solved = solveD s 
  putStrLn $ showSudoku solved
--  putStrLn $ showVerbose solved

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

groupN :: Int -> [a] -> [[a]]
groupN n li = case li of
               [] -> [[]]
               _ -> uncurry (\x y -> x:(groupN n y)) $ splitAt n li

pS :: Sudoku -> IO ()
pS = putStrLn . showSudoku

s1' = readSudoku $ groupN 9 "8          36      7  9 2   5   7       457     1   3   1    68  85   1  9    4"  
      -- "030001000006000050500000983080006302000050000903800060714000009020000800000400030"

s1 = initSudoku & addUnarys (getUnary s1')

Just s2 = solve s1


