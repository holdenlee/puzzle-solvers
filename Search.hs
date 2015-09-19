{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
#-}

module Search where
import qualified Data.Set as S

dFS :: [a] -> (a -> [a]) -> (a -> Bool) -> Maybe a
dFS li f cond = case li of
  h:rest -> if cond h then Just h else dFS (concatMap f li) f cond
  _ -> Nothing
--is dFS because of laziness.

--braid :: [a] -> [a] -> [a]
--braid x y 

dFSList :: [a] -> (a -> [a]) -> [a]
dFSList li f = concatMap (\x -> x:(dFSList (f x) f)) li

dFS2 :: [a] -> (a -> [a]) -> (a -> Bool) -> a
dFS2 li f cond = head $ filter cond $ dFSList li f
--much slower than dFS, why?

bFS :: [a] -> (a -> [a]) -> (a -> Bool) -> Maybe a
bFS li f cond = if null li then Nothing else
  case filter cond li of
   h:rest -> Just h
   _ -> bFS (concatMap f li) f cond

bFSList :: (a -> [a]) -> [a] -> [a]
bFSList f li = li ++ (bFSList f $ concatMap f li)

bFS2 :: (a -> [a]) -> (a -> Bool) -> [a] -> a
bFS2 f cond li = head $ filter cond $ bFSList f li

untilLefts :: [Either a a] -> (a -> [a]) -> (a -> Bool) -> [Either a a]
untilLefts li f cond = concatMap (\x -> case x of
                                         Left y -> [Left y]
                                         Right z -> if cond z then [Left z] else untilLefts (map Right $ f z) f cond) li

--[] gives error
dFSEither :: [a] -> (a -> [a]) -> (a -> Bool) -> a
dFSEither li f cond = either id id $ head $ untilLefts (map Right li) f cond

--dFS [0] (\x -> [x+1,x-1]) (==10000)  
--bFS [(0,0)] (\(x,y) -> [(x+1,y),(x,y+1)]) (\(x,y) -> x*y>=8)

{-
dFS :: [a] -> (a -> [a]) -> (a -> Bool) -> Maybe a
dFS li f cond =
  listToMaybe $ do
    x <- concatMap f li
    if cond x then [x] else dFS -}

{-
dFSCheck' :: ([a], S.Set b) -> (a -> [a]) -> (a -> Bool) -> (a -> b) -> Maybe a
dFSCheck' (li, s) f cond summ = case li of
  h:rest -> if cond h then Just h else
              let
                newS = S.union s (map summ li)
                filter (`notMember` s)
--destroys the laziness
-}
                --warning: need f to not keep going?
