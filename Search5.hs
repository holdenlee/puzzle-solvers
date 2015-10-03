{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XExistentialQuantification
 -XRank2Types
 -XFlexibleInstances
 -XFlexibleContexts
 -XTypeFamilies

#-}

{-# LANGUAGE TemplateHaskell #-}

module Search5 where
import qualified Data.Set as S
import qualified Data.Map as M
--import Control.Monad
--import Control.Monad.Trans.Except
--import Control.Monad.List
import Data.Maybe
import Data.Bifunctor
import Control.Lens
import Data.Sequence as Seq hiding (empty, null, length, filter, take, drop, splitAt, scanl)
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Except
import Data.List
import qualified Data.MultiMap as MM

foldIterate:: (a->c->c)->[a]->c->c
foldIterate f as x = foldl (flip f) x as

class Poppable m where
  tryPop :: m a -> Maybe (a, m a)

instance Poppable Seq where
  tryPop s = case viewl s of
              h :< rest -> Just (h, rest)
              EmptyL -> Nothing

instance Poppable [] where
  tryPop li = case li of
               h:rest -> Just (h, rest)
               [] -> Nothing

--http://stackoverflow.com/questions/32927048/switch-order-of-arguments-for-instance-declaration-in-haskell/32927165#32927165

newtype PQueue p a = PQueue {_mmap :: MM.MultiMap p a}

instance (Show p, Show a) => Show (PQueue p a) where
  show (PQueue q) = show $ MM.toMap q

makeLenses ''PQueue

instance (Ord p) => Poppable (PQueue p) where
  tryPop (PQueue mp) =
    do
      ((prio, x:rest), mp2) <- M.minViewWithKey $ MM.toMap mp
      return (x, PQueue (mp & MM.delete prio
                            & foldIterate (MM.insert prio) rest))

{-
newtype PQueue p a = PQueue {_mmap :: M.Map p [a]}

makeLenses ''PQueue

qInsert:: (Ord p) => p -> a -> PQueue p a -> PQueue p a
qInsert prio x pq = pq & (mmap . ix prio) %~ (x:)  

--need FlexibleInstances
instance (Ord p) => Poppable (PQueue p) where
  tryPop (PQueue mp) =
    do
      ((prio, x:rest), mp2) <- M.minViewWithKey mp
      if null rest
        then return (x, PQueue mp)
        else return (x, PQueue $ M.insert prio rest mp)
-}
maybeToExcept :: Maybe a -> Except (Maybe b) a
maybeToExcept mb = case mb of
                    Just m -> except $ Right m
                    Nothing -> except $ Left Nothing 

popFirstTrue :: (Poppable m) => (a -> Bool) -> m a -> Maybe (a, m a)
popFirstTrue cond li = case tryPop li of
                        j@(Just (h, rest)) -> if cond h
                                              then j
                                              else popFirstTrue cond rest
                        n -> n  --Nothing
     
data SearchMethod m a = SearchMethod {_initS :: [a] -> m a,
                                      _insertS :: [a] -> m a -> m a,
                                      _checkChildren :: Bool}

makeLenses ''SearchMethod

exitW :: a -> Except a b
exitW = except . Left

searchStep :: (Poppable m) => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> (m a -> Except (Maybe a) (m a))
searchStep (SearchMethod _ insert' checkChildren') f cond start =
  do
    (picked, rest) <- maybeToExcept (tryPop start)
    let children = f picked
    let ans = insert' children rest
    if cond picked then exitW $ Just picked
                        --note: don't have to check if we check children, unless it's an initial element.
    else
      if checkChildren'
      then
        case listToMaybe (filter cond children) of
         Just x -> exitW $ Just x
         Nothing -> return ans
      else return ans

runUntilExcept :: (s -> Except a s) -> s -> a
runUntilExcept f start = case runExcept $ f start of
                          Right next -> runUntilExcept f next
                          Left result -> result

search' :: (Poppable m) => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> (m a -> Maybe a)
search' sm f cond start = runUntilExcept (searchStep sm f cond) start

search :: (Poppable m) => SearchMethod m a -> (a -> [a]) -> (a -> Bool) -> ([a] -> Maybe a)
search sm f cond = search' sm f cond . (_initS sm)

dFS :: SearchMethod [] a
dFS = SearchMethod id (++) False 

bFS' :: SearchMethod [] a
bFS' = SearchMethod id (flip (++)) True

bFS :: SearchMethod Seq a
bFS = SearchMethod Seq.fromList (\x y -> y <> (Seq.fromList x)) True

bestFS :: (Ord k) => (a -> k) -> Bool -> SearchMethod (PQueue k) a
bestFS heuristic findMin = SearchMethod (PQueue . MM.fromList . map (\x -> (heuristic x, x)))
                                        (foldIterate (\x -> mmap %~ MM.insert (heuristic x) x))
                                        (not findMin)

data WithMemory b m a = WithMemory {_mem ::S.Set b, _ident :: a -> b, _possibs ::m a}
--ex. a is a path, and b is the final node.

--need FlexibleContexts
instance (Show b, Show (m a)) => Show (WithMemory b m a) where
  show (WithMemory mem' _ possibs') = "Memory: "++(show mem') ++ "\nList: "++(show possibs')

makeLenses ''WithMemory

instance (Ord b, Poppable m) => Poppable (WithMemory b m) where
  tryPop wm@(WithMemory mem' ident' possibs') =
    do
      (h,rest) <- popFirstTrue ((`S.notMember` mem') . ident') possibs'
      return (h, WithMemory (S.insert (ident' h) mem') ident' rest)

searchWithMemory :: (Ord b, Poppable m) => (a -> b) -> SearchMethod m a -> SearchMethod (WithMemory b m) a
searchWithMemory ident' (SearchMethod initS' insertS' checkChildren') =
  SearchMethod (WithMemory (S.empty) ident' . initS')
               (\x -> possibs %~ (insertS' x))
               checkChildren'

bestFSMem :: (Ord b, Ord p) => (a -> b) -> (a -> p) -> SearchMethod (WithMemory b (PQueue p)) a
bestFSMem ident' heuristic = searchWithMemory ident' (bestFS heuristic True)

makePathFunction1 :: (a -> [(b, a)]) -> ([b], a) -> [([b], a)]
makePathFunction1 f (path, x) = map (\(edge, vert) -> (edge:path, vert)) $ f x -- [(b,a)]

makePathFunction2 :: (a -> [(b, a)]) -> ([(b,a)], a) -> [([(b,a)], a)]
makePathFunction2 f (path, x) = map (\(edge, vert) -> ((edge, vert):path, vert)) $ f x -- [(b,a)]

makePathFunction :: (a -> [a]) -> [a] -> [[a]]
makePathFunction f path = map (:path) $ f $ head path -- [(b,a)]

l1 :: (Num a) => (a, a) -> (a, a) -> a
l1 (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

obstacles = map (\x -> (x, 10-x)) [0..10]

bestFSEx = bestFSMem head (\path -> length path + l1 (10,10) (head path))

ss = searchStep bestFSEx
                (makePathFunction (\(x,y) -> filter (not . (`elem` obstacles)) [(x+1,y), (x-1, y), (x, y-1), (x,y+1)]))
                --filter (not . (`elem` obstacles)) 
                ((==(10,10)). head)

st = (_initS bestFSEx) [[(0,0)]]
st1 = ss st
alls = scanl (>>=) st1 (repeat ss)

ans = search (bestFSMem head (\path -> length path + l1 (10,10) (head path)))
             (makePathFunction (\(x,y) -> filter (not . (`elem` obstacles)) [(x+1,y), (x-1, y), (x, y-1), (x,y+1)]))
             ((==(10,10)). head)
             [[(0,0)]]

{-
TODO:
* Square root search
* Instantiate for graph search
* (Give nice example.)
* Make paths
* Ex. searching on grid.
** f (x,y) = filter (not . obstacleAt) (map ($ (x,y)) [first (+1), first (-1), second (+1), second (-1)])
* Draw nice picture (with Helm).
* Alpha-beta search (this is different)
-}

{-

My first attempt is to switch it around with a data family declaration:

    data family PSQ' a b
    data instance PSQ' a b = PSQ b a
    instance (Ord p) => Poppable (PSQ' p) where
      tryPop = fmap (first Q.key) . Q.minView

However this gives the error
    
    Couldn't match type `Q.PSQ a p0' with `PSQ' p a'

even though they can match by setting p=p0.
-}
