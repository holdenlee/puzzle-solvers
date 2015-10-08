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

module AlphaBeta where
import Data.Ord
import Control.Monad

data WithInf a = NegInf | Inf | JustN a deriving Eq

class HasMin a where
    min' :: a

class HasMax a where
    max' :: a

instance HasMin (WithInf a) where
    min' = NegInf

instance HasMax (WithInf a) where
    max' = Inf

instance (Ord a) => (Ord (WithInf a)) where
    x <= y = case (x,y) of
               (JustN x', JustN y') -> x' <= y'
               (NegInf, _) -> True
               (Inf, _) -> False
               (_, NegInf) -> False
               (_, Inf) -> True
{-
    compare x y = case (x,y) of
                    (NegInf, NegInf) -> EQ
                    (Inf, Inf) -> EQ
                    (JustN x', JustN y') -> x == y'
                    (NegInf, _) -> LT
                    (Inf, _) -> GT
                    (_, NegInf) -> GT
                    (_, Inf) -> LT-}

--for :: [a] -> b -> (a -> b -> b) -> b

--left-biased
argmax :: (Ord a) => a -> a -> (Int, a)
argmax x y = 
    if x < y
    then (2,y)
    else (1,x)

argmin :: (Ord a) => a -> a -> (Int, a)
argmin x y = 
    if x > y
    then (2,y)
    else (1,x)

for' :: (Monad m) => [a] -> b -> (a -> b -> m b) -> m b
for' li x0 f = foldM (flip f) x0 li

{-
--type Breakable a = Either a a
data Breakable a = Breakable Bool a
--False means done

instance Monad Breakable where
    b@(Breakable False a) >>= _ = b
    Breakable True a >>= f = f a

evalB :: Breakable a -> a
evalB (Breakable _ x) = x-}

evalE :: Either a a -> a
evalE (Left x) = x
evalE (Right x) = x

forE li x0 = evalE . for' li x0

--vanilla version
alphaBeta :: (Ord h, HasMin h, HasMax h) => (a -> [a]) -> (a -> Bool) -> (a -> h) -> a -> Int -> h -> h -> Bool -> ([Int], h)
alphaBeta children isTerminal heuristic cur depth a b isMax = 
    if depth == 0 || isTerminal cur
    then ([], heuristic cur)
    else      
        if isMax
        then 
            (\(_, v', _, li') -> (li', v')) $
             forE (children cur) (0, min', a, []) 
                  (\c (i0, v0, a0, indList0) ->
                      do
                        let (li, vc) = alphaBeta children isTerminal heuristic c (depth - 1) a0 b False
                        let (j, v1) = argmax v0 vc
                        let indList1 = if j==2 then i0:li else indList0
                        let a1 = max a0 v1
                        (if b <= a1 then Left else Right) (i0+1, v1, a1, indList1))
        else 
            (\(_, v', _, li') -> (li', v')) $
             forE (children cur) (0, max', b, []) 
                  (\c (i0, v0, b0, indList0) ->
                      do
                        let (li, vc) = alphaBeta children isTerminal heuristic c (depth - 1) a b0 True
                        let (j, v1) = argmax v0 vc
                        let indList1 = if j==2 then i0:li else indList0
                        let b1 = min b0 v1
                        (if b1 <= a then Left else Right) (i0+1, v1, b1, indList1))
            
                        
                         
                     

{- undefined

    where
      f minOrMax aOrB
-}
