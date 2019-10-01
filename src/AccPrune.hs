{-# LANGUAGE RebindableSyntax, ScopedTypeVariables, ViewPatterns, TypeApplications, TypeOperators, FlexibleContexts #-}
module AccPrune (pruneAndCheck) where
import Utils
import Data.Array.Accelerate as A
import Data.Word
import Data.Array.Accelerate.Data.Bits
import Control.Lens.Type
import Data.Array.Accelerate.Control.Lens
import qualified Prelude as P

type Cell = Word16 --bitmap, bits 0 to 8 are numbers and bit 9 represents that there is only one option left.


-- Returns the pruned list of sudokus, together with a list of bools representing whether the sudoku is inconsistent (contains a field that can't be filled anymore)
-- TODO check whether this 'check' is sufficient, or we also need a check that there are no 2 identical singles in a line/column/block.
pruneAndCheck :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell, Array DIM1 Bool)
pruneAndCheck = finish . pruneGrids where
  finish :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell, Array DIM1 Bool)
  finish xs = lift (xs, check xs)
  check :: Acc (Array DIM3 Cell) -> Acc (Array DIM1 Bool)
  check = fold1 (||) . fold1 (||) . map (/=0)

{- gets a list of sudokus (2+1=3dim) (innermost dimensions should be 9*9)
 -
 - replicate this 3 times ((3,3,3)dim)
 - transposeOn the second one to make it columns instead of rows
 - permute the third one to make it blocks instead of rows (maybe this can be cleverly combined with ^)
 - (maybe convert the (,,) into [,,])
 -
 - replicate this 2 times ((4,4)dim) to store original version and create a temporary version to fold over:
 - fold over the innermost dimension of the second version to get a single bitmap of all numbers that are already found in this row/column/block
 - use the this version to imap over the first version and remove all disallowed bits
 - remove second version
 -
 - transposeOn and permute to make all 3 versions the same sudoku again
 - transposeOn and fold to fuse all the changes within each sudoku
 -
 - map over everything to set all relevant bit9s
 -
 - use awhile to loop this shit
 - -}

-- pruneGrids is `safe` on sudokus that are still possible, but as soon as a bad guess is done outside of pruneGrid it will ofcourse result in questionable sudoku solutions. This needs to be checked seperately.
pruneGrids :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
pruneGrids xs = mySnd $ awhile cond step ((lift :: Arrays a => (Acc a, Acc a) -> Acc (a,a)) (xs, (pruneGrids' xs)))
 where 
  myFst, mySnd :: forall a. Arrays a => Acc (a, a) -> Acc a
  myFst t = let (x, y) = unlift t :: (Acc a, Acc a) in x
  mySnd t = let (x,y) = unlift t :: (Acc a, Acc a) in y
  step :: Acc (Array DIM3 Cell, Array DIM3 Cell) -> Acc (Array DIM3 Cell, Array DIM3 Cell)
  step t = let (x,y) = unlift t :: (Acc (Array DIM3 Cell), Acc (Array DIM3 Cell)) in lift (y, pruneGrids' y)
  cond x = foldAll (&&) (constant True) . map (\y -> fst y == snd y) $ zip (myFst x) (mySnd x)


pruneGrids' :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
pruneGrids' = mapSingles . fuseSudokus . prune . splitSudokus where
  splitSudokus :: Acc (Array DIM3 Cell) -> Acc (Array DIM4 Cell)
  splitSudokus = (\xs -> permute const xs permutation xs) . (replicate (constant (Z:.i3:.All:.All:.All)))
  permutation :: Exp DIM4 -> Exp DIM4 -- if I make f total, could replace the 'target array' with unsafe uninitial to speed up
  permutation (unindex4 -> (m, n, i, j)) = if m == 0 then ignore                                                               -- rows
                            else if m == 1 then lift $ Z:.1:.n:.j:.i                                                 -- columns
                            else {- m == 2 -} let c = 3*i+(j`div`3); d = j`rem`3; c' = 9*(c`div`9) + 3*(c`rem`3) + (c`rem`9)`div`3 in
                                                lift $ Z:.2:.n:.(c'`div`3):.(3*(c'`rem`3)+d)  -- blocks
  prune :: Acc (Array DIM4 Cell) -> Acc (Array DIM4 Cell)
  prune xs = let ys = findSingles xs in A.imap (\(unindex4 -> (m, n, i, j)) x -> if testBit x 9 then x else x .&. (complement (ys ! lift (Z:.m:.n:.i)))) xs
  findSingles :: Acc (Array DIM4 Cell) -> Acc (Array DIM3 Cell)
  findSingles = let f z = if testBit z 9 then z else 0 in map (`clearBit` 9) . fold1 (\x y -> f x .|. f y) -- bit 9 represents a field having only 1 option left, here we .|. all those bits.
  fuseSudokus :: Acc (Array DIM4 Cell) -> Acc (Array DIM3 Cell)
  fuseSudokus = fold1 (.&.) . transposeOn _2 _4 . transposeOn _2 _3 . transposeOn _1 _4 . (\xs -> permute const xs permutation xs)
  -- the fold throws away some information in bit9, but mapSingles simply re-adds them. I think this should not cause slowdowns on a GPU?
  mapSingles :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
  mapSingles = map (\x -> if popCount x == 1 then setBit x 9 else x) . map (`clearBit` 9)


unindex4 :: Exp DIM4 -> (Exp Int, Exp Int, Exp Int, Exp Int)
unindex4 ix = let Z :. l :. k :. j :. i = unlift ix  :: Z :. Exp Int :. Exp Int :. Exp Int :. Exp Int
              in  (l, k, j, i)


