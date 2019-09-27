{-# LANGUAGE RebindableSyntax, ScopedTypeVariables #-}
module Main where
import Data.Array.Accelerate as A
import Data.Word
import Data.Array.Accelerate.Data.Bits
import qualified Prelude as P

type Cell = Word16 --bitmap, bits 0 to 8 are numbers and bit 9 represents that there is only one option left.

main = undefined 

pruneGrid :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
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
pruneGrid xs = mySnd $ awhile cond step ((lift :: Arrays a => (Acc a, Acc a) -> Acc (a,a)) (xs, (pruneGrid' xs)))
 where 
  myFst, mySnd :: forall a. Arrays a => Acc (a, a) -> Acc a
  myFst t = let (x, y) = unlift t :: (Acc a, Acc a) in x
  mySnd t = let (x,y) = unlift t :: (Acc a, Acc a) in y
  step :: Acc (Array DIM3 Cell, Array DIM3 Cell) -> Acc (Array DIM3 Cell, Array DIM3 Cell)
  step t = let (x,y) = unlift t :: (Acc (Array DIM3 Cell), Acc (Array DIM3 Cell)) in lift (y, pruneGrid' y)
  cond x = foldAll (&&) (constant True) . map (\y -> fst y == snd y) $ zip (myFst x) (mySnd x)


pruneGrid' :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
pruneGrid' = mapSingles . fuseSudokus . prune . splitSudokus where
  splitSudokus :: Acc (Array DIM3 Cell) -> Acc (Array DIM4 Cell)
  splitSudokus = undefined
  prune :: Acc (Array DIM4 Cell) -> Acc (Array DIM4 Cell)
  prune = undefined
  fuseSudokus :: Acc (Array DIM4 Cell) -> Acc (Array DIM3 Cell)
  fuseSudokus = undefined
  mapSingles :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
  mapSingles = map (\x -> if popCount x == 1 then setBit x 9 else x)



