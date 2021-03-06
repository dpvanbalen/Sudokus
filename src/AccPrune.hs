{-# LANGUAGE RebindableSyntax, ScopedTypeVariables, ViewPatterns, TypeOperators, FlexibleContexts #-}
module AccPrune where
import Utils
import Data.Array.Accelerate as A
import Data.Word
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Control.Lens
import qualified Prelude as P

type Cell = Word16 --bitmap, bits 0 to 8 are numbers and bit 9 represents that there is only one option left.


-- Returns the pruned list of sudokus, together with a list of bools representing whether the sudoku is inconsistent (contains a field that can't be filled anymore)
pruneAndCheck :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell, Array DIM1 Bool) --n*9*9 -> (n*9*9,n)
pruneAndCheck = finish . pruneGrids where
  finish :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell, Array DIM1 Bool)
  finish xs = lift (xs, zipWith3 (\x y z -> x&&y&&z) (check1 xs) (check2 xs) (check3 xs))
  check1, check2 :: Acc (Array DIM3 Cell) -> Acc (Array DIM1 Bool)
  check1 = fold1 (&&) . fold1 (&&) . map (/=0)
  check2 = fold1 (&&) . map (\x -> 511 == x.&.511) . fold1 (.|.)
  check3 = check2 . transposeOn _1 _2

{- gets a list of sudokus (2+1=3dim) (n*9*9)
 -
 - replicate this 3 times (4dim) (3*n*9*9)
 - permute the second one to make it columns instead of rows
 - permute the third one to make it blocks instead of rows
 -
 - replicate this twice, applying 'weirdtransform' to one of them. (2*3*n*9*9)
 - weirdtransform applies a bijection between the numbers 1..9 and the 9 fields in each `row`.
 - So for example if the bit for digit 5 was set in only the first three fields, the new fifth field will have binary value 111.
 - (I haven't seen this transformation anywhere before)
 - This allows us to only search for fields that have only 1 or 2 bits set, automatically also finding the bits that are only set in 1 or 2 fields.
 -
 - fold over the innermost dimension to get a single bitmap of all numbers that are already found in this row/column/block
 - use the this version to imap over the original and remove all disallowed bits
 -
 - permute to make all 3 versions align again
 - transposeOn and fold to fuse all the changes within each sudoku
 -
 - map over everything to set all relevant bit9s
 -
 - use awhile to loop this
 - -}

-- pruneGrids is `safe` on sudokus that are still possible, but as soon as a bad guess is done outside of pruneGrid it will ofcourse result in questionable sudoku solutions. This needs to be checked seperately.
pruneGrids :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
pruneGrids xs = mySnd $ awhile cond step ((lift :: Arrays a => (Acc a, Acc a) -> Acc (a,a)) (xs, pruneGrids' xs))
 where
  myFst, mySnd :: forall a. Arrays a => Acc (a, a) -> Acc a
  myFst t = let (x, y) = unlift t :: (Acc a, Acc a) in x
  mySnd t = let (x,y) = unlift t :: (Acc a, Acc a) in y
  step :: Acc (Array DIM3 Cell, Array DIM3 Cell) -> Acc (Array DIM3 Cell, Array DIM3 Cell)
  step t = let (x,y) = unlift t :: (Acc (Array DIM3 Cell), Acc (Array DIM3 Cell)) in lift (y, pruneGrids' y)
  cond x = map not $ foldAll (&&) (constant True) $ zipWith (==) (myFst x) (mySnd x)


pruneGrids' :: Acc (Array DIM3 Cell) -> Acc (Array DIM3 Cell)
pruneGrids' = mapSingles . fuseSudokus . pruneDoubles . pruneSingles . mapSingles . splitSudokus

splitSudokus = weirdtransform . (\xs -> backpermute (shape xs) permutation xs) . replicate (constant (Z:.i3:.All:.All:.All)) . map (`clearBit` 9)

permutation :: Exp DIM4 -> Exp DIM4 -- this permutation is a bijection, and its own inverse.
permutation (unindex4 -> (m, n, i, j)) =    if m == 0 then lift $ Z:.0:.n:.i:.j                          -- rows
                                       else if m == 1 then lift $ Z:.1:.n:.j:.i                          -- columns
                                       else {- m == 2 -} let c = 3*i+(j`div`3); d = j`rem`3; c' = 9*(c`div`9) + 3*(c`rem`3) + (c`rem`9)`div`3 in
                                                           lift $ Z:.2:.n:.(c'`div`3):.(3*(c'`rem`3)+d)  -- blocks

rep1 = replicate (constant (Z:.All:.All:.i1:.All:.All))
weirdtransform xs = concatOn _3 (rep1 xs) (rep1 $ weirdtransform' xs) -- adds another layer, bijecting between numbers and fields.
unweirdtransform xs = let ys = slice xs (constant (Z:.All:.All:.i0:.All:.All));
                          zs = slice xs (constant (Z:.All:.All:.i1:.All:.All)) in
   zipWith (.&.) ys (weirdtransform' zs)

--weirdtransform' is also its own inverse, except for bit9
weirdtransform' :: Acc (Array DIM4 Cell) -> Acc (Array DIM4 Cell)
weirdtransform' xs = let (ones,twos,threes,fours,fives,sixes,sevens,eights,nines) = (f 1,f 2,f 4,f 8,f 16,f 32,f 64,f 128,f 256);
                         f x = replicate (constant (Z:.All:.All:.All:.i1)) . fold1 (.|.) . A.imap (\(unindex4 -> (_,_,_,i)) y -> if x.&.y == 0 then 0 else setBit 0 i) $ xs;
                           in ones ++ twos ++ threes ++ fours ++ fives ++ sixes ++ sevens ++ eights ++ nines


pruneSingles xs = let ys = replicate (constant (Z:.All:.All:.All:.All:.i9)) (findSingles xs) in zipWith (\x y -> if testBit x 9 then x else x .&. (complement y)) xs ys

-- bit 9 represents a field having only 1 option left, here we .|. all those bits.
findSingles = fold1 (.|.) . map (\x -> if testBit x 9 then x `clearBit` 9 else 0)

fuseSudokus = fold1 (.&.) . transposeOn _1 _4 . transposeOn _1 _3 . transposeOn _1 _2 . (\xs -> backpermute (shape xs) permutation xs) . unweirdtransform

mapSingles  :: Shape a => Acc (Array a Cell) -> Acc (Array a Cell)
mapSingles  = map (\x -> if popCount x == 1 then setBit x 9 else x) . map (`clearBit` 9)

pruneDoubles = (\xs -> let ys = replicate (constant (Z:.All:.All:.All:.All:.i9)) (findDoubles xs) in
              zipWith (\x y -> if popCount x <= 2 && x==x.&.y then x else x.&.complement y) xs ys) . map (`clearBit` 9)

findDoubles = map unDoubleIndexes . map (\(unlift -> (z1,z2,z3)::(Exp Word64, Exp Word64, Exp Word64)) -> z2 - z3) . fold1 (\(unlift -> (x1,x2,x3)) (unlift -> (y1,y2,y3)) ->
                 lift (x1.|.y1, x2.|.y2.|.(x1.&.y1), x3.|.y3.|.(x1.&.y2).|.(x2.&.y1))) --(seen at least once, seen at least twice, seen more times)
                   . map (\x -> lift (x,0,0) :: Exp (Word64, Word64, Word64)) . map doubleIndexes


unDoubleIndexes :: Exp Word64 -> Exp Word16 -- inverse of below, for a map containing an arbitrary number of bits that each refer to two numbers, recover which numbers are all relevant
unDoubleIndexes x = fst $ while ((/=0) . snd) (\(unlift -> (a:: Exp Word16, b :: Exp Word64)) -> lift ((a .|.) .fromIntegral . unDoubleIndex $ b, clearBit b (countTrailingZeros b))) $ lift (0,x)
  where
  --unDoubleIndex could also be written with the floor of a square root for less conditionals
  unDoubleIndex :: Exp Word64 -> Exp Word64
  unDoubleIndex x = let i = countTrailingZeros x in
          if i <  8 then setBit (shift x    1 ) 0 else
          if i < 15 then setBit (shift x (- 6)) 1 else
          if i < 21 then setBit (shift x (-12)) 2 else
          if i < 26 then setBit (shift x (-17)) 3 else
          if i < 30 then setBit (shift x (-21)) 4 else
          if i < 33 then setBit (shift x (-24)) 5 else
          if i < 35 then setBit (shift x (-26)) 6 else
                         setBit (setBit 0 8)    7



doubleIndexes :: Exp Word16 -> Exp Word64 --given a word, if it has exactly two bits set, compute a bitmask that signifies which two bits are set. Barely doesn't fit in 32 bits :(
doubleIndexes i = if popCount i /= 2 then 0 else setBit 0 $ secondBit + (36 - shift ((9-firstBit)*(8-firstBit)) (-1))
 where
  firstBit = countTrailingZeros i
  secondBit = countTrailingZeros (shift i (-firstBit-1))



unindex3 :: Exp DIM3 -> (Exp Int, Exp Int, Exp Int)
unindex3 ix = let Z :. k :. j :. i = unlift ix  :: Z :. Exp Int :. Exp Int :. Exp Int
              in  (k, j, i)

unindex4 :: Exp DIM4 -> (Exp Int, Exp Int, Exp Int, Exp Int)
unindex4 ix = let Z :. l :. k :. j :. i = unlift ix  :: Z :. Exp Int :. Exp Int :. Exp Int :. Exp Int
              in  (l, k, j, i)


unindex5 :: Exp DIM5 -> (Exp Int, Exp Int, Exp Int, Exp Int, Exp Int)
unindex5 ix = let Z :. m :. l :. k :. j :. i = unlift ix  :: Z :. Exp Int :. Exp Int :. Exp Int :. Exp Int :. Exp Int
              in  (m, l, k, j, i)



