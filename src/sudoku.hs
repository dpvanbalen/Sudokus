{-# LANGUAGE BangPatterns, TypeApplications #-}
module Main where
import Data.Word
import qualified Data.Vector as V
import Data.Bits
import Control.Monad
import Control.Monad.Loops
import Data.Maybe
import Data.List
import Debug.Trace

data Cell = One !Word16 | More !Word16 --bitmap
  deriving (Show, Eq)
type Grid = V.Vector Cell
--  deriving (Show, Eq)

main = interact $ pretty . solve . readSudoku


pretty :: Maybe Grid -> String
pretty Nothing = "mwep"
pretty (Just gr) = foldl f "\n" (sud lst) where
  lst = V.toList gr
  sud [] = [] 
  sud xs = take 9 xs : sud (drop 9 xs)
  f s xs = s ++ intercalate " " (map (show . unBitmap . unCell) xs) ++ "\n"

isOne (One  _) = True
isOne (More _) = False

pruneGrid :: Grid -> Maybe Grid
pruneGrid gr = check $ (until =<< ((==) =<<) $ \g -> foldr pruneCells g indices) $ gr where
    indices = rows ++ cols ++ boxs
    rows = (\i -> [i..i+8]) <$> [0,9,18,27,36,45,54,63,72]
    cols = transpose rows
    boxs = (\i -> (i+) <$> [0,1,2,9,10,11,18,19,20]) <$> [0,3,6,27,30,33,54,57,60]
pruneCells :: [Int] -> Grid -> Grid
pruneCells is gr = V.accumulate g gr (V.fromList $ zip is (repeat remove)) where
    cells = map (gr V.!) is
    remove = complement $ foldr f 0 cells
    f (More _) r = r
    f (One w)  r = w .|. r
    g (One w)  _ = One w
    g (More w) r = More $ w .&. r

solve :: Grid -> Maybe Grid
solve gr 
  | solved gr = Just gr
  | otherwise = pruneGrid gr >>= solve'
  where
    splitLoc = V.minIndex . V.map choiceNumber
    choiceNumber (One  _) = 10 
    choiceNumber (More x) = popCount x
    solve' :: Grid -> Maybe Grid
    solve' g = mconcat . map solve . map (makeOpt g) $ splitBits (unCell $ g V.! splitLoc g) -- note that whenever we get an 'empty More', this line makes a Nothing.
    makeOpt :: Grid -> Word16 -> Grid
    makeOpt g w = let a = V.slice 0 (splitLoc g) g; b = V.slice (splitLoc g+1) (80-splitLoc g) g in a V.++ (V.cons (One w) b)
    

check x = if V.all (\w -> isOne w || w /= More 0) x then Just x else Nothing
solved = V.all isOne


readSudoku :: String -> Grid
readSudoku = V.fromList . take 81 . map f where
  f '.' = More           $ 511 --decimal for the binary value of 9 ones
  f '1' = One $ setBit 0 0 
  f '2' = One $ setBit 0 1 
  f '3' = One $ setBit 0 2 
  f '4' = One $ setBit 0 3 
  f '5' = One $ setBit 0 4 
  f '6' = One $ setBit 0 5 
  f '7' = One $ setBit 0 6 
  f '8' = One $ setBit 0 7 
  f '9' = One $ setBit 0 8 

splitBits :: Word16 -> [Word16]
splitBits w = mapMaybe f [0..8] 
  where f i
          | testBit w i = Just $ bit i
          | otherwise = Nothing

unCell (One w) = w
unCell (More w) = w

unBitmap 1   = 1
unBitmap 2   = 2
unBitmap 4   = 3
unBitmap 8   = 4 
unBitmap 16  = 5
unBitmap 32  = 6
unBitmap 64  = 7
unBitmap 128 = 8
unBitmap 256 = 9
unBitmap i = 0
