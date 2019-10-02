module Main where
import Data.Word
import Data.Bits
import Data.Maybe
import Data.List
import Utils
import AccPrune
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as A
import Debug.Trace 

main = interact $ pretty . solve . readSudoku


pretty :: Maybe [Word16] -> String
pretty Nothing = "mwep"
pretty (Just gr) = foldl f "\n" (sud gr) where
  sud [] = [] 
  sud xs = take 9 xs : sud (drop 9 xs)
  f s xs = s ++ intercalate " " (map (show . unBitmap) xs) ++ "\n"


solve :: [Word16] -> Maybe [Word16]
solve gr = solve' (accelerateStep [gr])
  where
    solve' :: [[Word16]] -> Maybe [Word16]
    solve' [] = Nothing
    solve' xs = case find solved xs of
      Just solution -> Just solution
      Nothing -> solve' . accelerateStep . concat . map doGuess $(\x -> trace (show (length x)) x) $ xs
    doGuess :: [Word16] -> [[Word16]]
    doGuess xs = let i = splitIndex xs in map (\j -> take i xs ++ j : drop (i+1) xs) (splitBits (xs !! i))
    splitIndex :: [Word16] -> Int
    splitIndex xs = let ys = map (\x -> if testBit x 9 then 100 else popCount x) xs in fromJust $ elemIndex (minimum ys) ys 
    accelerateStep :: [[Word16]] -> [[Word16]]
    accelerateStep xs = let (ys, bools) = accelerateStep' xs in (map snd . filter fst . zip (A.toList bools) . chunkList 81 . A.toList) $ ys
    accelerateStep' :: [[Word16]] -> (A.Array A.DIM3 Word16, A.Array A.DIM1 Bool)
    accelerateStep' xs = A.run $ pruneAndCheck $ A.use $ A.fromList (A.Z A.:. length xs A.:. 9 A.:. 9) $ concat xs

solved :: [Word16] -> Bool
solved = all isOne
isOne x = testBit x 9


readSudoku :: String -> [Word16]
readSudoku = take 81 . map f where
  f '.' = 511 --decimal for the binary value of 9 ones
  f '1' = (512 +) $ setBit 0 0 
  f '2' = (512 +) $ setBit 0 1 
  f '3' = (512 +) $ setBit 0 2 
  f '4' = (512 +) $ setBit 0 3 
  f '5' = (512 +) $ setBit 0 4 
  f '6' = (512 +) $ setBit 0 5 
  f '7' = (512 +) $ setBit 0 6 
  f '8' = (512 +) $ setBit 0 7 
  f '9' = (512 +) $ setBit 0 8 

splitBits :: Word16 -> [Word16]
splitBits w = mapMaybe f [0..8] 
  where f i
          | testBit w i = Just $ bit i
          | otherwise = Nothing


unBitmap 513 = 1
unBitmap 514 = 2
unBitmap 516 = 3
unBitmap 520 = 4 
unBitmap 528 = 5
unBitmap 544 = 6
unBitmap 576 = 7
unBitmap 640 = 8
unBitmap 768 = 9
unBitmap i = 0
