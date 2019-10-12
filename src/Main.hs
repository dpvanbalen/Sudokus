module Main where
import Data.Word
import Data.Bits
import Data.Maybe
import Data.List
import Utils
import AccPrune
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as Native
import qualified Data.Array.Accelerate.Interpreter as Interpreter

main = interact $ pretty . solve . readSudoku


pretty :: [[Word16]] -> String
pretty [] = "mwep"
pretty grs = concatMap (\gr -> foldl f "\n" (sud gr)) grs where
  sud [] = [] 
  sud xs = take 9 xs : sud (drop 9 xs)
  f s xs = s ++ intercalate " " (map (show . unBitmap) xs) ++ "\n"

-- currently returns a list of possible solutions, in practice this list usually contains only the actual solution. It should not be hard to filter the bad solutions out.
solve :: [Word16] -> [[Word16]]
solve gr = solve' (accelerateStep [gr],[])
  where
    solve' :: ([[Word16]],[[Word16]]) -> [[Word16]]
    solve' ([],xs) = xs
    solve' (todos,xs) = let ys = accelerateStep . concat . map doGuess $ filter (not . solved) todos in solve' (ys,xs ++ filter solved todos)
    doGuess :: [Word16] -> [[Word16]]
    doGuess xs = let i = splitIndex xs in map (\j -> take i xs ++ j : drop (i+1) xs) (splitBits (xs !! i))
    splitIndex :: [Word16] -> Int
    splitIndex xs = let ys = map (\x -> if testBit x 9 then 100 else popCount x) xs in fromJust $ elemIndex (minimum ys) ys 
    accelerateStep :: [[Word16]] -> [[Word16]]
    accelerateStep xs = let (ys, bools) = accelerateStep' xs in (map snd . filter fst . zip (A.toList bools) . chunkList 81 . A.toList) $ ys
    accelerateStep' :: [[Word16]] -> (A.Array A.DIM3 Word16, A.Array A.DIM1 Bool)
    accelerateStep' xs = Native.run $ pruneAndCheck $ A.use $ A.fromList (A.Z A.:. length xs A.:. 9 A.:. 9) $ concat xs

solved :: [Word16] -> Bool
solved = all isOne
isOne x = testBit x 9

help :: [[Word16]] -> String
help = show . map (map (map help') . chunkList 9)
help' x = map (\i -> testBit x (i-1)) [1..9]

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
