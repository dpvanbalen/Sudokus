module BruteForce where

import Sudoku
import Data.List
import Data.Maybe
import Control.Monad
import Utils


-- assumes that the input doesn't contain something illegal already
solve :: Sudoku -> Int -> Maybe SolvedSudoku
solve s n = solveAt (0, 0) s >>= traverse sequence
    where
        solveAt :: (Int, Int) -> Sudoku -> Maybe Sudoku
        solveAt (i, j) s
            | i == n*n = Just s
            | j == n*n = solveAt (i+1, 0) s
            | otherwise = case (s !! i) !! j of
                Just _ -> solveAt (i, j+1) s
                Nothing -> firstJust $ map (solveAt (i, j+1) . fillAt s (i, j)) (options (i, j) s n)

options :: (Int, Int) -> Sudoku -> Int -> [Maybe Int]
options (i, j) s n = filter (\k -> (k `notElem` (s !! i)) 
                                && (k `notElem` map (!! j) s)
                                && (k `notElem` square (i, j) s)) (Just <$> [1..n*n])
                where square (i, j) = concatMap (take n . drop (n * (j `div` n))) . take n . drop (n* (i `div` n))

fillAt :: Sudoku -> (Int, Int) -> Maybe Int -> Sudoku
fillAt s (i, j) k = update (update (const k) j) i s