module Main where

import Sudoku
import Data.List
import Data.Maybe
import Control.Monad
import Utils


main = interact $ show . const (solve example) . readSudoku

-- assumes that the input doesn't contain something illegal already
solve :: Sudoku -> Maybe SolvedSudoku
solve s = solveAt (0, 0) s >>= traverse sequence
    where
        solveAt :: (Int, Int) -> Sudoku -> Maybe Sudoku
        solveAt (i, j) s
            | i == 9 = Just s
            | j == 9 = solveAt (i+1, 0) s
            | otherwise = case (s !! i) !! j of
                Just _ -> solveAt (i, j+1) s
                Nothing -> firstJust $ map (solveAt (i, j+1) . fillAt s (i, j)) (options (i, j) s)

options :: (Int, Int) -> Sudoku -> [Maybe Int]
options (i, j) s = filter (\k -> (k `notElem` (s !! i)) 
                                && (k `notElem` map (!! j) s)
                                && (k `notElem` square (i, j) s)) (Just <$> [1..9])
                where square (i, j) = concatMap (take 3 . drop (3 * (j `div` 3))) . take 3 . drop (3* (i `div` 3))

fillAt :: Sudoku -> (Int, Int) -> Maybe Int -> Sudoku
fillAt s (i, j) k = update (update (const k) j) i s
