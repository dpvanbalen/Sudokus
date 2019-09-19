module Sudoku where

import Data.List
import Utils

type Row = [Maybe Int]
type Sudoku = [Row]

type SolvedRow = [Int]
type SolvedSudoku = [SolvedRow]

readSudoku :: String -> Sudoku
readSudoku = split9 . map g where 
    g :: Char -> Maybe Int
    g '.' = Nothing
    g x   = Just $ read [x]
    split9 [] = []
    split9 xs = take 9 xs : split9 (drop 9 xs)

example :: Sudoku 
example = readSudoku ".6....3..4..7............8......8.125..6............5..82...7.....5..6......1...."  

