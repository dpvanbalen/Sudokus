module Sudoku where

import Data.List
import Utils

type Row = [Maybe Int]
type Sudoku = [Row]

type SolvedRow = [Int]
type SolvedSudoku = [SolvedRow]

example :: Sudoku -- 9*9
example =   [[ Just 5,  Just 3, Nothing,    Nothing,  Just 7, Nothing,      Nothing, Nothing, Nothing]
            ,[ Just 6, Nothing, Nothing,     Just 1,  Just 9,  Just 5,      Nothing, Nothing, Nothing]
            ,[Nothing,  Just 9,  Just 8,    Nothing, Nothing, Nothing,      Nothing,  Just 6, Nothing]

            ,[ Just 8, Nothing, Nothing,    Nothing,  Just 6, Nothing,      Nothing, Nothing,  Just 3]
            ,[ Just 4, Nothing, Nothing,     Just 8, Nothing,  Just 3,      Nothing, Nothing,  Just 1]
            ,[ Just 7, Nothing, Nothing,    Nothing,  Just 2, Nothing,      Nothing, Nothing,  Just 6]
            
            ,[Nothing,  Just 6, Nothing,    Nothing, Nothing, Nothing,       Just 2,  Just 8, Nothing]
            ,[Nothing, Nothing, Nothing,     Just 4,  Just 1,  Just 9,      Nothing, Nothing,  Just 5]
            ,[Nothing, Nothing, Nothing,    Nothing,  Just 8, Nothing,      Nothing,  Just 7,  Just 9]]

exampleBig :: Sudoku -- 25 * 25
exampleBig = foldr (\i -> update (update (const $ Just i) (25 - i)) i) emptySudoku [1..25]
    where
        emptyRow = replicate 25 Nothing
        emptySudoku = replicate 25 emptyRow

