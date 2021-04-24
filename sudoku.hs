module Sudoku
    (
        Matrix,
        Cell,
        Sudoku,
        Block,
        Wave,
        Pos,
        Info,
        isSudoku,
        isFull,
        getRows,
        getColumns,
        getBoxes,
        getWaves,
        binded,
        getBlanks,
        conflict,
        solve
    )
where

import Data.List (transpose, (\\), sortOn)
import Data.Maybe (fromJust, isNothing)

type Matrix a = [[a]]

type Cell = Maybe Int
newtype Sudoku = Sudoku (Matrix Cell)

cells :: Sudoku -> Matrix Cell
cells (Sudoku s) = s

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s) = validRows && validCols && validNums where
    validRows = length s == 9
    validCols = all ((== 9) . length) s
    validNums = all (all validNum) s
    validNum Nothing = True
    validNum (Just n) = 1 <= n && n <= 9

isFull :: Sudoku -> Bool
isFull = all (Nothing `notElem`) . cells

instance Show Sudoku where
    show = concatMap (foldr ((:) . printCell) "\n") . cells where
        printCell Nothing = '.'
        printCell (Just n) = head (show n)

type Block = [Cell]

getRows :: Sudoku -> [Block]
getRows = cells

getColumns :: Sudoku -> [Block]
getColumns = transpose . cells

getBoxes :: Sudoku -> [Block]
getBoxes = map concat . concatMap transpose . chop3 . map chop3 . cells

chop3 :: [a] -> [[a]]
chop3 [] = []
chop3 l = take 3 l : (chop3 . drop 3 $ l)

type Wave = [Int]

getWave :: Block -> Wave
getWave [] = []
getWave (Nothing : xs) = getWave xs
getWave (x : xs) = fromJust x : getWave xs

getWaves :: Sudoku -> Matrix Wave
getWaves s = zipWith3 (zipWith3 (\a b c -> [1..9] \\ concat [a, b, c])) rowNums colNums boxNums where
    rowNums = map (replicate 9 . getWave) $ getRows s
    colNums = transpose . map (replicate 9 . getWave) $ getColumns s
    boxNums = concatMap (replicate 3 . concatMap (replicate 3)) . chop3 . map getWave $ getBoxes s

type Pos = (Int, Int)
type Info = (Cell, Wave)

indexed :: Matrix a -> [(Pos, a)]
indexed = concat . zipWith3 (zipWith3 (\i j e -> ((i, j), e))) (map (replicate 9) [1..9]) (replicate 9 [1..9])

binded :: Sudoku -> [(Pos, Info)]
binded s = indexed $ zipWith zip (cells s) (getWaves s)

getBlanks :: Sudoku -> [(Pos, Info)]
getBlanks = sortOn (length . snd . snd) . filter (isNothing . fst . snd) . binded

conflict :: Sudoku -> Bool
conflict = not . any (null . snd . snd) . binded

replace :: Sudoku -> Pos -> Cell -> Sudoku
replace (Sudoku s) (x, y) c = Sudoku (take (x - 1) s ++ (take (y - 1) row ++ c : drop y row) : drop x s) where
    row = s !! (x - 1)

solve :: Sudoku -> [Sudoku]
solve s
    | conflict s = []
    | isFull s = [s]
    | otherwise = concatMap (solve . replace s pos . Just) . snd . snd . head . getBlanks $ s where
        pos = fst . head . getBlanks $ s
