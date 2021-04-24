# Sudoku_Func

A functional program (in haskell) to solve sudokus.

## Usage

Under the same directory as the `sudoku.hs` file:

```haskell
import Sudoku
```

Inside the module are:

```haskell
type Matrix a = [[a]]
type Cell = Maybe Int
newtype Sudoku = Sudoku (Matrix Cell)
type Block = [Cell]
type Wave = [Int]
type Pos = (Int, Int)
type Info = (Cell, Wave)

isSudoku :: Sudoku -> Bool
isFull :: Sudoku -> Bool
getRows :: Sudoku -> [Block]
getColumns :: Sudoku -> [Block]
getBoxes :: Sudoku -> [Block]
getWaves :: Sudoku -> Matrix Wave
binded :: Sudoku -> [(Pos, Info)]
getBlanks :: Sudoku -> [(Pos, Info)]
conflict :: Sudoku -> Bool
solve :: Sudoku -> [Sudoku]
```
