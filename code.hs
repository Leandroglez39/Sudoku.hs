
import Data.List
import Data.Array


nonominos :: Int -> [[[Int]]]   
nonominos 1 = [[[3],[5]],[[0,0,0,1,0],[-1,0,0,0,-1],[-1,-1,0,-1,-1]]]
nonominos 2 = [[[2],[5]],[[-1,2,0,3,0],[0,0,0,9,0]]]
nonominos 3 = [[[4],[4]],[[6,-1,-1,-1],[0,0,-1,0],[8,0,0,0],[-1,-1,4,-1]]]
nonominos 4 = [[[3],[5]],[[0,0,0,8,0],[-1,-1,-1,0,0],[-1,-1,-1,0,0]]]
nonominos 7 = [[[5],[2]],[[0,0],[0,0],[3,0],[4,0],[0,-1]]]
nonominos 5 = [[[6],[4]],[[-1,-1,-1,7],[-1,-1,5,0],[-1,2,0,-1],[-1,9,-1,-1],[-1,1,-1,-1],[0,0,-1,-1]]]
nonominos 6 = [[[5],[3]],[[-1,4,0],[-1,6,0],[0,0,0],[-1,-1,0],[-1,-1,0]]]
nonominos 9 = [[[3],[3]],[[8,0,5],[7,0,9],[3,6,0]]]
nonominos 8 = [[[4],[3]],[[-1,0,0],[-1,0,0],[-1,0,0],[0,7,0]]]

    
tab :: [Bool]
tab = [False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False,
        False,False,False,False,False,False,False,False,False]

list_non :: [[[[[Int]]]]]
list_non = combinatory ([nonominos 1] ++ [nonominos 2] ++ [nonominos 3] ++ [nonominos 4] ++ [nonominos 5] ++ [nonominos 6]  ++ [nonominos 7] ++ [nonominos 8] ++ [nonominos 9])

combinatory:: [[[[Int]]]] -> [[[[[Int]]]]]
combinatory list = permutations list

insert_non :: [[Int]] -> [Bool]
insert_non non = [True]

convert_non :: [[[Int]]] -> [[Bool]]
convert_non non = [[if ((non!!1!!x!!y) < 0 ) then False else True |y <- [0..(non!!0!!1!!0 - 1)]]| x <- [0..(non!!0!!0!!0 - 1)]]


convert_non1 :: [[[Int]]] -> [[Int]]
convert_non1 non = non!!1

convert_all_non :: [[[Bool]]]
convert_all_non = [ convert_non x | x <- list_all_non]

list_all_non :: [[[[Int]]]]
list_all_non = [nonominos 1] ++ [nonominos 2] ++ [nonominos 3] ++ [nonominos 4] ++ [nonominos 5] ++ [nonominos 6]  ++ [nonominos 7] ++ [nonominos 8] ++ [nonominos 9]

pat ::Int -> [Int] -> [Bool]
pat a y = 
    let x = k
    in [t |t <- x , t == True ]     
    where
    k = tab
    k!!0 = True
    


su :: Int -> Int
su x = x + y + e + v 
    where 
    y = 1
    e = 2 
    v = 4

update :: [Bool] ->  Int -> [Bool]
update l n = (take n l)  ++ [True] ++ (drop (n+1) l) 

elem1:: ([[Int]],Int,[Char]) -> [[Int]]
elem1 (a,_,_) = a
elem2:: ([[Int]],Int,[Char]) ->Int
elem2 (_,a,_) = a
elem3:: ([[Int]],Int,[Char]) -> [Char]
elem3 (_,_,a) = a

append:: [Int] -> Int -> [Int]
append lis x = lis ++ [x]

set1:: [Bool]-> [[Bool]] -> Int -> Int -> ([Bool],Int,Int)
set1 board non 0 0 = ([True],1,1)

clean:: [[Bool]] -> [[Bool]]
clean lis = [ [  (x!!y)  | y <- [0..((length x) -1)], (x!!y)==True ] | x <-lis]

clean1:: [[Int]] -> [[Int]]
clean1 lis = [ [  (x!!y)  | y <- [0..((length x) -1)], (x!!y)/=(-1) ] | x <-lis]

inserted:: [[Int]] -> [[Int]] -> Int -> [[Int]]
inserted lis non x
    | x == 0 = [(lis!!i) ++ (non!!i) |i <- [0..((length non) -1)]] ++ [ lis!!j | j <- [(x+(length non))..((length lis) -1)]]
    |otherwise = [lis!!i | i <-[0..x-1]] ++ [lis!!(x+j) ++ non!!j | j <- [0..((length non)-1)]] ++ [ lis!!k | k <- [(x+(length non))..((length lis) -1)]]   


build_board:: [[Int]] -> [[Int]] -> [[Int]]
build_board lis non 
    | ((length (lis!!0)) < 9 && (9- (length (lis!!0)) >= (length (non!!0)) )) =  inserted lis non 0
    | ((length (lis!!1)) < 9 && (9- (length (lis!!1)) >= length (non!!0) )) = inserted lis non 1
    | ((length (lis!!2)) < 9 && (9- (length (lis!!2)) >= length (non!!0) )) = inserted lis non 2
    | ((length (lis!!3)) < 9 && (9- (length (lis!!3)) >= length (non!!0) )) = inserted lis non 3
    | ((length (lis!!4)) < 9 && (9- (length (lis!!4)) >= length (non!!0) )) = inserted lis non 4
    | ((length (lis!!5)) < 9 && (9- (length (lis!!5)) >= length (non!!0) )) = inserted lis non 5
    | ((length (lis!!6)) < 9 && (9- (length (lis!!6)) >= length (non!!0) )) = inserted lis non 6
    | ((length (lis!!7)) < 9 && (9- (length (lis!!7)) >= length (non!!0) )) = inserted lis non 7
    | otherwise = inserted lis non 8


non1 = clean1 (convert_non1 (nonominos 1))
non2 = clean1 (convert_non1 (nonominos 2))
non3 = clean1 (convert_non1 (nonominos 3))
non4 = clean1 (convert_non1 (nonominos 4))
non5 = clean1 (convert_non1 (nonominos 5))
non6 = clean1 (convert_non1 (nonominos 6))
non7 = clean1 (convert_non1 (nonominos 7))
non8 = clean1 (convert_non1 (nonominos 8))
non9 = clean1 (convert_non1 (nonominos 9))

x1 = build_board [[],[],[],[],[],[],[],[],[]] non1
x2 = build_board x1 non2
x3 = build_board x2 non3
x4 = build_board x3 non4
x5 = build_board x4 non5
x6 = build_board x5 non6
x7 = build_board x6 non7
x8 = build_board x7 non8
x9 = build_board x8 non9

--pat 2 y@(x:xs) = if True
--    then y ++ [2] 
--   else ""
   




-- Solve the example puzzle specified below
-- TODO: read puzzle from input
main = do
    let solution = solve puzzleBoard
    printBoard solution

-- The marks on the board are represented by Ints in the range 0..9, where 0 represents "empty".
type Mark = Int

-- A square is identified by a (row, column) pair
type Location = (Int, Int)

-- A sudoku board is a 9x9 matrix of marks
type Board = Array Location Mark

-- The sudoku board to be solved
puzzleBoard :: Board
puzzleBoard = array ((0, 0), (8, 8)) $ puzzleAssocs examplePuzzle3

-- Example puzzle from http://en.wikipedia.org/wiki/Sudoku
examplePuzzle :: [[Mark]]
examplePuzzle = [[5, 3, 0,  0, 7, 0,  0, 0, 0],
                 [6, 0, 0,  1, 9, 5,  0, 0, 0],
                 [0, 9, 8,  0, 0, 0,  0, 6, 0],

                 [8, 0, 0,  0, 6, 0,  0, 0, 3],
                 [4, 0, 0,  8, 0, 3,  0, 0, 1],
                 [7, 0, 0,  0, 2, 0,  0, 0, 6],

                 [0, 6, 0,  0, 0, 0,  2, 8, 0],
                 [0, 0, 0,  4, 1, 9,  0, 0, 5],
                 [0, 0, 0,  0, 8, 0,  0, 7, 0]]

examplePuzzle1 :: [[Mark]]
examplePuzzle1 = [[0, 0, 0,  0, 0, 0,  0, 0, 0],
                  [0, 0, 0,  0, 0, 0,  0, 0, 0],
                  [0, 0, 0,  0, 0, 0,  0, 0, 0],
                  [0, 0, 0,  0, 0, 0,  0, 0, 0],
                  [0, 0, 0,  0, 0, 0,  0, 0, 0],
                  [0, 0, 0,  0, 0, 0,  0, 0, 0],
                  [0, 6, 0,  0, 0, 0,  0, 0, 0],
                  [0, 0, 0,  0, 0, 0,  0, 0, 0],
                  [0, 0, 0,  0, 0, 0,  0, 0, 0]]   
                  
examplePuzzle2 :: [[Mark]]
examplePuzzle2 = x9   

examplePuzzle3 :: [[Mark]]
examplePuzzle3 = [[0, 0, 0,  1, 0, 2,  0, 3, 0],
                  [6, 0, 0,  0, 0, 0,  0, 9, 0],
                  [0, 0, 0,  0, 0, 0,  0, 8, 0],
                  [8, 0, 0,  0, 7, 4,  0, 0, 0],
                  [0, 0, 4,  5, 0, 6,  0, 0, 0],
                  [0, 0, 2,  0, 0, 0,  0, 0, 0],
                  [3, 0, 9,  8, 0, 5,  0, 0, 0],
                  [4, 0, 1,  7, 0, 9,  0, 0, 0],
                  [0, 0, 0,  3, 6, 0,  0, 7, 0]]  

-- Return first solution, or Nothing if no solutions found
solve :: Board -> Maybe Board
solve = headOrNothing . solutions

-- Return all solutions
solutions :: Board -> [Board]
solutions b = solutions' (emptyLocations b) b
  where
    -- Given list of empty locations on a board, pick an empty location,
    -- determine which marks can be put in that location, and then
    -- recursively find all solutions for that set of marks.
    solutions' :: [Location] -> Board -> [Board]
    solutions' []     b = [b]
    solutions' (x:xs) b = concatMap (solutions' xs) candidateBoards
      where
        candidateMarks  = [m | m <- [1..9], isPossibleMark m x b]
        candidateBoards = map (\m -> copyWithMark m x b) candidateMarks

-- Return list of locations where value is 0
emptyLocations :: Board -> [Location]
emptyLocations b = [(row, col) | row <- [0..8], col <- [0..8], b ! (row, col) == 0]

-- Determine whether the specified mark can be placed at specified position
isPossibleMark :: Mark -> Location -> Board -> Bool
isPossibleMark m (row, col) b = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem m $ b `marksInRow` row
    notInColumn = notElem m $ b `marksInColumn` col
    notInBox    = notElem m $ b `marksIn3x3Box` (row, col)

-- Return board with specified value in specified Location
copyWithMark :: Mark -> Location -> Board -> Board
copyWithMark mark (row, col) b = b // [((row, col), mark)]

-- Return the marks in the specified row
marksInRow :: Board -> Int -> [Mark]
b `marksInRow` row = [b ! loc | loc <- range((row, 0), (row, 8))]

-- Return the marks in the specified column
marksInColumn ::  Board -> Int -> [Mark]
b `marksInColumn` col = [b ! loc | loc <- range((0, col), (8, col))]

-- Return the marks in the 3x3 box that includes the specified Location
marksIn3x3Box :: Board -> Location -> [Mark]
b `marksIn3x3Box` (row, col) = [b ! loc | loc <- locations]
  where
    row' = (row `div` 3) * 3
    col' = (col `div` 3) * 3
    locations = range((row', col'), (row' + 2, col' + 2))

-- Convert a list of rows of marks (as in examplePuzzle above) to a list of array associations
puzzleAssocs :: [[Mark]] -> [(Location, Mark)]
puzzleAssocs = concatMap rowAssocs . zip [0..8]
  where
    rowAssocs :: (Int, [Mark]) -> [((Int, Int), Mark)]
    rowAssocs (row, marks) = colAssocs row $ zip [0..8] marks

    colAssocs :: Int -> [(Int, Mark)] -> [((Int, Int), Mark)]
    colAssocs row cols = map (\(col, m) -> ((row, col), m)) cols

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

printBoard :: Maybe Board -> IO ()
printBoard Nothing  = putStrLn "No solution"
printBoard (Just b) = mapM_ putStrLn [show $ b `marksInRow` row | row <- [0..8]]