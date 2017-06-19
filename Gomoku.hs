module Gomoku where

data Element = Cross | Circle | Empty deriving  (Eq, Read)


instance Show Element where
                   show Cross = "X"
                   show Circle = "O"
                   show Empty = "_"


data Board = Board { elements :: [[Element]] } deriving Eq

showelems :: [Element] -> String
showelems [] = ""
showelems (x:xs) = " " ++ ((show x) ++ " " ++ (showelems xs))

showlines :: [[Element]] -> String
showlines [] = ""
showlines (x:xs) = (showelems x) ++ show(19 - length xs)  ++ "\n" ++ (showlines xs)

instance Show Board where
    show (Board b) = showlines b ++ " 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19"

initBoard :: Board
initBoard = Board [[Empty|_<-[1..19]]|_<-[1..19]]

updateBoard :: Board -> Element -> Int -> Int -> Board
updateBoard (Board m) c x y = Board (take x m ++ [take y ( m !! x) ++ [c] ++ drop (y + 1) (m !! x)] ++ drop (x + 1) m)

positionIsEmpty :: Board -> Int -> Int -> Bool
positionIsEmpty (Board x) r c = (x !! r !! c) == Empty


checkList :: [[Element]] -> Int -> Int -> [[Element]]
checkList board x y =
    let
        getRow = board !! x
        getCol = map (!! y) board
        width  = (length (head board))+1
        height = (length board)+1

        startUL | x < y        = (1, y-x+1)
                | otherwise    = (x-y+1, 1)
        startUR | width-x < y  = (width-1, y-(width-x))
                | otherwise    = (x+y-1, 1)

        getULtoDR x y lst | x < width && y < height = getULtoDR (x+1) (y+1) (lst ++ [board !! (x-1) !! (y-1)])
                          | otherwise               = lst
        
        getURtoDL x y lst | 0 < x && y < height = getURtoDL (x-1) (y+1) (lst ++ [board !! (x-1) !! (y-1)])
                          | otherwise            = lst
    in
        [getRow, getCol, getULtoDR (fst startUL) (snd startUL) [], getURtoDL (fst startUR) (snd startUR) []]


checkFive :: [[Element]] -> Element -> Bool
checkFive xs x | xs == [] = False
               | otherwise =
      let
        checkLoop lst cnt | cnt >= 6         = True
                          | lst == []        = checkFive (tail xs) x
                          | head lst == x    = checkLoop (tail lst) (cnt+1)
                          | otherwise        = checkLoop (tail lst) 1
      in
		checkLoop (head xs) 1


