module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------
{-
gameOver :: Board -> Bool
gameOver gvnBoard
  = gameOverR gvnBoard || gameOverC gvnBoard || gameOverD gvnBoard
    where 
      r @ (currentR @ (c1 : restC) : rest) = rows gvnBoard
      currentR @ (c1:restC)
      c = cols gvnBoard
      d = diags gvnBoard
      gameOverR :: Board -> Bool
      gameOver []
        = False
      gameOverR gvn 
        | c1 == Empty   = False
        | c1 == Taken X = gameOverRFoundx rest
        | c1 == Taken Y = gameOverRFoundy rest
          where 
            gameOverRFoundx gvn'
              | c1
              -}  -- Was taking way too long

gameOver :: Board -> Bool
gameOver gvnBoard
  = foldr1 (||) (map examine (rows gvnBoard)) || foldr1 (||) (map examine (cols gvnBoard)) || foldr1 (||) (map examine (diags gvnBoard))
    where 
      examine :: [Cell] -> Bool
      examine (x1 : x2 : xs)
        | x1 == x2 && x1 /= Empty  = examine (x2 : xs)
        | otherwise                = False
      examine (x : []) -- is it better to write it as [x]? I like the option I used more
        = True
      examine []
        = True

{-
gameOver :: Board -> Bool
gameOver gvnBoard
  = or (map examine (rows gvnBoard)) || or (map examine (cols gvnBoard)) || or (map examine (diags gvnBoard))
    where 
      examine :: [Cell] -> Bool
      examine (x1 : x2 : xs)
        | x1 == x2 && x1 /= Empty  = examine (x2 : xs)
        | otherwise                = False
      examine x : [] -- is it better to write it as [x]? I like the option I used more
        = True
      examine []
        = True
-}  -- Found out there is a function "or"



-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--


{- 
parsePosition :: String -> Maybe Position
parsePosition all 
  = Just (from (readMaybe (helper all) :: Maybe Int), from (readMaybe (helper' all) :: Maybe Int))
    where 
      helper (chr : str)
        | chr == ' '  = []
        | chr /= ' ' = chr : helper str
      helper [] = "-1"
      helper' (chr : str)
        | chr == ' '  = str
        | chr /= ' ' = helper str
      helper' [] = "-1"     
      from :: Maybe a -> a
      from (Just a) = a
-}

{-
parsePosition :: String -> Maybe Position
parsePosition
  = final . map (readMaybe :: Maybe Int) .words
    where
      final :: [Maybe Int] -> Maybe Position
      final [Just x, Just y] = Just (x,y)
      final _                 = Nothing
-}                                                -- Why didn't this work?

parsePosition :: String -> Maybe Position
parsePosition str
  = check (map readIt (words str))
    where 
      readIt x = readMaybe x :: Maybe Int
      check :: [Maybe Int] -> Maybe Position
      check [Just a, Just b] = Just (a, b)
      check _                = Nothing


  {- | (readMaybe chr :: Maybe Int) == Just Int && (readMaybe str :: Maybe Int) == Just Int = Just (readMaybe chr :: Maybe Int, readMaybe str :: Maybe Int)
     | otherwise = Nothing  -}

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p pos@(x, y) board@(boardList, n)
  | x > n - 1 || x < 0 = Nothing
  | y > n - 1 || y < 0 = Nothing
  | drop (x * n + y) (take (x * n + y + 1) boardList) == [Empty] = Just ((take (x * n + y) boardList ++ [Taken p] ++ drop (x * n + y + 1) boardList) , n)
  | otherwise = Nothing

  {-
  I could use a varaible z that is equal to "drop (x * n + y) (take (x * n + y + 1) boardList) == [Empty]" to make the code
    more readable
  Found out later that i ca n use boardList !! (x * n + y) to extract the (x * n + y)th element
  Decided to stick to the more complicated version because it is easier to read for me since it is a 
    direct representation of how I computed the problem in my head
  Hopefully this doesn't affect my style points 
  -}
-------------------------------------------------------------------
-- I/O Functions

prettyPrint :: Board -> IO ()
prettyPrint board@(boardList, n)
  = putStrLn (prettyPrint' boardList 0)
    where 
      prettyPrint' :: [Cell] -> Int -> String   -- nu merge, trebuie sa ma mai uit peste numere dar ideea e buna
      prettyPrint' boardList'@(c:cs) i
        | i == 0              = printHead n         
        | mod i n == 0        = "|" ++ (check c) ++ "|" ++ "\n" ++ prettyPrint' cs (i + 1)
        | otherwise           = "|" ++ (check c) ++ prettyPrint' cs (i + 1)
          where
            printHead 0     = [] ++ "\n" ++ prettyPrint' boardList' 1
            printHead nr    
              | nr == n     =  " " ++ "_" ++ printHead (nr - 1)
              | otherwise   =  " " ++ "_" ++ printHead (nr - 1)
            check (Taken O) = "O"
            check (Taken X) = "X"
            check Empty     = " "
      prettyPrint' [] i       = printFinal n
          where
            printFinal 0    = []
            printFinal nF   = " " ++ "‾" ++ printFinal (nF - 1)
            
-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).

doParseAction :: (String -> Maybe a) -> String -> IO a
doParseAction fct strError =
  do
    x <- getLine
    if isJust (fct x) then return (fromJust (fct x))
      else putStrLn strError >> doParseAction fct strError

myFlip :: (a -> b -> c -> d) -> (a -> c -> b -> d)
myFlip fct a c b = fct a b c  -- Is this a good way to flip?
  
{-
takeTurn :: Board -> Player -> IO (Board)
takeTurn board@(boardList, n) player
  |(tryMove X input board) == Nothing = putStrLn "Nah man" >> takeTurn board player
  |otherwise                          = prettyPrint (fromJust (tryMove player input board))
    where 
      input = fromJust parsePosition getLine
-}


takeTurn :: Board -> Player -> IO (Board)
takeTurn board@(boardList, n) player =
  do
    putStrLn "Great choice!    Or is it? \n Please input a position (y-x):"
    pos <- doParseAction parsePosition "Invalid position, try something like '0 2'" 
    if isJust (tryMove player pos board) then return (fromJust (tryMove player pos board))
      else putStrLn "Invalid move. Try again!" >> takeTurn board player


-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.

playGame :: Board -> Player -> IO ()
playGame board@(boardList, n) player =
  do
    prettyPrint board
    board' <- takeTurn board player
    if gameOver board' then prettyPrint board' >> putStrLn "Game over! Good job winner! Try harder loser!"
      else playGame board' (switchPlayer player)


switchPlayer :: Player -> Player
switchPlayer player
  | player == X = O
  | player == O = X  

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main =
  do
    putStrLn "\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n" 
    putStrLn "Hello players! Welcome to TicTacTudor! \n"
    putStrLn "The first player is 'X'! \n"
    putStrLn "Please type the board size! (single number) \n"
    boardSize <- doParseAction readMaybe "\n That is not an int. Please insert an int!! \n" :: IO Int
    playGame ((take (boardSize * boardSize) (repeat Empty), boardSize)) X


  
-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
