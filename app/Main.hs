module Main where

import NoughtsCrossesHaskell
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B

main :: IO ()
main = startGame

position :: Parser Position
position = do
    x <- decimal
    char ','
    y <- decimal
    pure (x, y)

parseX :: Parser Player
parseX = do
  char 'X'
  pure X

parseO :: Parser Player
parseO = do
  char 'O'
  pure O

parsePlayer :: Parser Player
parsePlayer = parseX <> parseO

moveCommand :: Parser Command
moveCommand = do
  pos <- position
  skipSpace
  p <- parsePlayer
  pure (MakeMove pos p)

-- (a -> b -> a) -> a -> [b] -> a
-- printBoard :: Board -> IO ()
-- printBoard b = do
--   keys b
--   |> foldl (\s -> (x, y) -> )

loggingPerformCommand :: GameState -> Either String Command -> IO GameState
loggingPerformCommand state (Left error) = do
  print "Unknown command. Enter move like 'xpos,ypos X|O', eg '0,0 X'"
  return state
loggingPerformCommand state (Right command) = do
  print events
  print (board newState)
  return newState
  where (newState, events) = performCommand (state, []) command

gameLoop :: GameState -> IO ()
gameLoop state = do
  l <- B.getLine
  newState <- loggingPerformCommand state (parseOnly moveCommand l)
  gameLoop newState


startGame :: IO ()
startGame = do
  print "Welcome to Noughts and Crosses"
  gameLoop (applyEvent GameNotStarted (GameCreated "a" "b"))