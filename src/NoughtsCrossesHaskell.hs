{-# LANGUAGE OverloadedStrings #-}

module NoughtsCrossesHaskell where

import qualified Data.Map.Strict as Map
import Data.Either

type Position = (Int, Int)
data Player = X | O deriving (Show, Eq)
type Board = Map.Map Position Player
data GameState = GameNotStarted | GameState {
  playerX :: String,
  playerO :: String,
  nextPlayer :: Player,
  board :: Board
}

data Command
  = CreateGame String String
  | MakeMove Position Player

data GameResult = Won Player | Tie deriving (Show, Eq)

data Event
  = GameCreated String String
  | MoveMade Position Player
  | GameFinished GameResult
  deriving (Show, Eq)

data Direction
  = West
  | East
  | North
  | South
  | NorthWest
  | NorthEast
  | SouthWest
  | SouthEast

(|>) x y = y x

addDirection :: Direction -> Position -> Position
addDirection West (x, y) = (x-1, y)
addDirection East (x, y) = (x+1, y)
addDirection North (x, y) = (x, y+1)
addDirection South (x, y) = (x, y-1)
addDirection NorthWest (x, y) = (x-1, y+1)
addDirection NorthEast (x, y) = (x+1, y+1)
addDirection SouthWest (x, y) = (x-1, y-1)
addDirection SouthEast (x, y) = (x+1, y-1)

countPieces :: Board -> Position -> Player -> Direction -> Int
countPieces board pos player direction =
  case Map.lookup nextPosition board of
    Just p -> if player == p then 1 + (countPieces board nextPosition player direction) else 0
    _ -> 0
  where nextPosition = (addDirection direction) pos

piecesInRow :: Board -> Position -> Player -> (Direction, Direction) -> Int
piecesInRow board pos player (dir1, dir2) =
  1 + (countPieces board pos player dir1) + (countPieces board pos player dir2)

maxInRow :: Board -> Position -> Player -> Int
maxInRow board pos player =
  [(West, East), (North, South), (NorthWest, SouthEast), (NorthEast, SouthWest)]
  |> map (piecesInRow board pos player)
  |> maximum

handleCommand :: Command -> GameState -> [Event]
handleCommand (CreateGame a b) _ = [GameCreated a b]
handleCommand (MakeMove pos player) state
  | nextPlayer state /= player = []
  | otherwise =
  case Map.lookup pos (board state) of
    Just _ -> []
    Nothing ->
      [MoveMade pos player] ++
      if maxInRow (board state) pos player >= 5
        then [GameFinished (Won player)]
        else []

applyEvent :: GameState -> Event -> GameState
applyEvent _ (GameCreated a b) = GameState {
  playerX = a,
  playerO = b,
  nextPlayer = X,
  board = Map.empty
}
applyEvent state (MoveMade pos player) = state {
  nextPlayer = if player == X then O else X,
  board = Map.insert pos player (board state)
}

performCommand :: (GameState, [Event]) -> Command -> (GameState, [Event])
performCommand (state, events) command =
  (foldl applyEvent state newEvents, events ++ newEvents)
  where newEvents = handleCommand command state
