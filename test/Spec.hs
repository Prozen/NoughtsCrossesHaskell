
import Test.Hspec
import NoughtsCrossesHaskell
import Data.List
import Data.Maybe


generatesEvent :: [Command] -> Event -> Bool
generatesEvent commands event =
  (foldl performCommand (GameNotStarted, []) commands)
  |> snd
  |> (find (== event))
  |> isJust

main :: IO ()
main = hspec $ do
  describe "noughts and crosses" $ do
    it "5 in a horizontal row should win the game" $
      generatesEvent
        [
          (CreateGame "a" "b"),
          (MakeMove (0, 0) X),
          (MakeMove (1, 0) X),
          (MakeMove (2, 0) X),
          (MakeMove (4, 0) X),
          (MakeMove (3, 0) X)
        ]
        (GameFinished (Won X) :: Event)
