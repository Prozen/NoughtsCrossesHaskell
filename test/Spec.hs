
import Test.Hspec
import NoughtsCrossesHaskell
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map


generatesFn :: [Command] -> (Event -> Bool) -> Bool
generatesFn commands pred =
  (foldl performCommand (GameNotStarted, []) commands)
  |> snd
  |> find pred
  |> isJust

generatesEvent :: [Command] -> Event -> Bool
generatesEvent commands event =
  generatesFn commands (== event)

main :: IO ()
main = hspec $ do
  describe "noughts and crosses" $ do
    it "5 in a horizontal row should win the game" $
      generatesEvent
        [
          (CreateGame "a" "b"),
          (MakeMove (0, 0) X),
          (MakeMove (0, 2) O),
          (MakeMove (1, 0) X),
          (MakeMove (0, 3) O),
          (MakeMove (2, 0) X),
          (MakeMove (0, 4) O),
          (MakeMove (4, 0) X),
          (MakeMove (0, 5) O),
          (MakeMove (3, 0) X)
        ]
        (GameFinished (Won X) :: Event)
    it "nextPlayer is enforced" $
      not $ generatesFn
        [
          (CreateGame "a" "b"),
          (MakeMove (0, 0) O)
        ]
        (\e -> case e of
          MoveMade _ O -> True
          _ -> False)
    it "4 random should not win the game" $
      not $ generatesEvent
        [
          (CreateGame "a" "b"),
          (MakeMove (1,1) X),
          (MakeMove (1,2) O),
          (MakeMove (2,1) X),
          (MakeMove (3,3) O),
          (MakeMove (3,1) X),
          (MakeMove (2,2) O),
          (MakeMove (4,1) X),
          (MakeMove (0,1) O)
        ]
        (GameFinished (Won O) :: Event)
    it "4 random should not win the game" $
      not $ generatesEvent
        [
          (CreateGame "a" "b"),
          (MakeMove (1,1) X),
          (MakeMove (2,1) O),
          (MakeMove (22,1) X),
          (MakeMove (13,3) O),
          (MakeMove (3,1) X),
          (MakeMove (4,8) O),
          (MakeMove (4,1) X),
          (MakeMove (0,1) O)
        ]
        (GameFinished (Won O) :: Event)
    it "piecesInRow before is one more" $
      piecesInRow (Map.fromList [((1,0), X),((2,0), X)]) (0,0) X (West, East)
      `shouldBe` 3
    it "piecesInRow one side" $
      piecesInRow (Map.fromList [((1,0), X),((2,0), X)]) (1,0) X (West, East)
      `shouldBe` 2
    it "piecesInRow other side" $
      piecesInRow (Map.fromList [((1,0), X),((2,0), X)]) (2,0) X (West, East)
      `shouldBe` 2
    it "piecesInRow does not count other player" $
      piecesInRow (Map.fromList [((1,0), X),((2,0), X)]) (2,0) O (West, East)
      `shouldBe` 1
    it "maxInRow counts pieces" $
      maxInRow (Map.fromList [((1,0), X),((2,0), X)]) (2,0) X
      `shouldBe` 2
    it "maxInRow counts pieces in all directions" $
      maxInRow (Map.fromList [((1,0), X),((2,0), X),((3,0), X),((2,1), X)]) (2,0) X
      `shouldBe` 3
    it "maxInRow assumes position is for same player" $
      maxInRow (Map.fromList [((1,0), X),((2,0), X)]) (2,0) O
      `shouldBe` 1
