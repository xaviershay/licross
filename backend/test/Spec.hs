{-# LANGUAGE OverloadedStrings #-}

-- Don't really know where these specs should go yet...
module Spec where

-- tasty
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- unordered-containers
import qualified Data.HashMap.Strict as M

import Licross.FakeData

-- text
import qualified Data.Text as T

-- licross
import Licross.Prelude
import Licross.Types
import Licross.Api

instance Arbitrary Position where
  arbitrary = pure mkPos <*> arbitrary <*> arbitrary
  shrink = genericShrink

test_Skeleton = testCase "Works" $ 1 @=? 1

mkGame :: [[Bonus]] -> Game
mkGame spec =
  let board =
        M.map (\x -> set spaceBonus x emptySpace) . M.fromList . listToPositions $
        spec
   in set gameBoard board emptyGame

listToPositions :: [[a]] -> [(Position, a)]
listToPositions input =
  concatMap
    (\(row, y) -> map (\(elem, x) -> (mkPos x y, elem)) $ zip row [0 ..]) $
  zip input [0 ..]

positionsToList :: [(Position, a)] -> [[a]]
positionsToList =
  map (map snd) . groupBy ((==) `on` (yPos . fst)) . sortBy (compare `on` fst)

test_listToPositions =
  testGroup
    "listToPositions"
    [ testCase "example" $
      [(mkPos 0 0, 1), (mkPos 0 1, 2), (mkPos 1 1, 3)] @=?
      listToPositions [[1], [2, 3]]
    , testProperty "preserves element count" $ \x ->
        length (concat x) == length (listToPositions (x :: [[Int]]))
    -- TODO: , testProperty "is contiguous"
    ]

test_positionsToList =
  testGroup
    "positionsToList"
    [ testCase "example" $
      [[1], [2, 3]] @=?
      positionsToList [(mkPos 0 0, 1), (mkPos 0 1, 2), (mkPos 1 1, 3)]
    , testProperty "preserves element count" $ \x ->
        length x == length (concat $ positionsToList (x :: [(Position, Int)]))
    -- TODO: , testProperty "is contiguous"
    ]

--extractLetters :: Board -> [[T.Text]]
--extractLetters board =
--   positionsToList . M.toList .
--   M.map (extractLetter . view spaceOccupant) $
--   board

--extractLetter :: Maybe PlacedTile -> T.Text
--extractLetter (Just (PlacedTile text _)) = text
--extractLetter Nothing = ""

extractLetter :: Game -> Position -> T.Text
extractLetter game position =
  case filter (\x -> view tileLocation x == LocationBoard position) $ (M.elems $ view gameTiles game) of
    [x] -> toText $ view tileLetter x
    _ -> ""

  where
    toText (Letter x) = x
    toText Blank = ""

test_PlayTiles =
  testGroup
    "Move: PlayTiles"
    [ testCase "adds tiles to board" $
      let expected = ["A", "B", ""]
       in let actual =
                applyMove
                  (PlayTiles
                        [ set tileLocation (LocationBoard $ mkPos 0 0) $ mkTile 1 "A" 0
                        , set tileLocation (LocationBoard $ mkPos 1 0) $ mkTile 2 "B" 0
                        ]) $
                set
                  gameTiles
                  (M.fromList . map (\x -> (view tileId x, x)) $
                    [ mkTile 1 "A" 0
                    , mkTile 2 "B" 0
                    ]
                  )
                $ mkGame [[None, None, None]]
           in expected @=? (map (extractLetter actual) [mkPos 0 0, mkPos 1 0, mkPos 2 0])
    ]
