{-# LANGUAGE TypeApplications #-}

import qualified Data.Set as Set
import qualified System.Environment as Environment

main :: IO ()
main = do
  args <- Environment.getArgs
  let
    count = case args of
      [] -> 20
      text : _ -> read @Int text
  mapM_ print $ countMultResultSizesToInclusive count

countAdditionResultSizesToInclusive :: Int -> [(Int, Int)]
countAdditionResultSizesToInclusive count = take count $ fmap (\x -> (x, countAdditionResultSize x)) [0..]

countAdditionResultSize :: Int -> Int
countAdditionResultSize maxInt = Set.size $ Set.fromList results
  where
  lefts = [0..maxInt]
  rights = [0..maxInt]
  results :: [Int]
  results = [x + y | x <- lefts, y <- rights]

countMultResultSizesToInclusive :: Int -> [(Int, Int)]
countMultResultSizesToInclusive count = take count $ fmap (\x -> (x, countMultResultSize x)) [0..]

countMultResultSize :: Int -> Int
countMultResultSize maxInt = Set.size $ Set.fromList results
  where
  lefts = [0..maxInt]
  rights = [0..maxInt]
  results :: [Int]
  results = [x * y | x <- lefts, y <- rights]
