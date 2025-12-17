{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import AllDays
import System.IO (readFile')
import Data.List.Split (splitOn)
import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.Foldable (forM_)
import Text.Printf (printf)

main :: IO ()
main = getExpectedResponses >>= \expected ->
  hspec $ do
    forM_ (zip [1..] expected) $ \(day, parts) ->
      describe (printf "Day %02d" day) $ do
        forM_ (zip [1..] parts) $ \(part, expectedResponse) ->
          describe (printf "Part %d" part) $ do
            it "should return the correct response" $ do
              let fn = getInputFile' day >>= pure . (allDays !! (day - 1) !! (part - 1))
              fn `shouldReturn` expectedResponse

getInputFile' :: Int -> IO String
getInputFile' = readFile' . printf "assets/day%02d-input.txt"

getExpectedResponses :: IO [[Int]]
getExpectedResponses =
  readFile' "assets/expected-responses.txt"
  <&> (
    lines
    >>> drop 1
    >>> map (map (read @Int) . splitOn ",")
  )
