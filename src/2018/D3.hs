module D3 where
import Data.IntSet (IntSet, member, empty, insert)
import Data.Sequence (Seq, Seq(Empty), Seq((:|>)), ViewL(EmptyL), ViewL((:<)), fromList, singleton, viewl, (><), (<|))
import Debug.Trace
import qualified Data.Map.Strict as M (Map, insertWith, empty, filter)
import MyUtils

inputFile :: String
inputFile = "inputs/2018/day3.txt"

data Coordinate = {
  x :: Int,
  y :: Int
  }
  
data class Size = {
  width :: Int,
  height:: Int
  }

data Claim = {
    pos :: Coordinate
    size :: Size
  }

parseClaim :: String -> Claim

a :: IO ()
a = do
  inp <- loadFile inputFile
  let ids = lines inp
  print(containExactlyThree ids * containExactlyTwo ids)

diff :: String -> String -> Int
diff [] _ = 0
diff _ [] = 0
diff (c1:a) (c2:b) =
  diff a b +
  if (c1 == c2) then 0
  else 1

findMatch :: String -> [String] -> Maybe String
findMatch _ [] = Nothing
findMatch s (x:xs) =
  if (diff s x == 1) then Just x
  else findMatch s xs

findTheMatch :: [String] -> (String, String)
findTheMatch (x:xs) = case findMatch x xs of
  Just match -> (x, match)
  Nothing -> findTheMatch xs

b :: IO ()
b = do
  inp <- loadFile inputFile
  let ids = lines inp
  print(findTheMatch ids)