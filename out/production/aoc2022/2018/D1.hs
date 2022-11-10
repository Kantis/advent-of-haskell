module D1 where
import System.IO
import Data.IntSet (IntSet, member, empty, insert)
import Data.Sequence (Seq, Seq(Empty), Seq((:|>)), ViewL(EmptyL), ViewL((:<)), fromList, singleton, viewl, (><), (<|))
import Debug.Trace
import MyUtils (loadFile)

inputFile :: String
inputFile = "inputs/2018/day1.txt"

toInt :: String -> Int
toInt "" = error("empty line..")
toInt s = read s :: Int

parseNum :: String -> Int
parseNum s = toInt(if Prelude.take 1 s == ['+'] then (Prelude.drop 1 s) else s)

a :: IO ()
a = do
  inp <- loadFile inputFile
  let numbers = map parseNum (lines inp)
  print(foldr (+) 0 numbers)

b :: IO ()
b = do
  inp <- loadFile inputFile
  let numbers = map parseNum (lines inp)
  print(frequencyTracker empty 0 (cycle numbers))

frequencyTracker :: IntSet -> Int -> [Int] -> Int
frequencyTracker visited acc (x:xs) = do
  let next = acc + x
  if (member next visited) then traceShow(visited) next
  else frequencyTracker (insert next visited) next xs

