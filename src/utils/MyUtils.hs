module MyUtils (loadFile) where
import System.IO

loadFile :: String -> IO String
loadFile inputFile =
  openFile inputFile ReadMode >>= hGetContents