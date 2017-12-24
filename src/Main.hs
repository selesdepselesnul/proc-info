module Main where
import qualified Data.List.Split as Split

main :: IO ()
main = do
  putStrLn "hello world"

splitCpusInfo x =
  Split.splitOn "\n\n" x

readCpuInfo = do
  x <- readFile "/proc/cpuinfo"
  let xs = splitCpusInfo x
  putStrLn $ head xs
