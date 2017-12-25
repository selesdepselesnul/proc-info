module Main where
import qualified Data.List.Split as Split
import qualified Data.List as List

main :: IO ()
main = do
  putStrLn "hello world"


whereCpuInfo x =
  List.isInfixOf "model name" x
  || List.isInfixOf "vendor_id" x
  || List.isInfixOf "cpu MHz" x
  || List.isInfixOf "processor" x

splitCpuInfo cpuInfoStr =
  filter whereCpuInfo
         $ Split.splitOn "\n" cpuInfoStr

splitCpusInfo bigStr =
  map splitCpuInfo
      $ filter
        (/= "")
        (Split.splitOn "\n\n" bigStr)

readCpuInfo = do
  x <- readFile "/proc/cpuinfo"
  let xs = splitCpusInfo x
  putStrLn (show xs)









