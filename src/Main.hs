module Main where
import qualified Data.List.Split as Split
import qualified Data.List as List

main :: IO ()
main = do
  x <- readFile "/proc/cpuinfo"
  putStrLn $ splitCpusInfo x

whereCpuInfo :: [Char] -> Bool
whereCpuInfo x =
  List.isInfixOf "model name" x
  || List.isInfixOf "vendor_id" x
  || List.isInfixOf "cpu MHz" x
  || List.isInfixOf "processor" x

splitCpuInfo :: String -> [String]
splitCpuInfo cpuInfoStr =
  (map (++ "\n")
  (filter whereCpuInfo
          (Split.splitOn "\n" cpuInfoStr)) ) ++ ["\n"]

splitCpusInfo :: String -> String
splitCpusInfo bigStr =
  "CPU Summary\t:\n\n" ++
  (List.intercalate ""
  . concat
  . map splitCpuInfo
      $ filter
        (/= "")
        (Split.splitOn "\n\n" bigStr))
 


