module Main where
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Control.Exception as Exception
import qualified System.Environment as Environment

printIfSuccess :: IO String -> IO ()
printIfSuccess f = do
  result <- Exception.try (f) :: IO (Either Exception.SomeException [Char])
  case result of
    Left ex  -> putStrLn $ "ups something wrong -> " ++ show ex
    Right val -> putStrLn val
        
procPath :: String
procPath = "/proc/"

readProc :: (String -> String) -> String -> IO String
readProc f path = 
  (readFile $ procPath ++ path) >>= return . f

whereCpuInfo :: [Char] -> Bool
whereCpuInfo x =
  let isMatchStr =
        flip List.isInfixOf x
  in
    isMatchStr "model name"
    || isMatchStr "vendor_id"
    || isMatchStr "cpu MHz"
    || isMatchStr "processor"

extractCpuInfo :: String -> [String]
extractCpuInfo cpuInfoStr =
  (map
   (++ "\n")
   (filter whereCpuInfo
          (Split.splitOn "\n" cpuInfoStr)))
  ++ ["\n"]

extractCpusInfo :: String -> String
extractCpusInfo bigStr =
  "CPU Summary\t:\n\n"
  ++ (List.intercalate ""
      . concat
      . map extractCpuInfo
        $ filter
          (/= "")
          (Split.splitOn "\n\n" bigStr))
 
main :: IO ()
main = do
    args <- Environment.getArgs
    if (length args) == 0 then
      putStrLn "please, fill the argument"
    else
      printProc $ head args
    where
      printProc arg =  
          case arg of
          "--cpuinfo" -> printIfSuccess $ readProc extractCpusInfo "cpuinfo"
          "--nixversion" -> printIfSuccess $ readProc id "version"
          _ -> putStrLn "argument doesnt valid"






