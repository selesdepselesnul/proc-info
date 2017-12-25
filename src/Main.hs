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

isMatchStr :: String -> String -> Bool
isMatchStr x y = flip List.isInfixOf x y

whereCpuInfo :: [Char] -> Bool
whereCpuInfo x =
  let isMatchCpuInfo = isMatchStr x
  in
    isMatchCpuInfo "model name"
    || isMatchCpuInfo "vendor_id"
    || isMatchCpuInfo "cpu MHz"
    || isMatchCpuInfo "processor"

extractCpuInfo :: String -> [String]
extractCpuInfo cpuInfoStr =
  map
   (++ "\n")
   (filter whereCpuInfo
          (Split.splitOn "\n" cpuInfoStr))
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

whereMemInfo :: String -> Bool
whereMemInfo x =
  let isMatchMemInfo = isMatchStr x
  in
    isMatchMemInfo "MemTotal"
    || isMatchMemInfo "MemFree"
    || isMatchMemInfo "MemAvailable"
  
extractMeminfo :: String -> String
extractMeminfo x =
  "Memory Summary\t:\n\n"
  ++ List.intercalate
     ""
     (map
       (++ "\n")
       (filter whereMemInfo
               (Split.splitOn "\n" x )))   
  
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
          "--meminfo" -> printIfSuccess $ readProc extractMeminfo "meminfo"
          _ -> putStrLn "argument doesnt valid"

