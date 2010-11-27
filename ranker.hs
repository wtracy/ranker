import Data.Char
import List
import System.Process
import System.IO
import System.Environment
import Directory

-- Returns the contents of the working directory as a newline-delimited string
listDirectory :: IO String
listDirectory = do 
  readProcess "ls" [] []

-- Converts a list of strings into a list of string-integer pairs where all the
-- integers are initialized to zero.
initializeData :: [String] -> [(String, Integer)]
initializeData input = zip input (repeat 0)

-- Currently just a wrapper for initializeData
buildData :: [String] -> [(String, Integer)]
buildData input = initializeData input

-- Given a file handle, parse the contents assuming that it is a text file
-- where every other line is an arbitrary string, and the remaining lines
-- are decimal representations of integers.
doReadData :: Handle -> IO [(String, Integer)]
doReadData handle = do
    --putStrLn "doReadData called"
    done <- hIsEOF handle
    if (not done)
	then do
            name <- hGetLine handle
            scoreString <- hGetLine handle
            theRest <- (doReadData handle)
            let score = (read scoreString)
            return ((name, score) : theRest)
        else return []

-- Opens a data file and parses it.
readData :: FilePath -> IO [(String, Integer)]
readData path = do
    exists <- doesFileExist path
    if (exists)
	then do
            handle <- openFile path ReadMode
	    list <- doReadData handle
	    hClose handle
	    return list
        else return []

-- Dumps the data to standard output as a tab- and newline-delimited table.
printData :: [(String, Integer)] -> IO ()
printData [] = putStr ""
printData list = do
  putStr (fst (head list))
  putStr "\t"
  putStrLn (show (snd (head list)))
  printData (tail list)

-- Writes each entry to the given file handle, as one line containing only
-- the string value from the pair, and a second line containing a decimal
-- representation of the integer value from the pair.
dumpToFile :: Handle -> [(String, Integer)] -> IO ()
dumpToFile handle [] = return ()
dumpToFile handle list = do
    hPutStrLn handle (fst (head list))
    hPutStrLn handle (show (snd (head list)))
    dumpToFile handle (tail list)

-- Opens a file. Dumps the data out to said file.
dumpData :: [(String, Integer)] -> IO ()
dumpData list = do
    --putStrLn "Opening file for output"
    handle <- openFile ".rank" WriteMode
    dumpToFile handle ((sortBy score list))
    hClose handle

-- Prompts the user to rate the given pair of images, and records the result.
processPair :: (String, Integer) -> (String, Integer) -> IO [(String, Integer)]
processPair x y = do
  first <- runProcess ("display") [(fst x)] Nothing Nothing Nothing Nothing Nothing
  second <- runProcess ("display") [(fst y)] Nothing Nothing Nothing Nothing Nothing
  putStrLn ("1. " ++ (fst x) ++ " (currently rated " ++ (show (snd x)) ++ ")")
  putStrLn ("2. " ++ (fst y) ++ " (currently rated " ++ (show (snd y )) ++ ")")
  putStrLn "Pick your favorite. Enter 1 or 2: "
  answer <- getLine
  --putStrLn "Closing windows ..."
  resultA <- terminateProcess first
  resultB <- terminateProcess second
  --putStrLn "Closed windows."
  case answer of
    "1" -> return [((fst x), ((snd x) + 1)), ((fst y), ((snd y) - 1))]
    "2" -> return [((fst x), ((snd x) - 1)), ((fst y), ((snd y) + 1))]
    z -> return []

-- Splits the entries into sets of two and prompts the user to compare each
-- pair.
doProcessData :: [(String, Integer)] -> IO (Bool, [(String, Integer)])
doProcessData [] = return (True, [])
doProcessData [x] = return (True, [x])
--doProcessData [x, y, z] = do
--  processedB <- processPair y x
--  processedA <- processPair (head processedB) z
--  return (processedA ++ (tail processedB))
doProcessData list = 
  let
    x = head list
    y = head (tail list)
    remainder = tail (tail list)
  in do
    processed <- processPair x y
    if (processed == [])
      then return (False, list)
      else do
        processedRemainder <- doProcessData remainder
        return ((fst processedRemainder), (processed ++ (snd processedRemainder)))

-- sorts pairs by score, ignoring file names
score :: (String, Integer) -> (String, Integer) -> Ordering
-- sort in descending order
score x y = compare (snd y) (snd x)
-- sort ascending
--score x y = compare (snd x) (snd y)

processData :: [(String, Integer)] -> IO [(String, Integer)]
processData x = do
  result <- doProcessData (sortBy score x)
  let resultData = (snd result)
  if (fst result)
    then (processData (sortBy score (resultData)))
    else return (resultData)

main :: IO ()
main = do
  args <- getArgs
  if ((length args) > 0)
      then setCurrentDirectory (head args)
      else return ()
  dir <- getCurrentDirectory
  putStrLn dir
  list <- listDirectory
  archive <- readData ".rank"
  let
    files = buildData (lines list)
    archiveFiles = map fst archive
    currentFiles = map fst files
    filteredArchive = [x | x <- archive, (fst x) `elem` currentFiles]
    filteredFiles = [x | x <- files, not ((fst x) `elem` archiveFiles)]
    input = filteredArchive ++ filteredFiles
  --printData (sortBy score input)
  results <- processData (input)
  dumpData results 
  --printData (sortBy score results)
