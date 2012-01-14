import Data.Char
import List
import System.Process
import System.IO
import System.Environment
import Directory
import Control.Monad

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
printData ((name, score):remainder) = do
  putStr name
  putStr "\t"
  putStrLn (show score)
  printData (remainder)

-- Writes each entry to the given file handle, as one line containing only
-- the string value from the pair, and a second line containing a decimal
-- representation of the integer value from the pair.
dumpToFile :: Handle -> [(String, Integer)] -> IO ()
dumpToFile handle [] = return ()
dumpToFile handle ((name, score):remainder) = do
    hPutStrLn handle name
    hPutStrLn handle (show score)
    dumpToFile handle remainder

-- Opens a file. Dumps the data out to said file.
dumpData :: [(String, Integer)] -> IO ()
dumpData list = do
    --putStrLn "Opening file for output"
    handle <- openFile ".rank" WriteMode
    dumpToFile handle (sortBy score list)
    hClose handle

-- Prompts the user to rate the given pair of images, and records the result.
processPair :: (String, Integer) -> (String, Integer) -> IO [(String, Integer)]
processPair (xName, xScore) (yName, yScore) = do
  first <- runProcess ("feh") [xName, yName] Nothing Nothing Nothing Nothing Nothing
  --second <- runProcess ("feh") [yName] Nothing Nothing Nothing Nothing Nothing
  putStrLn ("a. " ++ (xName) ++ " (currently rated " ++ (show xScore) ++ ")")
  putStrLn ("b. " ++ (yName) ++ " (currently rated " ++ (show yScore) ++ ")")
  putStrLn "Pick your favorite. Enter a or b: "
  answer <- getLine
  resultA <- terminateProcess first
  --resultB <- terminateProcess second
  case answer of
    "a" -> return [(xName, (xScore + 1)), (yName, (yScore - 1))]
    "b" -> return [(xName, (xScore - 1)), (yName, (yScore + 1))]
    invalidInput -> return []

-- Splits the entries into sets of two and prompts the user to compare each
-- pair.
doProcessData :: [(String, Integer)] -> IO (Bool, [(String, Integer)])
doProcessData [] = return (True, [])
doProcessData [x] = return (True, [x])
doProcessData (x:y:remainder) = do
    processed <- processPair x y
    if (processed == [])
      then return (False, x:y:remainder)
      else do
        (success, processedRemainder) <- doProcessData remainder
        return (success, (processed ++ processedRemainder))

-- sorts pairs by score, ignoring file names
score :: (String, Integer) -> (String, Integer) -> Ordering
-- sort in descending order
score (_, x) (_, y) = compare y x
-- sort in ascending order
-- score (_, x) (_, y) = compare x y

processData :: [(String, Integer)] -> IO [(String, Integer)]
processData [] = do 
  putStrLn "No files to rank!"
  return[]
processData [x] = do 
  putStrLn "It doesn't make sense to rank a single file."
  return [x]
processData x = do
  (success, resultData) <- doProcessData (sortBy score x)
  when (success) $ do
    (success, resultData) <- doProcessData (sortBy score resultData)
    return ()
  return (resultData)

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
    filteredArchive = [x | x@(y, _) <- archive,   (y `elem` currentFiles)]
    filteredFiles   = [x | x@(y, _) <- files, not (y `elem` archiveFiles)]
    input = filteredArchive ++ filteredFiles
  --printData (sortBy score input)
  results <- processData (input)
  dumpData results 
  --printData (sortBy score results)
