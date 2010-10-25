import Data.Char
import List
import System.Process

listDirectory :: IO String
listDirectory = do 
  readProcess "ls" [] []

buildData :: [String] -> [(String, Integer)]
buildData input = zip input (repeat 0)

dumpData :: [(String, Integer)] -> IO ()
dumpData [] = putStr ""
dumpData list = do
  putStr (fst (head list))
  putStr "\t"
  putStrLn (show (snd (head list)))
  dumpData (tail list)


processPair :: (String, Integer) -> (String, Integer) -> IO [(String, Integer)]
processPair x y = do
  --bla <- runProcess "xbiff" [] Nothing Nothing Nothing Nothing Nothing
  first <- runProcess ("display") [(fst x)] Nothing Nothing Nothing Nothing Nothing
  second <- runProcess ("display") [(fst y)] Nothing Nothing Nothing Nothing Nothing
  putStrLn ("1. " ++ (fst x))
  putStrLn ("2. " ++ (fst y))
  putStrLn "Which do you prefer? Enter 1 or 2: "
  answer <- getLine
  putStrLn "Closing windows ..."
  resultA <- terminateProcess first
  resultB <- terminateProcess second
  --terminateProcess bla
  putStrLn "Closed windows."
  if (answer == "1")
    then return [((fst x), (1 + (snd x))), y]
    else return [x, ((fst y), (1 + (snd y)))]

doProcessData :: [(String, Integer)] -> IO [(String, Integer)]
doProcessData [] = return []
doProcessData [x, y, z] = do
  processedB <- processPair x z
  processedA <- processPair (head processedB) y
  return (processedA ++ (tail processedB))
doProcessData list = 
  let
    x = head list
    y = head (tail list)
    remainder = tail (tail list)
  in
  do
    processed <- processPair x y
    processedRemainder <- doProcessData remainder
    return (processed ++ processedRemainder)

score :: (String, Integer) -> (String, Integer) -> Ordering
score x y = compare (snd x) (snd y)

processData :: [(String, Integer)] -> Integer -> IO [(String, Integer)]
processData x 0 = return x
processData x n = do
  intermediateResults <- (doProcessData x)
  processData (sortBy score intermediateResults) (n - 1)

main :: IO ()
main = do
  list <- listDirectory
  results <- (processData (buildData (lines list)) 1)
  dumpData results
