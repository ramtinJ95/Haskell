import Data.List
import System.IO

strToInt :: String -> [Int]
strToInt s = map read $ words s


main = do
	list1 <- getLine
	