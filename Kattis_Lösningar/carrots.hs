import Data.List
import System.IO

strToInt :: String -> [Int]
strToInt s = map read $ words s

getSnd :: [Int] -> Int
getSnd (_:x:_) = x

main = do
	list1 <- getLine
	let intlist1 = strToInt list1
	print $ getSnd intlist1