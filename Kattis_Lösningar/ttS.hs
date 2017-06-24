import Data.List
import System.IO

strToInt :: String -> [Int]
strToInt s = map read $ words s 

getAnswer :: [Int] -> String -> String
getAnswer [] temp = temp
getAnswer (x:xs) temp = if (mod x 2 == 0) then getAnswer xs "Bob" else getAnswer xs "Alice"


main = do 
	list <- getLine
	let num = strToInt list
	let b = getAnswer num "a"
	putStrLn b 