import Data.List
import System.IO

strToInt :: String -> [Int]
strToInt s = map read $ words s

getAnswer :: [Int] -> Int -> Int
getAnswer [] acc = acc
getAnswer (x:xs) acc = if (x < 0) then getAnswer xs (acc +1) else getAnswer xs acc

main = do
	line1 <- getLine
	line2 <- getLine
	let intlist1 = strToInt line2
	let answer = getAnswer intlist1 0
	print answer