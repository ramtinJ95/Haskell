import Data.List
import System.IO

placeP = "P"
placeE = "E"
placeR = "R"

getAnswer :: String -> Int -> Int -> Int
getAnswer [] acc _ = acc
getAnswer (x:xs) acc place
	| (notElem x placeP) && (place == 1) = getAnswer xs (acc +1) (place + 1)
	| (notElem x placeE) && (place == 2) = getAnswer xs (acc +1) (place + 1)
	| (notElem x placeR) && (place == 3) = getAnswer xs (acc +1) (place - 2)
	| place == 3 = getAnswer xs (acc) (place - 2)
	| otherwise = getAnswer xs acc (place +1)
	
main = do
	string1 <- getLine
	let s = string1
	let answer = getAnswer s 0 1
	print answer

