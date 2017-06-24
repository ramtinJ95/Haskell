import Data.List
import System.IO

strToInt :: String -> [Int]
strToInt s = map read $ words s -- read converts individual string to ints

getTime :: Num a => [a] -> a
getTime (_:x:_) = x

getAcc :: Int -> Int
getAcc a = a

getNumTask :: [Int] -> Int -> Int -> Int -> Int
getNumTask [] _ _ _ = 0
getNumTask arr maxTime acc acc2
	| (head arr + acc <= maxTime) = getNumTask (tail arr) maxTime (head arr + acc) (acc2 +1)
	| (head arr + acc > maxTime) = getAcc acc2
	| otherwise = getAcc 0

main = do
	list1 <- getLine
	list2 <- getLine
	let intlist1 = strToInt list1
	let intlist2 = strToInt list2
	let maxTime = getTime intlist1
	let answer = getNumTask intlist2 maxTime 0 0 
	if (answer == 0) && (intlist2 /= []) then print (head intlist1) else print answer
	

	