import Data.List
import System.IO

strToInt :: String -> [Int]
strToInt s = map read $ words s

getHeight :: [Int] -> Int
getHeight (x:xs) = x


getWidth :: [Int] -> Int
getWidth (x:y:xs) = y

getNumBrick :: [Int] -> Int
getNumBrick (x:y:z:xs) = z

--acc1 total area, acc2 antalet bricks som användts, acc3 kollar så varje rad blir exakt width 
getAnswer :: Int -> Int -> Int -> [Int] -> Int -> Int -> Int -> Int
getAnswer height width _ [] acc1 _ _ = if (acc1 == (height * width)) then 1 else 0
getAnswer height width bricks (x:xs) acc1 acc2 acc3
	| (acc1 == (height * width)) && (acc2 == height) = 1
	| acc3 == width = getAnswer height width bricks (x:xs) acc1 0 0
	| (acc3 < width)&&(acc2 <= height) = getAnswer height width bricks xs (acc1 +x) (acc2 +1) (acc3+x)
	| acc3 > width = 0
	| (acc2 > bricks) || (acc2 > height) = 0
	| otherwise = 0


main = do
	list1 <- getLine
	list2 <- getLine
	let intlist1 = strToInt list1
	let intlist2 = strToInt list2
	let answer = getAnswer (getHeight intlist1) (getWidth intlist1) (getNumBrick intlist1) intlist2 0 0 0
	if (answer == 1) then putStrLn "Yes" else putStrLn "NO"

