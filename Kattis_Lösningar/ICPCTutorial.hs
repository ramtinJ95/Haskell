import Data.List
import System.IO
import GHC.Integer.Logarithms



strToInt :: String -> [Integer]
strToInt s = map read $ words s

factorial :: Integer -> Integer -> Integer
factorial maxtime elements = if elements > 15 then 0 else if (product [1..elements]) <= maxtime then 1 else 0
facHelper :: Integer -> Integer -> Integer
facHelper 1 acc = acc
facHelper n acc = facHelper (acc*n) (n-1)

twoToN :: Integer -> Integer -> Integer 
twoToN maxtime elements = if 2^elements <= maxtime then 1 else 0

nToFourth :: Integer -> Integer -> Integer
nToFourth maxtime elements = if elements^4 <= maxtime then 1 else 0

nToThird :: Integer -> Integer -> Integer
nToThird maxtime elements = if elements^3 <= maxtime then 1 else 0

nSquared :: Integer -> Integer -> Integer
nSquared maxtime elements = if elements^2 <= maxtime then 1 else 0

nLog :: Integer -> Integer -> Integer
nLog maxtime elements = let e = fromIntegral elements in if truncate (e * (logBase 2 e )) < maxtime then 1 else 0

constant :: Integer -> Integer -> Integer
constant maxtime elements = if elements <= maxtime then 1 else 0

getAnswer :: [Integer] -> Integer
getAnswer (x:y:p:xs) 
	| p == 1 = factorial x y 
	| p == 2 = twoToN x y
	| p == 3 = nToFourth x y
	| p == 4 = nToThird x y
	| p == 5 = nSquared x y
	| p == 6 = nLog x (fromIntegral y)
	| p == 7 = constant x y
	| otherwise = error "You done fked up"


main = do
	list <- getLine
	let arr = strToInt list
	let answer = getAnswer arr 
	if answer == 1 then putStrLn "AC" else putStrLn "TLE"




