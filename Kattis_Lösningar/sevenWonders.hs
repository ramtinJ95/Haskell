import Data.List
import System.IO

extractT :: String -> Int -> Int
extractT [] acc = acc
extractT s acc
	| (head s == 'T') = extractT (tail s) (acc + 1)
	| otherwise = extractT (tail s) acc

extractC :: String -> Int -> Int
extractC [] acc = acc
extractC s acc
	| (head s == 'C') = extractC (tail s) (acc + 1)
	| otherwise = extractC (tail s) acc

extractG :: String -> Int -> Int
extractG [] acc = acc
extractG s acc
	| (head s == 'G') = extractG (tail s) (acc + 1)
	| otherwise = extractG (tail s) acc

addSeven :: String -> Int -> Int -> Int -> Int -> Int
addSeven [] 0 _ _  out = out
addSeven [] _ 0 _  out = out
addSeven [] _ _ 0  out = out
addSeven [] accT accC accG out = if ((accT /= 0) || (accC /=0) || (accG /=0)) then addSeven [] (accT - 1) (accC - 1) (accG - 1) (out + 1) else out 	
addSeven s accT accC accG out
	| ((accT /= 0) && (accC /= 0) && (accG /= 0)) = addSeven s (accT - 1) (accC - 1) (accG - 1) (out +1)
	| (head s == 'T') = addSeven (tail s) (accT + 1) accC accG out
	| (head s == 'C') = addSeven (tail s) accT (accC + 1) accG out
	| (head s == 'G') = addSeven (tail s) accT accC (accG + 1) out
	| otherwise = error "lili"

main = do
	list1 <- getLine
	let strArr = list1
	let t = extractT strArr 0
	let c = extractC strArr 0
	let g = extractG strArr 0
	let seven = addSeven strArr 0 0 0 0 
	print ((t*t) + (c*c) + (g*g) + (7*seven))