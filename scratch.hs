import Data.Char
import Prelude hiding(and, replicate, (!!),elem)

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

--positions x xs = find x $ zip xs [0..n]
--    where n = length xs - 1


scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct  xs ys = sum [x * y | x <- xs, y <- ys]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c 
    | isLower c = int2let $ (let2int c + n) `mod` 26
    | isUpper c = toUpper $ int2let $ (let2int (toLower c) + n) `mod` 26 
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x , y] | (x,y) <- xs `zip` ys]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]

c m 0 = 1
m `c` n = (m * m) `c` (n - 1)

and [] = True
--and (b:bs) = b && and bs
and (b:bs)
    | b = b
    | otherwise = and bs
--and (b:bs) = and bs && b

replicate 0 _ = []
replicate n x = x : replicate (n-1) x 

(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)


elem _ [] = False
elem x (y:ys)
    | x == y = True
    | otherwise = elem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = halve xs
