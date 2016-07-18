import Data.List
import System.IO

sumOfNums = sum[1..100]
--num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral 9)

trueAndFalse = True && False

primeNumbers = [3,5,7,11]

more = 2 : primeNumbers
init1 = init more
evenList = [2,4..20]

listTimes2 = [x*2 | x<-[1..30],x*2<=50]

interesting = [x | x<-[1..500],mod x 9 == 0]

addMe :: Int -> Int -> Int
-- funcName param1 param2 = operations (returned value)

addMe x y = x + y 

addTuples :: (Int,Int) -> (Int,Int) -> (Int,Int)

addTuples (x,y) (a,b) = (x + a,y + b)

whatAge :: Int -> String

whatAge _ = "haha,you are old"

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial (n-1)

isOdd :: Int -> Bool

isOdd n
   | n `mod` 2 == 0 = False
   | otherwise = True


--isFat :: Int -> Int -> String

--isFat weight hight 
--   | ibm > 10 = "fat"
--   | otherwise = "OK"
--   where ibm = weight / hight

getListItems :: [Int] -> String

getListItems [] = "empty"
getListItems (x:xs) = show xs

multBy4 :: [Int] -> [Int]

multBy4 [] = []
multBy4 (x:xs) = x * 4 : multBy4 xs

areStringEq :: [Char] -> [Char] -> Bool

areStringEq [] [] = True
areStringEq (x:xs) (y:ys) = x == y && areStringEq xs ys
areStringEq _ _ = False


doubleEvenNumber x = 
	if (x `mod` 2 /= 0)
		then x
		else x * 2
data Student = Student String String String

fufu :: Student 

fufu = Student "haha" "big" "small"

getInfo :: Student -> String
getInfo (Student _ _ info) = info

writeToFile = do
   theFile <- openFile "test.txt" WriteMode
   hPutStrLn theFile ("hahah")
   hClose theFile
readFromFile = do
	theFile2 <- openFile "test.txt" ReadMode
	contents <- hGetContents theFile2
	putStr contents
	hClose theFile2


listFunc :: [Int] -> [Int]
listFunc (x:xs) =  xs