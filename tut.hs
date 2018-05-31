-- comment
{- multiline comment -}

import Data.List
import System.IO

-- Int - bounded int : -2^63 - 2^63
maxInt = maxBound :: Int
minInt = minBound :: Int

-- Integer - as big as memory holds
-- Double : precise up to 11 points
bigFloat = 3.999999999999 + 0.00000000005

-- Bool True or False
-- Char Single unicode characters ' '
-- Tuple Stores list made up of many different types

-- Declare a Value (a constant function)
always5 :: Int
always5 = 5

sumOfNums = sum [1..1000]

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

-- Prefix operators
modEx = mod 5 4

-- Infix operators use ``
modEx2 = 5 `mod` 4

-- Must surround negs with ()
negNumEx = 5 + (-4)

-- :t sqrt
-- sqrt :: Floating a => a -> a
num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

-- sin, cos, tan, asin, acosh
-- pi, exp, log, truncate, round, ceiling, floor

piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not True

-- :t (+)
-- (+) :: Num a => a -> a -> a

-- :t truncate
-- truncate :: (RealFrac a, Integral b) => a -> b

primeNumbers = [3, 5, 7, 11]
morePrimes = primeNumbers ++ [13, 17, 19, 23, 29]

-- equal to [2, 7, 21, 66]
favNums = 2 : 7 : 21 : 66 : []

multList = [[3, 5, 7], [11, 13, 17]]
morePrimes2 = 2 : morePrimes
lenPrime = length morePrimes2

revPrime = reverse morePrimes2
isListEmpty = null morePrimes2

-- !! is indexing operator
secondPrime = morePrimes2 !! 1

firstPrime = head morePrimes2
lastPrime = last morePrimes2

-- everything but the last item
primeInit = init morePrimes2
first3Primes = take 3 morePrimes2
removePrimes = drop 3 morePrimes2

-- Check for list membership
is7InList = 7 `elem` morePrimes2
maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2
newList = [2, 3, 5]
prodPrimes = product newList
zeroToTen = [0..10]
evenList = [2,4..20]
letterList = ['A','C'..'Z']

-- infinite lists are lazy calculated
infinPow10 = [10,20..]

-- repeat 2 = [2,2,2,2,2,2,2,2,2,2,2,..]
many2s = take 10 (repeat 2)

-- replicate [3,3,3,3,3,3,3,3,3,..]
many3s = replicate 10 3

cycle [1,2,3] = [1,2,3,1,2,3,1,2,3,1,2,3,..]
cycleList = take 10 (cycle [1,2,3,4,5])
listTimes3 = [x * 3 | x <- [1..10], x * 3  <= 50]
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]
sortedList = sort [9,1,8,3,4,7,6]

-- zipWith = [7,9,11,13,15]
sumOfLists = zipWith (+) [1..5] [6..10]

listBiggerThan5 = filter (> 5) morePrimes
evensUpTo20 = takeWhile (<= 20) [2,4..]

-- From left to right * them all together = 120
-- also foldr from right to left.
-- like reduce
multOfList = foldl (*) 1 [2,3,4,5]

{- List comprehensions -}

pow3List = [3 ^ n | n <- [1..10]]
multTable = [[x * y | y <- [1..10] | x <- [1..10]]

-- Tuples : Lists have to be same data type tuples not so much
randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52)

-- => "Bob Smith"
bobsName = fst bobSmith
-- => 52
bobsAge = snd bobSmith

names = ["bob", "mary", "tom"]
addresses = ["123 main", "234 North", "567 south"]
-- [("Bob", "123 Main") ...]
namesNAddress = zip names addresses

num7 = 7
getTriple x = x * 3
-- => 21
getTriple num7

-- ghc --make <filename>.hs
{- 
main = do
	putStrLn "What's your name"
	name <- getLine
	putStrLn ("Hello " ++ name)
-}

-- Type declarations:
addMe :: Int -> Int -> Int
addMe x y = x + y

-- :t addMe
-- addMe :: Int -> Int -> Int
-- addMe 5 3
-- => 8

sumMe x y = x + y
-- sumMe 4.5 6.7
-- => 11.2

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)
-- addTuples (1,2) (3,4)
-- => (4, 6)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
-- _ is used for values you don't care about
whatAge _ = "Nothing important"
-- whatAge 16
-- => "You can drive"
-- whatAge 40
-- => "Nothing important"

factorial :: Int -> int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- factorial 3
-- => 6
-- 3
-- 3 * factorial 3 - 1
-- 2 * factorial 2 - 1
-- 1 * factorial 1 - 1
-- factorial 0 = 1

-- Another way of defining factorials
prodFact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
	| n `mod` 2 == 0 = False
	| otherwise = True

isEven n = n `mod` 2 == 0

whatGrade :: Int -> String
whatGrade age
	| (age >= 5) && (age <= 6) = "Kindergarten"
	| (age >= 6) && (age <= 10) = "Elementary"
	| (age >= 10) && (age <= 14) = "Middle school"
	| otherwise = "Go to college"

-- where clause : used to define common calculations in guards:
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg <= 0.200 = "Terrible"
	| avg <= 0.250 = "Average player"
	| avg <= 0.280 = "Your doing pretty good"
	| otherwise = "You are are super star"
	where avg = hits / atBats

getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
-- show is like toString in other languages
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " " + show y
getListItems (x:xs) = "The first item is " ++ show x ++ " and the rest are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

{- Higher Order Functions -}

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1..5]
-- => [4, 8, 12, 16, 20]

-- recursion in Haskell:
multby4 :: [Int] -> [Int]
multby4 [] = []
multby4 (x:xs) = times4 x : multby4 xs
{-
[1,2,3,4] : x = 1 | xs = [2,3,4]
[2,3,4] : x = 2 | xs = [3,4]
[3,4] : x = 3 | xs = [4]
[4] : x = 4 | xs = []
[] = []
-}

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

-- (Int -> Int) is a functino takes an Int and returns an Int.
doMult :: (Int -> Int) -> Int
doMult func = func 3
num3Times4 = doMult times4 

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus3 = adds3 4
threePlusList = map adds3 [1..5]

-- Lambdas
dbl1To10 = map (\x -> x * 2) [1..10]

-- NOTE: not equal to is /= in haskell!
doubleEvenNumber y = 
	if (y `mod` 2 /= 0)
	then y
	else y * 2

getClass :: Int -> String
getClass n = case n of
	5 -> "Go to kindergarten"
	6 -> "Go to elementary school"
	_ -> "Go away"

-- Modules

module SampFunctions (
	getClass, 
	doubleEvenNumbers
) where
	getClass = ...
	doubleEvenNumbers = ...


-- Enums
data BaseballPlayer = Pitcher 
	| Catcher 
	| Infielder
	| Outfielder
	deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfielder = True

-- Custom Types
data Customer = Customer String String Double
	deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ balance) = balance


data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats rock"
-- ...
shoot _ _ = "Error"

-- shoot Paper Rock
-- => "Paper beats rock"

data Shape = Circle Float Float Float
	| Rectangle Float Float Float Float
	deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 - y))
-- $ stands for ( ... )
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)

-- The dot . operator chains functions:
sumValue = putStringLn (show (1 + 2))
sumValue2 = putStringLn . show $ 1 + 2
areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 100

