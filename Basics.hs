module Basics where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


---------------------
-- Introduce Tools --
---------------------

-- * GHCi commands
--     :help, :load, :reload, :quit, :type, :info
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- In GHCi:
--  * basic data types (Bool, Int, Float)
--  * numeric and boolean operators
--  * if-then-else expressions
--  * let-expressions


---------------------
-- Basic Functions --
---------------------

-- * function types
-- * defining and applying functions
-- * pattern matching
-- * partial application


-- | Add an integer to itself.
-- double = undefined
double :: Int -> Int
double x = x + x

-- | Is this integer zero?
-- isZero = undefined

isZero :: Int -> Bool
-- isZero x = if x == 0 then True else False
-- isZero x = x == 0

--Pattern Matching Way
    -- It is going to try each of these cases in order (Order does matter) 
isZero 0 = True 
-- isZero x = False
isZero _ = False --Same as previous line, but since we don't use x we can use _

-- | Is this integer non-zero?
-- isNonZero = undefined
isNonZero :: Int -> Bool
-- Option 1
-- isNonZero 0 = False
-- isNonZero _ = True

-- Option 2
-- isNoneZero x = x /= 0

-- Option 3  Using a previously defined function
-- isNonZero x = not (isZero x)

-- Option 4 (Function compositions)
isNonZero = not . isZero




-- | Computes the average of two floating point numbers.
-- avg = undefined
avg :: Float -> Float -> Float
avg x y = (x + y) / 2.0

-- | Uses avg to compute half of a floating point number.
-- half = undefined
half :: Float -> Float
-- half x = avg x 0
half = avg 0.0 -- Using partial of a function (partial application)




-- In GHCi:
--  * infix vs. prefix application: operators are just functions!
--    * (+) x y = x + y
--    * avg x y = x `avg` y
-- * anonymous functions
-- \x -> x + x

----------------------
-- Basic Data Types --
----------------------

-- * a data type definition consists of:
--   * a new type name
--   * a set of cases, each with:
--     * a data constructor
--     * zero or more arguments
-- * more pattern matching
--   * top-level and case-expressions

-- | An example data type with two cases.
  -- You must use capital letters for cases
data Result = OK Int | Error
  deriving (Eq,Show)
-- :t OK (returns a function Int -> Result)
-- :t Error (returns type Result)

-- OK is a constructor that converts an Int to type result

-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
-- safeDiv = undefined
safeDiv _ 0 = Error
safeDiv x y = OK (x `div` y)

-- | Get the integer from an OK result, or return 0 on an Error.
fromResult :: Result -> Int
-- fromResult = undefined

-- Option 1
-- fromResult (OK x) = x
-- fromResult Error = 0

fromResult r = case r of
  Error -> 0
  OK x -> x

-- | Add two results.
addResults :: Result -> Result -> Result
-- addResults = undefined

addResults (OK x) (OK y) = OK (x + y)
addResults _ _ = Error --Since the remaining cases will return Error


-- The definition of Bool in the Haskell Prelude looks like this:
--   
--   data Bool = False | True


-- The type Result is similar to the Maybe type in the Prelude:
--
--   data Maybe a = Just a | Nothing



---------------
-- Recursion --
---------------

-- * recursive data type definitions
-- * recursive functions

-- | An example of a recursive data type.
data List
   = Nil
   | Cons Int List
  deriving (Eq,Show)

-- | The empty list.
-- empty = undefined
empty :: List
empty = Nil

-- | The list: [2,3,4]
-- exList = undefined
exList :: List
exList = Cons 2 (Cons 3 (Cons 4 Nil))

-- | Compute the length of a list.
-- listLength = undefined
listLength :: List -> Int
listLength Nil = 0
listLength (Cons _ t) = 1 + listLength(t)

-- | Compute the sum of the integers in a list.
-- listSum = undefined
listSum :: List -> Int
listSum Nil = 0
listSum (Cons h t) = h + listSum(t)


-- Example evaluation:
--
-- listSum (Cons 3 (Cons 4 Nil))



-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [a]
--    = []         -- Nil
--    | a : [a]    -- Cons

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length :: [a] -> Int
-- length = undefined
length [] = 0
length(h:t) = 1 + length t

-- | Compute the sum of an integer list.
  -- if wanted to generalize to any number:
-- sum :: Num a => [a] -> a
sum :: [Int] -> Int
sum [] = 0
sum (h:t) = h + sum t


-- | Compute the product of the elements in a list.
-- product :: [Int] -> Int
product :: Num a => [a] => a
-- product = undefined
product [] = 1
product (h:t) = h * product t

-- Are all of the integers in this list odd?
allOdd :: [Int] -> Bool
allOdd [] = True
allOdd (h:t) = odd h && allOdd t

-- | Double all the elements in an integer list.
-- doubleAll :: [Int] -> [Int]
doubleAll :: Num a => [a] -> [a]
-- doubleAll = undefined
doubleAll [] = []
doubleAdd (h:t) = 2 * h : doubleAdd t

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
-- notAll = undefined
notAll [] = []
notAll (h:t) not h : notAll t

-- | Apply the even function to all elements in the list.
-- evenAll = undefined
evenAll :: [Int] -> [Bool]
evenAll [] = []
evenAll (h:t) = even h : evenAll t


----------------------------
-- Higher-Order Functions --
----------------------------

-- * map and foldr


-- | Map a function over the elements in a list.
-- map = undefined
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (h:t) = f h : map f t

-- | Reimplement doubleAll using map.
-- doubleAll' :: [Int] -> [Int]
-- doubleAll' = undefined
doubleAll' :: Num a => [a] -> [a]
-- doubleAll' xs = map (\x -> 2 * x) xs
doubleAll' = map (\x -> 2 * x) --Still works since it waits for another argument

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
-- notAll' = undefined
-- notAll' = map not
notAll' = map (\b -> not b)

-- | Reimplement evenAll using map.
evenAll' :: [Int] -> [Bool]
-- evenAll' = undefined
evenAll' = map even

-- | Fold an accumulator function over the elements in a list.
-- foldr = undefined
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x
foldr f x (h:t) = f h (foldr f x t)

-- | Reimplement sum using foldr.
-- sum' :: [Int] -> Int
-- sum' = undefined
sum' :: Num a => [a] -> a
sum 

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = undefined

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = undefined

-- | Reimplement allOdd using foldr.
allOdd' :: [Int] -> Bool
allOdd' = undefined

-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
countTrues = undefined