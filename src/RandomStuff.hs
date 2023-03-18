module RandomStuff where

import Data.Function (fix)
import Data.Function.Memoize (memoFix, memoFix2)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust, listToMaybe,mapMaybe)
import Control.Monad (unless)
import qualified Data.Map as Map
import Control.Applicative (Alternative(..))
import Data.Foldable (Foldable(foldl'))
import Data.List (minimum, delete, find,nub, sort, sortBy)
import Debug.Trace (trace)
import Data.Char (toLower)
import Data.Semigroup (Min(Min),Max(Max))
import Data.List.GroupBy(group)
import qualified Data.HashMap as HashMap
import qualified Data.Heap as Heap
import Data.Data

-- binary search

-- O(log n)
binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch _ [] = Nothing
binarySearch x xs | x == r    = Just index
                  | r <  x    = (+index) . (+1) <$> binarySearch x rs
                  | otherwise = binarySearch x l
        where index = length xs `quot` 2
              (l,r:rs) = splitAt index xs

-- sorting

-- O(n*Log n)
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = let (l,r) = splitAt (div (length xs) 2) xs
                 in merge (mergeSort l) (mergeSort r)

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs         = xs
merge xs []         = xs
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge ys (x:xs)

mergeN :: Ord a => [[a]] -> [a]
mergeN  = fix $ \rec xs -> case xs of
  []       -> []
  [x]      -> x
  x1:x2:xs ->  rec $ x1 `merge` x2 : xs

-- worst time is O(n^2) but average
-- (random pivot will list split rougly in half) is O(n*Log n)
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort lesser ++ [x] ++ quickSort greater
    where lesser = filter (< x) xs
          greater = filter (>= x) xs

-- O(n^2)
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insertionSort' :: Ord a => [a] -> [a]
insertionSort' = foldr insert []

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:insert x ys

-- O(n2)
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = minimum xs
                    in x : selectionSort (delete x xs)

-- dynamic programming

-- fibonacci

-- the classic Haskell fibonacci implementation
-- O(n)
coolFib :: [Integer]
coolFib = 0 : 1 : zipWith (+) coolFib (tail coolFib)

-- O(2^n)
naiveFib :: Int -> Integer
naiveFib 0 = 1
naiveFib 1 = 1
naiveFib n = naiveFib (n - 1) + naiveFib (n - 2)

fib :: (Int -> Integer) -> (Int -> Integer)
fib _ 0 = 1
fib _ 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

-- fibMemo with factored out recursion
-- O(n)
fibMemoFix :: Int -> Integer
fibMemoFix = fix (memoize . fib)

{- lets 'install the definition of fib and memoize to see whats really happening...'

fibMemoFix',fibMemoFix'',fibMemoFix''',fibMemoFix'''',fibMemoFix''''' :: Int -> Integer
fibMemoFix'     = let x = (memoize . fib) x                 in x
fibMemoFix''    = let x = memoize (fib x)                   in x
fibMemoFix'''   = let x = (\f -> (map f [0 ..] !!)) (fib x) in x
fibMemoFix''''  = let x = (map (fib x) [0 ..] !!)           in x
fibMemoFix''''' = let x = ([fib x n | n <- [0..]] !!)       in x
-}

-- fibMemo with explicit recursion
-- O(n)
fibMemoExplicitRecursion :: Int -> Integer
fibMemoExplicitRecursion = (map fib' [0..] !!)
     where fib' 0 = 1
           fib' 1 = 1
           fib' n = fibMemoExplicitRecursion (n-2) + fibMemoExplicitRecursion (n-1)

           -- map fib' [0..] == let xs = [fib' 0, fib' 1, fib' 2, fib' 3, ... ]
           --                == let xs = [1, 1, xs !! 0 + xs !! 1  , ... ]
           --                == let xs = [1, 1, 1 + 1  , ... ]
           --                == let xs = [1, 1, 2  , ... ]
           -- This only works because haskell evaluation of [] is lazy

-- using memoFix from Data.Function.Memoize

-- O(n)
fibMemo :: Int -> Integer
fibMemo = memoFix fib

-- grid traveler problem

-- O(2^(n+m))
naiveGridTraveler :: Int -> Int -> Int
naiveGridTraveler 0 _ = 0
naiveGridTraveler _ 0 = 0
naiveGridTraveler 1 1 = 1
naiveGridTraveler n m = naiveGridTraveler (n-1) m + naiveGridTraveler n (m-1)

gridTraveler :: (Int -> Int -> Int) -> (Int -> Int -> Int)
gridTraveler f 0 _ = 0
gridTraveler f _ 0 = 0
gridTraveler f 1 1 = 1
gridTraveler f n m = f (n-1) m + f n (m-1)

-- recursion factored out (still slow)
-- O(2^(n+m))
gridTravelerFix :: Int -> Int -> Int
gridTravelerFix = fix gridTraveler

-- using memoFix from Data.Function.Memoize
-- O(n+m)
gridTravelerMemo :: Int -> Int -> Int
gridTravelerMemo = memoFix2 gridTraveler

-- TODO: FIX
gridTravelerMemoExplicitRecursion :: Num a => (Int, Int) -> a
gridTravelerMemoExplicitRecursion (i, j) = [g (n, m) | n <- [0 .. ], m <- [0 .. ]] !! (i*j + j)
        where g (0, _) = 0
              g (_, 0) = 0
              g (1, 1) = 1
              g (n, m) = gridTravelerMemoExplicitRecursion (n-1, m) +
                         gridTravelerMemoExplicitRecursion (n, m-1)

-- can sum

-- can't reuse numbers
-- O(n choose k)
naiveCanSum :: Int -> [Int] -> Bool
naiveCanSum target = any ((== target) . sum) . mconcat . (\xs -> [combinations n xs | n <- [0..length xs]])

-- O(n choose k) == O(C(n,k)) == O(min(n^k, n^(n-k)))
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = return mempty
combinations _ []     = mempty
combinations m (x:xs) = map (x:) (combinations (m-1) xs) ++ combinations m xs

-- can't reuse numbers (recursive implementation)
naiveCanSum' :: Int -> [Int] -> Bool
naiveCanSum' targetSum numbers
        | targetSum == 0 = True
        | targetSum <  0 = False
        | otherwise      = fix f (targetSum, numbers)
        where f _ (_, [])     = False
              f g (ts, x:xs)  = let targetSum' = targetSum - x
                in naiveCanSum' targetSum' xs || g (targetSum', xs)

-- how sum

naiveHowSum :: Int -> [Int] -> [[Int]]
naiveHowSum target = filter ((== target) . sum) . mconcat . (\xs -> [combinations n xs | n <- [0..length xs]])

-- longest common subsequence

{-
xs = "ABCDE"
ys = "A C E"

lcs = 0 (longest common subsequence starts at 0)

1) start comparing the first elements of xs and ys:
is head xs = head ys? Three things can happen:
        * yes -> set lcs = lcs + 1 and call 1 + lcs (tail xs, tail ys)
        * no  -> take the max between lcs (tail xs, ys) and lcs (xs, tail ys)

        in this example: 'A' = 'A' -> lcs = lcs + 1 = 0 + 1 = 1

we continue this until we reach a base case: either xs or ys is empty.
-}

-- explicit recursion
naiveLCS :: String -> String -> Int
naiveLCS  = go 0
        where go n [] _          = n
              go n _ []          = n
              go n (x:xs) (y:ys) | x == y    = go (n+1) xs ys
                                 | otherwise = max (go n (x:xs) ys) (go n xs  (y:ys))

-- edit distance

minEditDistance :: String ->  String  -> Int
minEditDistance "" "" = 0
minEditDistance "" ys = length ys
minEditDistance xs "" = length xs
minEditDistance (x:xs) (y:ys)
        | x == y    = minEditDistance xs ys
        | otherwise = minimum [minEditDistance (y:x:xs) (y:ys) + 1, -- insert
                               minEditDistance      xs  (y:ys) + 1, -- delete
                               minEditDistance (y:  xs) (y:ys) + 1] -- replace

minEditDistance' :: (String ->  String -> Int) -> (String ->  String -> Int)
minEditDistance' f "" "" = 0
minEditDistance' f "" ys = length ys
minEditDistance' f xs "" = length xs
minEditDistance' f (x:xs) (y:ys)
        | x == y    = f xs ys
        | otherwise = minimum [f (y:x:xs) (y:ys) + 1, -- insert
                               f      xs  (y:ys) + 1, -- delete
                               f (y:  xs) (y:ys) + 1] -- replace

minEditDistanceMemo :: String ->  String -> Int
minEditDistanceMemo = memoFix2 minEditDistance'

test :: [[Int]]
test = [[1,2], -- 0
        [3,4], -- 1
        [5],   -- 2
        [],    -- 3
        [5],   -- 4
        []]    -- 5

{- breadth first search

def bfs (graph,node):
  visited = []
  queue   = []

  visited.append(node)
  queue.append(node)

  while queue:
    s = queue.pop(0)
    print(s,end = " ")

    for n not in visited:
      visited.append(n)
      queue.append(n)
-}

-- depth first search

-- O(V+E) (graph) or O(V) (tree)
dfs :: [[Int]] -> Int -> [Int] -> [Int]
dfs g n visited = foldl accum (visited ++ [n]) (g !! n)
  where accum visited next
          | next `elem` visited = visited
          | otherwise           = dfs g next visited

dfs'' :: [[Int]] -> IO ()
dfs'' g = do
  putStrLn "Nodes visited in following order:"
  mapM_ print $ dfs g 0 []

-- an alternative formulation for DFS and BFS

data SearchProblem a  = SearchProblem { start :: a
                                      , expand :: a -> [a]
                                      , isDone :: a -> Bool }

type Search a = SearchProblem a -> Maybe a

-- O(V+E) (graph) or O(V) (tree)
dfs' :: Search a
dfs' (SearchProblem start expand isDone) = loop start
    where loop x | isDone x  = Just x
                 | otherwise = listToMaybe $ mapMaybe loop (expand x)

-- O(V+E) (graph) or O(V) (tree)
bfs' :: Eq a => Search a
bfs' (SearchProblem start expand isDone) = loop [start]
    where loop xs | any isDone xs = find isDone xs
                  | otherwise     = loop (nub $ concatMap expand xs)

-- Some examples



findNumberSearchProblem :: SearchProblem Int
findNumberSearchProblem = SearchProblem
    { start = 23
    , expand = \n -> trace (show n) concat [ [ n+2, n+5] | n < 42 ]
    , isDone = (==42) }

gridSearchProblem :: SearchProblem (Int,Int)
gridSearchProblem = SearchProblem start expand isDone where
    start = (0,0)
    expand = \(x,y) -> trace (show (x,y)) $ [ (x+1,y) | x < 4 ] ++ [ (x,y+1) | y < 4 ]
    isDone = (== (4,4))

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq)

exampleTree :: BinaryTree String
exampleTree = Node "Hay"
               (Node "Hay"
                 (Node "Hay" Empty Empty)
                 (Node "Needle A" Empty Empty))
               (Node "Needle B"
                 (Node "Hay" Empty Empty)
                 Empty)

findInTreeSearchProblem :: BinaryTree a -> (a -> Bool) -> SearchProblem (BinaryTree a)
findInTreeSearchProblem start isDone = SearchProblem start subtrees treeIsDone
    where treeIsDone Empty        = False
          treeIsDone (Node x _ _) = isDone x
          subtrees Empty          = []
          subtrees (Node _ a b)   = [a, b]

findNeedleSearchProblem :: SearchProblem (BinaryTree String)
findNeedleSearchProblem = findInTreeSearchProblem exampleTree (("Needle" ==) . take 6)

-- Fixed point computation

constFixed :: String
constFixed = fix (const "hello")
{-
fix (const "hello") == let x = (const "hello") x in x
                    == let x = "hello"           in x
                    == "hello"
-}

fact :: Int -> Int
fact = fix $ \rec n -> if n == 0 then 1 else n * rec (n-1)

loopIO :: IO ()
loopIO = do
        putStrLn "Start"
        fix $ \loop -> do
                str <- getLine
                unless (str == "quit") $ do
                        putStrLn (reverse str)
                        loop
        putStrLn "Bye"

-- ((b -> c) -> b -> c) -> b -> c

forLoop :: IO ()
forLoop = do
    putStrLn "Start"
    flip fix 3 $ \loop n ->
        unless (n == 0) $ do
        str <- getLine
        unless (str == "quit") $ do
            putStrLn (reverse str)
            loop (n-1)
    putStrLn "Bye"

-- check prime

-- naive implemenation
-- O(n)
isPrime :: Integral a => a -> Bool
isPrime k = length [ x | x <- [2..k], k `mod` x == 0] == 1

-- First make function to get all factors of n:
-- O(n)
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], mod n x == 0]

-- Then check if factors are only the given number and 1,
-- if so, the number is prime:
-- O(n)
isPrime' :: Integral a => a -> Bool
isPrime' n = factors n == [1,n]

-- fizzbuzz

fizz :: Int -> String
fizz n | n `mod` 15 == 0  = "FizzBuzz"
       | n `mod` 3  == 0  = "Fizz"
       | n `mod` 5  == 0  = "Buzz"
       | otherwise        = show n

-- O(n)
fizzbuzz :: IO()
fizzbuzz = mapM_ (putStrLn . fizz) [1..100]

-- binary to decimal

-- O(n)
bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + mod i 10

-- O(n)
bintodec' :: [Bool] -> Int
bintodec' = foldr (\x y -> fromEnum x + 2*y) 0

-- kth largest element
-- O(n*logn)
findKthLargest :: Int -> [Int] -> Maybe Int
findKthLargest k xs
        | k > 0 && k < length xs = Just $ (reverse.sort) xs !! (k-1)
        | otherwise              = Nothing

-- 4 sum
fourSum :: Int -> [Int] -> [[Int]]
fourSum target numbers = filter (\xs -> length xs == 4) (naiveHowSum target numbers)

-- palindrome
-- O(n*logn)
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

-- sum to n
sumNToTotal :: Int -> Int -> [Int] -> [[Int]]
sumNToTotal n totalNeeded xs = filter matchesSum (combinations n xs)
        where matchesSum ys = sum ys == totalNeeded

-- sum to any target

sumAnyToTarget :: Int -> [Int] -> [[Int]]
sumAnyToTarget totalNeeded xs = foldMap (\n -> sumNToTotal n totalNeeded xs) [0..length xs]

-- anagrams

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

isAnagramIgnoreCase :: String -> String -> Bool
isAnagramIgnoreCase xs ys = (sort . map toLower) xs == (sort . map toLower) ys

-- min and max
-- Given a list of elements, find the smallest and largest element of that list!

minMax :: Ord a => [a] -> Maybe (a, a)
minMax xs = case foldMap (\a -> Just (Min a, Max a)) xs of
                Just (Min x, Max y) -> Just (x, y)
                _ -> Nothing

-- Word Frequency
-- O(n*logn)
mostCommonWord :: String -> Maybe String
mostCommonWord "" = Nothing
mostCommonWord xs = return . (!! 0) . (!! 0) . sortBy freq . group . sort . words $ xs
        where freq xs ys | length xs >  length ys = LT
                         | length xs == length ys = EQ
                         | otherwise              = GT


-- equational reasoning

-- slow reverse
slowReverse :: [a] -> [a]
slowReverse []     = []
slowReverse (x:xs) = slowReverse xs ++ [x]

fastReverse :: [a] -> [a] -> [a]
fastReverse [] ys     = ys
fastReverse (x:xs) ys = fastReverse xs (x:ys)

fastReverse' :: [a] -> [a] -> [a]
fastReverse' xs ys = foldl' (flip (:)) ys xs

-- exceptions
{-
data Exceptional e a =
     Success a
   | Exception e
   deriving (Show)

instance Monad (Exceptional e) where
   return              =  Success
   Exception l >>= _   =  Exception l
   Success  r  >>= k   =  k r

throw :: e -> Exceptional e a
throw = Exception

catch :: Exceptional e a -> (e -> Exceptional e a) -> Exceptional e a
catch (Exception  l) h = h l
catch (Success r)    _ = Success r
-}

{- Generics
Datatype-generic programming, also frequently just called generic
programming or generics in Haskell, is a form of abstraction that
allows defining functions that can operate on a large class of datatypes.
-}

-- Data.List implementation of permutations
permutations :: [a] -> [[a]]
permutations xs = xs : perms xs []
  where
  perms [] _      = []
  perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
    where
    interleave    xs     r = let (_,zs) = interleave' id xs r in zs
    interleave' _ []     r = (ts, r)
    interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                             in  (y:us, f (t:y:us) : zs)

{- Exestential types
Normally when creating a new type using type, newtype, data, etc.,
every type variable that appears on the right-hand side must also
appear on the left-hand side. Existential types are a way of turning this off.
-}

-- Universal & Existential quantification
-- The following are equivalent in terms of first-order predicate logic.

-- Existential quantification
-- (exists x. p x) -> q

-- Universal quantification
-- forall x. (p x -> q)

-- Higher-rank types

-- Rank-0 types are just monomorphic types (also called monotypes), 
-- i.e. they don’t have any type variables:

-- f :: Int -> Int

-- Rank-1 types have a forall that does not appear to the left of any arrow. 
-- The type variables bound by that forall are universally quantified.

-- f :: forall a. (a -> a)

-- Rank-2 types take a type of rank 1 (but no higher) as an argument. 
-- In other words, they may have a forall that appears to the left of one arrow.
-- The type variables bound by that forall are existentially quantified.

-- f :: (forall a. (a -> a))   -> Int

-- Rank-3 types take a type of rank 2 (but no higher) as an argument. 
-- The forall appears to the left of two arrows.
-- Here, a is universally quantified.

-- f :: ((forall a. (a -> a)) -> Int)   -> Int

-- In general:
-- * A rank-n type is a function whose highest rank argument is n-1.
-- * A type variable is universally quantified if it’s bound by a forall
--   appearing to the left of an even number of arrows.
-- * A type variable is existentially quantified if it’s bound by a forall
--   appearing to the left of an odd number of arrows.

-- Here’s another example for good measure:

-- f :: forall a. a -> (forall b. Show b => b -> IO ()) -> IO ()

-- Here, f is a rank-2 type with two type variables: a universal type
-- variable a and an existential type variable b.

-- What does it mean for a type variable to be existentially quantified?

-- In short: whereas universally quantified type variables are instantiated at
-- the “use site” (i.e. the user of the function has to choose which type they
-- want that type variable to be), existentially quantified type variables are
-- instantiated at the “definition site” (where the function is defined).
-- In other words, the function’s definition is free to choose how to instantiate
-- the type variable; but the user of the function is not.