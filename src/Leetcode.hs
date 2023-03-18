{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Use section" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}

module Leetcode where

import qualified Data.List       as List
import qualified Data.Maybe      as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.List.Split as Split
import Debug.Trace                  (trace)
import Data.Function                (fix)
import qualified Control.Monad   as Monad
import Data.Function.Memoize (memoFix, memoFix2)
import RandomStuff (isPalindrome, BinaryTree)

-- This module contains leetcode problems from the most common interview question patterns.

-- Sliding window pattern

-- Longest Substring Without Repeating Characters.
-- 
-- Given a string s, find the length of the longest
-- substring without repeating characters.

lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring = length . go "" ""
  where
  isBigger :: String -> String -> Bool
  isBigger ys xs = length ys > length xs

  mkOccurrenceMap :: String -> Map.Map Char Int
  mkOccurrenceMap = Map.fromListWith (+) . map (\x -> (x,1))

  hasUniqueCharactes :: String -> Bool
  hasUniqueCharactes = (\m -> sum m == length m) . mkOccurrenceMap

  go :: String -> String -> String -> String
  go xs ys []
    | hasUniqueCharactes ys && isBigger ys xs = ys
    | otherwise                               = xs
  go xs ys q@(z:zs)
    | hasUniqueCharactes ys && isBigger ys xs = go ys (ys <> [z]) zs
    | hasUniqueCharactes ys                   = go xs (ys <> [z]) zs
    | otherwise                               = go xs (tail ys) q

-- Substring with Concatenation of All Words.
-- 
-- You are given a string s and an array of strings words.
-- All the strings of words are of the same length.

-- A concatenated substring in s is a substring that contains all the strings of any
-- permutation of words concatenated.

-- For example, if words =
-- ["ab","cd","ef"], then
--  "abcdef", "abefcd",
--  "cdabef", "cdefab",
-- "efabcd", and "efcdab" are all
-- concatenated strings. "acdbef" is
-- not a concatenated substring
-- because it is not the concatenation
-- of any permutation of words.

-- Return the starting indices of all the
-- concatenated substrings in s. You can
-- return the answer in any order.

findSubstring :: [String] -> String -> [Int]
findSubstring words = go [] ("",0)
  where
  aux :: [String]
  aux = map concat $ List.permutations words

  isSubstring :: String -> String -> Bool
  isSubstring s1 s2 = (and . map (\(a,b) -> a == b) $ zip s1 s2) &&
                      length s1 <= length s2

  isConcatenatedSubstring :: String -> Bool
  isConcatenatedSubstring s = any (isSubstring s) aux

  go :: [Int] -> (String,Int) -> String -> [Int]
  go xs (ys,i) ""
    | isConcatenatedSubstring ys                  = i:xs
    | otherwise                                   = xs
  go xs (ys,i) q@(z:zs)
    | null ys                                     = go xs (ys <> [z],i) zs
    | isConcatenatedSubstring ys &&
      (not . isConcatenatedSubstring) (ys <> [z]) = go (i:xs) ("",i+length ys) q
    | isConcatenatedSubstring (ys <> [z])         = go xs (ys <> [z],i) zs
    | otherwise                                   = go xs (tail $ ys <> [z],i+1) zs

-- Repeated DNA Sequences
--
-- The DNA sequence is composed of a series of nucleotides
-- abbreviated as 'A', 'C', 'G', and 'T'.
--   * For example, "ACGAATTCCG" is a DNA sequence.
--
-- When studying DNA, it is useful to identify repeated sequences
-- within the DNA.
--
-- Given a string s that represents a DNA sequence, return all the
-- 10-letter-long sequences (substrings) that occur more than
-- once in a DNA molecule. You may return the answer in any
-- order.

findRepeatedDnaSequences :: String -> [String]
findRepeatedDnaSequences = Map.keys . Map.filter (>= 2) . mkOccurrenceMap . sequencesOfLength10
  where
  mkOccurrenceMap :: [String] -> Map.Map String Int
  mkOccurrenceMap = Map.fromListWith (+) . map (\x -> (x,1))

  sequencesOfLength10 :: String -> [String]
  sequencesOfLength10 s@(_:_) = filter (\s' -> length s' == 10) $ take 10 s : sequencesOfLength10 (tail s)
  sequencesOfLength10 _       = []

-- Maximum sum of a contigous subarray of size 3.
-- 
-- Find the maximum sum of a contigous subarray of size 3.

maximumSumOfContigousSubarrayOfSize3 :: [Int] -> Int
maximumSumOfContigousSubarrayOfSize3 xs = go 0 xs
  where
  go n ys = case ys of
    (a:(p@(b:c:_))) -> if a+b+c > n then go (a+b+c) p else go n p
    otherwise       -> n

-- Maximum sum of a contigous subarray of size k.
-- 
-- Find the maximum sum of a contigous subarray of size k.

maximumSumOfContigousSubarrayOfSizeK :: [Int] -> Int -> Int
maximumSumOfContigousSubarrayOfSizeK xs k = go 0 xs
    where
    go n ys = case compare (length ys) k of
      LT -> n
      EQ -> n
      GT -> let n'  = sum $ take k ys
                ys' = tail ys
            in if n' > n then go n' ys' else go n ys'

-- Smallest subarray with a given sum k.
--
-- Find the smallest subarray where the sum of values is greater than or equal to k.

-- Naive solution: (functional approach with function composition)
-- The naive solution can be implemented with the composition of 3 functions:
--   1: subarrays   -> Generate all possible contigous subarrays.
--   3: keepValid   -> Filter arrays to keep only those that have a sum greater than or equal to k.
--   4: smallestSum -> Select the array with the smallest sum.

-- The time complexity is O(n²) + O(n) + O(n*log(n)) = O(n²)
--   * subarrays O(n²)
--   * keepValid O(n)
--   * smallestSum O(n*log(n))

naiveSmallestContigousSubarrayWithGivenSumK :: Int -> [Int] -> Maybe [Int]
naiveSmallestContigousSubarrayWithGivenSumK k = smallestSum . keepValid . subarrays
  where
  smallestSum :: [[Int]] -> Maybe [Int]
  smallestSum = Maybe.listToMaybe . (List.sortBy (\xs ys -> sum xs `compare` sum ys))

  keepValid :: [[Int]] -> [[Int]]
  keepValid = filter (\xs -> sum xs >= k)

  prefixes :: [a] -> [[a]]
  prefixes []     = [[]]
  prefixes xs     = [xs] ++ prefixes (init xs)

  suffixes :: [a] -> [[a]]
  suffixes []     = [[]]
  suffixes xs     = [xs] ++ suffixes (tail xs)

  subarrays :: [a] -> [[a]]
  subarrays = concat . map suffixes . prefixes

-- Optimal solution:
-- The optimal solution is a little more complicated.
-- We must keep track of 2 things whilst we 'slide' the dynamic window along the array.
--   1: The current smallest contigous subarray with given sum k.
--   2: The current contigous subarray whilst we slide the window.

-- We begin by gradually sliding the window to the right until our constraints are met (i.e the current contigous
-- subarray has a sum greater than or equal to k). Once this happens we will store this window as the current 
-- smallest contigous subarray with given sum k. At this point, it is clear that sliding the window to the right any further
-- will yield a 'less optimal' array, thus the only logical option we are left with is to shorten the array from the left and repeat the process.
-- This process continous until the array is equal to the empty array.

-- The time complexity is O(n) since we are sliding the dynamically resizing window through the entire array in one motion.

-- ! FIXME: There's a nasty bug lurking in the code...
optimalSmallestContigousSubarrayWithGivenSumK :: Int -> [Int] -> Maybe [Int]
optimalSmallestContigousSubarrayWithGivenSumK k = go Nothing []
  where
  hasGivenSum :: [Int] -> Bool
  hasGivenSum xs = sum xs >= k

  isSmaller :: [Int] -> [Int] -> Bool
  isSmaller xs ys = length xs < length ys

  go :: Maybe [Int] -> [Int] -> [Int] -> Maybe [Int]
  go xs ys []
    | hasGivenSum ys &&
      Maybe.maybe True (isSmaller ys) xs = Just ys
    | otherwise                          = xs
  go Nothing ys q@(z:zs)
    | hasGivenSum ys = go (Just ys) (tail ys) q
    | otherwise      = go Nothing (ys ++ [z]) zs
  go (Just xs) ys q@(z:zs) = case (hasGivenSum ys, isSmaller ys xs) of
    (True, True)  -> go (Just ys) (tail ys) q
    (False, True) -> go (Just xs) (ys ++ [z]) zs
    _             -> go (Just xs) (tail ys) zs

longestSubstringLengthWithKDistinctCharacters :: Int -> String -> String
longestSubstringLengthWithKDistinctCharacters k = go "" ""
  where
  mkOccurrenceMap :: String -> Map.Map Char Int
  mkOccurrenceMap = Map.fromListWith (+) . map (\x -> (x,1))

  kOrdering :: String -> Ordering
  kOrdering s = compare (length (Map.keys (mkOccurrenceMap s))) k

  isBigger :: String -> String -> Bool
  isBigger ys xs = sum (mkOccurrenceMap ys) > sum (mkOccurrenceMap xs)

  go :: String -> String -> String -> String
  go xs ys []
    | kOrdering ys == EQ && isBigger ys xs = ys
    | otherwise                            = xs
  go xs ys q@(z:zs)
    | kOrdering ys == EQ && isBigger ys xs = go ys (ys <> [z]) zs
    | kOrdering ys == LT                   = go xs (ys <> [z]) zs
    | otherwise                            = go xs (tail ys) q

-- Contains Duplicate II
--
-- Given an integer array nums and an integer k, return true if
-- there are two distinct indices i and j in the array such that
-- nums[i] == nums[j] and abs(i - j) <= k.

containsNearbyDuplicate :: [Int] -> Int -> Bool
containsNearbyDuplicate nums k = case Map.toList $ Map.fromListWith (\a1 a2 -> a1++a2) (zip nums (map (pure :: a -> [a]) [0..length nums])) of
  (_,xs@(_:(_:_))):_ -> case List.sort xs of (x:y:_) -> (abs (x-y)) <= k
  _                  -> False

-- Contains Duplicate III
--
-- You are given an integer array nums and two integers
-- indexDiff and valueDiff.
-- 
-- Find a pair of indices (i, j) such that:
--   * i != j,
--   * abs(i - j) <= indexDiff.
--   * abs(nums[i] - nums[j]) <= valueDiff, and
-- 
-- Return true if such pair exists or false otherwise.

containsNearbyAlmostDuplicate :: [Int] -> Int -> Int -> Bool
containsNearbyAlmostDuplicate nums indexDiff valueDiff = g . sortByValue $ valueIndexMap
  where
  f x y = compare (fst x) (fst y)
  sortByValue = List.sortBy f . Map.toList
  valueIndexMap = Map.fromListWith (\a1 a2 -> a1++a2) . zip nums . map (pure :: a -> [a]) $ [0..length nums]
  g q@((a,xs@(_:(_:_))):ks)                    = if case List.sort xs of
                                                      (x:y:_) -> (abs (x-y)) <= indexDiff
                                                 then True
                                                 else g ks
  g q@((a1,xs@(_:[])):ks@(((a2,ys@(_:[]))):_)) = case List.sort (xs ++ ys) of
                                                   (x:y:_) -> if (abs (x-y)) <= indexDiff && (abs (a1-a2)) <= valueDiff
                                                              then True
                                                              else g ks
  g q                                          = False

-- Sliding Window Maximum
--
-- You are given an array of integers nums, there is a sliding 
-- window of size k which is moving from the very left of the array
-- to the very right. You can only see the k numbers in the window.
-- Each time the sliding window moves right by one position.
-- 
-- Return the max sliding window.

maxSlidingWindow :: [Int] -> Int -> [Int]
maxSlidingWindow nums k = go [] (take k nums) (drop k nums)
  where
  go :: [Int] -> [Int] -> [Int] -> [Int]
  go xs ys []     = xs <> [maximum ys]
  go xs ys (z:zs) = go (xs <> [maximum ys]) (tail (ys <> [z])) zs

-- Longest Substring with At Least K Repeating Characters
--
-- Given a string s and an integer k, return the length of the longest substring of s
-- such that the frequency of each character in this substring is greater than or equal
-- to k.

longestSubstring :: String -> Int -> Maybe Int
longestSubstring "" 0 = Just 0
longestSubstring "" _ = Nothing
longestSubstring s  k = length <$> go Nothing "" s
  where
  mkOccurrenceMap :: String -> Map.Map Char Int
  mkOccurrenceMap = Map.fromListWith (+) . map (\x -> (x,1))

  isBetterThan :: String -> Maybe String -> Bool
  isBetterThan s1 mS2 = and (map ((>=k) . snd) (Map.toList (mkOccurrenceMap s1))) &&
                        maybe True (\s2 -> sum (mkOccurrenceMap s1) > sum (mkOccurrenceMap s2)) mS2

  go :: Maybe String -> String -> String -> Maybe String
  go xs ys "" = if ys `isBetterThan` xs then return ys else xs
  go xs ys (z:zs)
    | ys `isBetterThan` xs = go (Just ys) (ys <> [z]) zs
    | otherwise            = go xs (ys <> [z]) zs

-- Minimum Recolors to Get K Consecutive Black Blocks
-- 
-- You are given a 0-indexed string blocks of length n, where
-- blocks[i] is either 'W' or 'B', representing the color of the ith
-- block. The characters 'W' and 'B' denote the colors white and black,
-- respectively.
-- 
-- You are also given an integer k, which is the desired number of
-- consecutive black blocks.
-- 
-- In one operation, you can recolor a white block such that it becomes a
-- black block.
-- 
-- Return the minimum number of operations needed such that there is at
-- least one occurrence of k consecutive black blocks.

data Block  = W | B deriving (Eq, Ord)

minimumRecolors :: [Block] -> Int -> Int
minimumRecolors blocks k = go k (take k blocks) blocks
  where
  mkOccurrenceMap :: [Block] -> Map.Map Block Int
  mkOccurrenceMap = Map.fromListWith (+) . map (\x -> (x,1))

  recolors :: [Block] -> Int
  recolors = sum . Map.filterWithKey (\k _ -> k == W) . mkOccurrenceMap

  go :: Int -> [Block] -> [Block] -> Int
  go n ys []
    | recolors ys < n = recolors ys
    | otherwise       = n
  go n ys (z:zs)
    | recolors ys < n = go (recolors ys) (tail (ys ++ [z])) zs
    | otherwise       = go n (tail (ys ++ [z])) zs

-- Find All Anagrams in a String
-- 
-- Given two strings s and p, return an array of all the start indices of p's
-- anagrams in s. You may return the answer in any order.
-- 
-- An Anagram is a word or phrase formed by rearranging the letters of a
-- different word or phrase, typically using all the original letters exactly once.

findAnagrams :: String -> String -> [Int]
findAnagrams s p = map fst $ filter (\(i,b) -> b == True) $ zip [0..] $ map (isAnagram p) (allSubs (length p) s)
  where
  allSubs :: Int -> String -> [String]
  allSubs n s
    | length s >= n = take n s : allSubs n (tail s)
    | otherwise = []

  isAnagram :: String -> String -> Bool
  isAnagram s1 s2 = List.sort s1 == List.sort s2

-- Sliding Window Median
-- 
-- The median is the middle value in an ordered integer list. If the size of the
-- list is even, there is no middle value. So the median is the mean of the two
-- middle values.
-- 
--   * For examples, if arr = [2,3,4], the median is 3.
--   * For examples, if arr = [1,2,3,4], the median is (2 + 3) / 2 = 2.5.
-- 
-- You are given an integer array nums and an integer k. There is a sliding
-- window of size k which is moving from the very left of the array to the
-- very right. You can only see the k numbers in the window. Each time the
-- sliding window moves right by one position.

-- Return the median array for each window in the original array. Answers
-- within 10-5 of the actual value will be accepted.

-- Let's make this one short and complicated to read just for giggles
medianSlidingWindow k = foldr (\xs ys -> (f.g) xs:ys) [] . map List.sort . fix (\f n s -> if length s >= n then take n s : f n (tail s) else []) k
  where g l@(_:_:_:_) = g $ tail $ init l; g l = l
        f xs = if length xs == 1 then fromIntegral $ head xs else (fromIntegral $ sum xs) / 2

-- Smallest Range Covering Elements from K Lists
-- 
-- You have k lists of sorted integers in non-decreasing order. Find the smallest
-- range that includes at least one number from each of the k lists.
-- 
-- We define the range [a, b] is smaller than range [c, d] if b - a < d - c
-- or a < c if b - a == d - c.

smallestRange :: [[Int]] -> Maybe (Int,Int)
smallestRange xss
  | null xss || any null xss = Nothing
  | otherwise                = Just (map head xss) >>=
    \xs -> Just $ go (minimum xs, maximum xs) $ map (\xs -> (head xs, tail xs)) xss
  where
  mkRange :: [(Int,[Int])] -> (Int,Int)
  mkRange = (\xs -> (head xs, last xs)) . List.sort . map fst

  isSmallerThan :: (Int,Int) -> (Int,Int) -> Bool
  isSmallerThan (a,b) (c,d)
    | b - a == d - c = a < c
    | otherwise      = b - a < d - c

  go :: (Int,Int) -> [(Int,[Int])] -> (Int,Int)
  go (a,b) yss
    | (mkRange yss) `isSmallerThan` (a,b) = go (mkRange yss) yss
    | otherwise                           = case List.sortBy (\(a,_) (b,_) -> compare a b) yss of
      (x,(y:ys)):xs -> go (a,b) ((y,ys):xs)
      xss           -> if (mkRange xss) `isSmallerThan` (a,b)
        then (mkRange xss)
        else (a,b)

-- Maximum Average Subarray I
--
-- You are given an integer array nums consisting of n elements, and an
-- integer k.
--
-- Find a contiguous subarray whose length is equal to k that has the
-- maximum average value and return this value. Any answer with a
-- calculation error less than 10-5 will be accepted.

findMaxAverage :: [Int] -> Int -> Double
findMaxAverage nums k = go (average $ take k nums) (take k nums) nums
  where
  average :: [Int] -> Double
  average xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)

  go :: Double -> [Int] -> [Int] -> Double
  go n ys [] = if average ys > n then average ys else n
  go n ys q@(z:zs)
    | average ys > n = go (average ys) ys q
    | otherwise      = go n (tail $ ys <> [z]) zs

-- Permutation in String

checkInclusion s1 s2 = foldr (\s b -> if s2 `contains` s then b || True else b) False (perm s1)
  where
  ins :: a -> [a] -> [[a]]
  ins x []     = [[x]]
  ins x (y:ys) = [x:y:ys] ++ (map (y:) (ins x ys))

  perm :: [a] -> [[a]]
  perm []     = [[]]
  perm (x:xs) = (perm xs) >>= (ins x)

  go :: String -> String -> String -> Bool
  go s ys (z1:z2:zs)
    | ys == s   = True
    | otherwise = go s (tail $ ys <> [z1]) (z2:zs)
  go _ _ _      = False

  contains :: String -> String -> Bool
  contains xs ys = go ys (take (length ys) xs) xs

-- Max Value of Equation
-- 
-- You are given an array points containing the coordinates of points on a 2D
-- plane, sorted by the x-values, where points[i] = [xi, yi] such that xi <
-- xj for all 1 <= i < j <= points.length. You are also given an integer k.
-- 
-- Return the maximum value of the equation yi + yj + |xi - xj| where |xi -
-- xj| <= k and 1 <= i < j <= points.length.
-- 
-- It is guaranteed that there exists at least one pair of points that satisfy the
-- constraint |xi - xj| <= k.

findMaxValueOfEquation :: [(Int,Int)] -> Int -> Maybe Int
findMaxValueOfEquation [] _         = Nothing
findMaxValueOfEquation [_] _        = Nothing
findMaxValueOfEquation (p1:p2:ps) k = go Nothing p1 p2 ps
  where
  f :: (Int,Int) -> (Int,Int) -> Int
  f (xi,yi) (xj,yj) = yi + yj + abs (xi - xj)

  g :: (Int,Int) -> (Int,Int) -> Bool
  g (xi,yi) (xj,yj) = abs (xi - xj) <= k

  go :: Maybe Int -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Maybe Int
  go Nothing p1 p2 (p3:ps)
    | g p1 p2   = go (Just (f p1 p2)) p1 p3 ps
    | otherwise = go Nothing p2 p3 ps
  go (Just n) p1 p2 []
    | g p1 p2 && f p1 p2 > n = Just (f p1 p2)
    | otherwise              = Just n
  go (Just n) p1 p2 (p3:ps)
    | g p1 p2 && f p1 p2 > n = go (Just (f p1 p2)) p1 p3 ps
    | otherwise              = go (Just n) p2 p3 ps

-- Shortest Subarray with Sum at Least K
-- 
-- Given an integer array nums and an integer k, return the length of the
-- shortest non-empty subarray of nums with a sum of at least k. If there
-- is no such subarray, return -1.
-- 
-- A subarray is a contiguous part of an array.

shortestSubarray :: Int -> [Int] -> Maybe Int
shortestSubarray k = fmap length . Maybe.listToMaybe . filter g . allSubs'
  where
  allSubs' :: [a] -> [[a]]
  allSubs' = concat . \xs -> [allSubs i xs | i <- [1..length xs]]

  allSubs :: Int -> [a] -> [[a]]
  allSubs n xs
    | length xs >= n = take n xs : allSubs n (tail xs)
    | otherwise = []

  g :: [Int] -> Bool
  g xs = sum xs >= k

-- Maximum Sum of Two Non-Overlapping Subarrays
-- 
-- Given an integer array nums and two integers firstLen and
-- secondLen, return the maximum sum of elements in two non-
-- overlapping subarrays with lengths firstLen and secondLen.
-- 
-- The array with length firstLen could occur before or after the array with length
-- secondLen, but they have to be non-overlapping.
-- 
-- A subarray is a contiguous part of an array.

maxSumTwoNoOverlap :: [Int] -> Int -> Int -> Int
maxSumTwoNoOverlap nums firstLen secondLen = maximum $ do
  i <- [0..length nums]
  let (xs,ys) = splitAt i nums
  maybe [] return $ max ((+) <$> (maxSumK xs firstLen)  <*> (maxSumK ys secondLen))
                        ((+) <$> (maxSumK xs secondLen) <*> (maxSumK ys firstLen))

maxSumK :: [Int] -> Int -> Maybe Int
maxSumK xs n
  | length xs < n = Nothing
  | otherwise     = Just $ maximum [ sum xs | xs <- allSubs n xs ]
  where
  allSubs :: Int -> [a] -> [[a]]
  allSubs n xs
    | length xs >= n = take n xs : allSubs n (tail xs)
    | otherwise = []

-- Constrained Contigous Subsequence Sum
-- 
-- Given an integer array nums and an integer k, return the maximum sum
-- of a non-empty Contigous subsequence of that array such that for every two
-- consecutive integers in the subsequence, nums[i] and nums[j],
-- where i < j, the condition j - i <= k is satisfied.
-- 

constrainedContigousSubsetSum :: [Int] -> Int -> Int
constrainedContigousSubsetSum nums k = maximum $ map sum $ filter (\xs -> g [1..length xs]) $ concat [allSubs i nums | i <- [1..length nums]]
  where
  allSubs :: Int -> [a] -> [[a]]
  allSubs n xs
    | length xs >= n = take n xs : allSubs n (tail xs)
    | otherwise = []

  g :: [Int] -> Bool
  g (xi:xj:xs) = xj - xi <= k && g (xj:xs)
  g _          = True

-- Constrained Subsequence Sum
-- 
-- Given an integer array nums and an integer k, return the maximum sum
-- of a non-empty subsequence of that array such that for every two
-- consecutive integers in the subsequence, nums[i] and nums[j],
-- where i < j, the condition j - i <= k is satisfied.
-- 
-- A subsequence of an array is obtained by deleting some number of
-- elements (can be zero) from the array, leaving the remaining elements in
-- their original order.

constrainedSubsetSum :: Int -> [Int] -> Int
constrainedSubsetSum k nums = case filter (\xs -> g [1..length xs]) . List.delete [] . List.subsequences $ nums of
  [] -> maximum nums
  xs -> maximum $ map sum xs
  where
  g :: [Int] -> Bool
  g (xi:xj:xs) = xj - xi <= k && g (xj:xs)
  g _          = True

-- Find the K-Beauty of a Number
-- 
-- The k-beauty of an integer num is defined as the number of substrings
-- of num when it is read as a string that meet the following conditions:
-- 
-- It has a length of k.
-- It is a divisor of num.
-- 
-- Given integers num and k, return the k-beauty of num.
-- 
-- Note:
--   * Leading zeros are allowed.
--   * 0 is not a divisor of any value.

-- A substring is a contiguous sequence of characters in a string.

divisorSubstrings :: Int -> Int -> Int
divisorSubstrings num k = length . filter f . map read . allSubs k . show $ num
  where
  allSubs :: Int -> [a] -> [[a]]
  allSubs n xs
    | length xs >= n = take n xs : allSubs n (tail xs)
    | otherwise      = []

  f :: Int -> Bool
  f 0 = False
  f n
    | mod num n == 0 = True
    | otherwise      = False

-- Two Sum
-- 
-- Given an array of integers nums and an integer target, return indices of
-- the two numbers such that they add up to target.
-- 
-- You may assume that each input would have exactly one solution, and you
-- may not use the same element twice.
-- 
-- You can return the answer in any order.

twoSumBruteForce :: [Int] -> Int -> Maybe (Int,Int)
twoSumBruteForce nums target = (\((_,i),(_,j)) -> (i,j)) <$> (Monad.join $ Maybe.listToMaybe <$> (Maybe.listToMaybe $ map (filter (\((n,i),(m,j)) -> n + m == target)) [ map (\ab -> (n,ab)) xs | n <- (zip nums [0..]), xs <- replicate (length nums) (zip nums [0..])]))

twoSumBinarySearch :: [Int] -> Int -> Maybe (Int,Int)
twoSumBinarySearch nums target = Monad.join $ Maybe.listToMaybe $ filter (not . Maybe.isNothing) [go ni (zip nums [0..]) | ni <- (zip nums [0..])]
  where
  split :: [a] -> ([a],[a])
  split xs = List.splitAt (div (length xs) 2) xs

  go :: (Int,Int) -> [(Int,Int)] -> Maybe (Int,Int)
  go n'@(n,i) [] = Nothing
  go n'@(n,i) xs = let (a,b)       = split xs
                       (l,(m,j),r) = (a,head b,tail b)
    in case compare (n+m) target of
      LT -> go n' r
      EQ -> Just (i,j)
      GT -> go n' l

-- Two sum using 2 indices
twoSum :: [Int] -> Int -> Maybe (Int,Int)
twoSum nums target = go (0,length nums - 1)
  where
  go :: (Int,Int) -> Maybe (Int,Int)
  go (i,j)
    | i == j    = Nothing
    | otherwise = let n = nums !! i
                      m = nums !! j
      in case compare (n+m) target of
         LT -> go (i,j-1)
         EQ -> Just (i,j)
         GT -> go (i+1,j)

-- Squares of a Sorted Array
-- 
-- Given an integer array nums sorted in non-decreasing order, return an array of
-- the squares of each number sorted in non-decreasing order.

sortedSquares :: [Int] -> [Int]
sortedSquares []     = []
sortedSquares (x:xs) = go (x ^ 2) [] (reverse xs)
  where
  go :: Int -> [Int] -> [Int] -> [Int]
  go x [] []     = [x]
  go x [] (z:zs) = if x <= (z^2) then go x [z^2] zs else go (z^2) [x] zs
  go x ys []     = x:ys
  go x ys (z:zs) = if x <= (z^2) then go x ((z^2):ys) zs else go (z^2) (x:ys) zs

-- Subarray Product Less Than K
-- 
-- Given an array of integers nums and an integer k, return the number of contiguous
-- subarrays where the product of all the elements in the subarray is strictly less
-- than k.

numSubarrayProductLessThanKBruteForce :: [Int] -> Int -> Int
numSubarrayProductLessThanKBruteForce nums k = length $ filter f $ concatMap (flip allSubs nums) [1..length nums]
  where
  allSubs :: Int -> [a] -> [[a]]
  allSubs n xs
    | length xs >= n = take n xs : allSubs n (tail xs)
    | otherwise = []

  f xs = product xs < k


-- Sort Colors
--
-- Given an array nums with n objects colored red, white, or blue, sort them in-place
-- (not possible in Haskell) so that objects of the same color are adjacent, 
-- with the colors in the order red, white, and blue.
--
-- We will use the integers 0, 1, and 2 to represent the color red, white, and blue,
-- respectively.
--
-- You must solve this problem without using the library's sort function.

data Color  = Red | White | Blue deriving (Eq, Show)

instance Ord Color where
  compare Red Red     = EQ
  compare White White = EQ
  compare Blue Blue   = EQ
  compare Red _       = LT
  compare White Blue  = LT
  compare White Red   = GT
  compare Blue _      = GT

intListToColors :: [Int] -> [Color]
intListToColors = map f
  where
  f 0 = Red
  f 1 = White
  f 2 = Blue

colorsToIntList :: [Color] -> [Int]
colorsToIntList = map f
  where
  f Red   = 0
  f White = 1
  f Blue  = 2

sortColors :: [Color] -> [Color]
sortColors colors = go [] 0 (length colors - 1)
  where
  go :: [Color] -> Int -> Int -> [Color]
  go xs i j
    | i >= j    = xs
    | otherwise = case compare (colors !! i) (colors !! j) of
      GT -> go (xs <> pure (colors !! j)) i (j-1)
      _  -> go (xs <> pure (colors !! i)) (i+1) j

sortColors' :: [Int] -> [Int]
sortColors' = colorsToIntList . sortColors. intListToColors

-- Dynamic Programming

-- Climbing Stairs 
--
-- You are climbing a staircase. It takes n steps to reach the top.
-- Each time you can either climb 1 or 2 steps. In how many distinct
-- wys can you climb to the top.

climbStairs :: Int -> Int
climbStairs n = go 0
  where
  go n'
    | n' < n  = go (n'+1) + go (n'+2)
    | n' == n = 1
    | n' > n  = 0

memoClimbStairs :: Int -> Int
memoClimbStairs = memoFix2 f 0
  where
  f g n' n = if | n' < n  -> f g (n'+1) n + f g (n'+2) n
                | n' == n -> 1
                | n' > n  -> 0

-- House Robber
--
-- You are a professional robber planning to rob houses along a street. Each house
-- has a certain amount of money stashed, the only constraint stopping you from
-- robbing each of them is that adjacent houses have security systems connected
-- and it will automatically contact the police if two adjacent houses were
-- broken into on the same night.
-- 
-- Given an integer array nums representing the amount of money of each house,
-- return the maximum amount of money you can rob tonight without alerting the
-- police.

rob :: [Int] -> Int
rob [x]          = x
rob (x:q@(_:xs)) = max (x + rob xs) (rob q)
rob _            = 0

memoRob :: [Int] -> Int
memoRob = memoFix f
  where
  f _ [x]          = x
  f g (x:q@(_:xs)) = max (x + f g xs) (f g q)
  f _ _            = 0

-- Longest Palindrome Substring
-- 
-- Given a string s, return the longest palindromic substring in s.
-- A string is called a palindrome string if the reverse of that string is the same as the
-- original string.

longestPalindrome :: String -> String
longestPalindrome = List.maximumBy (\s1 s2 -> compare (length s1) (length s2)) . filter isPalindrome . List.subsequences

-- Reverse a linked list
--
-- Given the head of a singly linked list, reverse the list, and return the reversed list.

reverseList :: [a] -> [a]
reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Reverse a linked list II
--
-- Given the head of a singly linked list and two integers left and right where left <=
-- right, reverse the nodes of the list from position left to position right, and return the
-- reversed list.

reverseBetween :: Int -> Int -> [Int] -> [Int]
reverseBetween left right xs = l ++ r ++ m
  where
  l = take (left-1) xs
  m = drop right xs
  r = reverseList . take (right - (left-1)) . drop (left-1) $ xs

-- Reverse Nodes in k-Group
--
-- Given the head of a linked list, reverse the nodes of the list k at a time,
-- and return the modified list.
--
-- k is a positive integer and is less than or equal to the length of the linked
-- list. If the number of nodes is not a multiple of k then left-out nodes, in
-- the end, should remain as it is.
--
-- You may not alter the values in the list's nodes, only nodes themselves
-- may be changed.

reverseKGroup :: [a] -> Int -> [a]
reverseKGroup xs k = concatMap (\ys ->
  if length ys == k
  then reverseList ys
  else ys) $ Split.chunksOf k xs

-- Tree Breadth First Search

data BinaryTree' a = Empty' | Node' (BinaryTree' a) a (BinaryTree' a)
    deriving (Show, Eq)

newtype Preorder    a = PreOrder    (BinaryTree' a) deriving (Eq, Show)
newtype PostOrder   a = PostOrder   (BinaryTree' a) deriving (Eq, Show)
newtype Levelorder  a = LevelOrder  (BinaryTree' a) deriving (Eq, Show)
newtype ZigZagOrder a = ZigZagOrder (BinaryTree' a) deriving (Eq, Show)

type OrderedTree = Foldable

tree :: BinaryTree' Integer
tree = Node' (Node' Empty' 9 Empty') 3 (Node' (Node' Empty' 15 Empty') 20 (Node' Empty' 7 Empty'))

instance Foldable BinaryTree' where
  foldr :: (a -> b -> b) -> b -> BinaryTree' a -> b
  foldr f ini Empty' = ini
  foldr f ini (Node' left center right) = foldr f (f center (foldr f ini right)) left

instance Foldable Preorder where
  foldr :: (a -> b -> b) -> b -> Preorder a -> b
  foldr f ini (PreOrder Empty') = ini
  foldr f ini (PreOrder (Node' left center right)) = f center (foldr f (foldr f ini (PreOrder right)) (PreOrder left))

instance Foldable PostOrder where
  foldr :: (a -> b -> b) -> b -> PostOrder a -> b
  foldr f ini (PostOrder Empty') = ini
  foldr f ini (PostOrder (Node' left center right)) = foldr f (foldr f (f center ini) (PostOrder right)) (PostOrder left)

instance Foldable Levelorder where
  foldr :: (a -> b -> b) -> b -> Levelorder a -> b
  foldr f ini (LevelOrder Empty') = ini
  foldr f ini (LevelOrder tree@(Node' left center right)) = helper [tree]
      where
          helper [] = ini
          helper (Empty' : xs) = helper xs
          helper ((Node' left center right) : xs) = f center (helper (xs ++ [left, right]))
