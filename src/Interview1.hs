module Interview1 where

-- 1) Implement a function in Haskell to find the longest palindrome in a given string.
longestPalindrome :: String -> String
longestPalindrome = undefined

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- 2) Write a function in Haskell to perform a binary search on a sorted list.
binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch _ [] = Nothing
binarySearch x xs | x == r    = Just index
                  | r <  x    = (+index) . (+1) <$> binarySearch x rs
                  | otherwise = binarySearch x l
        where index = length xs `quot` 2
              (l,r:rs) = splitAt index xs

-- 3) Write a function in Haskell to calculate the nth Fibonacci number using memoization.
fib:: Int -> Integer
fib = fib' <$> ([0..] !!)
     where fib' 0 = 1
           fib' 1 = 1
           fib' n = fib (n-2) + fib (n-1)

-- 4) Implement a function in Haskell to find the shortest path between two nodes in a graph using Dijkstra's algorithm.
-- 5) Write a function in Haskell to check if a given binary tree is a binary search tree.
-- 6) Implement a function in Haskell to solve the Tower of Hanoi problem with n disks.
-- 7) Write a function in Haskell to perform a topological sort on a directed acyclic graph.

exampleGraph :: [(String, [String])]
exampleGraph = [("a", ["b", "c"]), ("b", ["d"]), ("c", ["d"]), ("d", [])]

directedAcyclicGraphTopologicalSort :: Ord a => [(a, [a])] -> [a]
directedAcyclicGraphTopologicalSort = go
  where
  go [] = []
  go xs = let (ys, zs) = partition (null . snd) xs
              (as, bs) = partition (null . snd) zs
          in (fmap fst ys <> go (fmap (second (\\ fmap fst ys)) bs))
  partition p = foldr (\x ~(as,bs) -> if p x then (x:as,bs) else (as,x:bs)) ([],[])
  (\\) = foldl (flip delete)
  delete x = filter (/= x)
  second f (a,b) = (a, f b)

-- 8) Implement a function in Haskell to find the longest common subsequence of two strings using dynamic programming.
-- 9) Write a function in Haskell to compute the determinant of a square matrix using Gaussian elimination.
-- 10) Implement a function in Haskell to solve the knapsack problem using dynamic programming.
