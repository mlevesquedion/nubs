import           Data.List

nub1 :: Ord a => [a] -> [a]
nub1 = concatMap (take 1) . group . sort

toSet :: Eq a  => [a] -> [a]
toSet []     = []
toSet (x:xs) = toSet' x xs
  where
    toSet' comparator [] = [comparator]
    toSet' comparator (x:xs) = if comparator == x then toSet' comparator xs
                                                else comparator:toSet' x xs

nub2 :: Ord a => [a] -> [a]
nub2 = toSet . sort

main :: IO ()
main = do
  let numbers = [1, 2, 3, 1, 1, 2, 1, 3, 4, 23, 10, 2, 3] :: [Int]
  let expected = [1, 2, 3, 4, 10, 23] :: [Int]
  let nubs = [nub1, nub2]
  let nubbed = expected : map ($numbers) nubs
  if (length $ nub nubbed) == 1 then
                            putStrLn "SUCC"
                            else
                            putStrLn "FAIL"
