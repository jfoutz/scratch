module Lib
    ( someFunc
    ) where

-- https://wiki.haskell.org/99_questions/1_to_10
-- (*) Find the last element of a list.
mylast :: [a] -> a
mylast [] = error "empty list"
mylast (x : []) = x
mylast (_ : xs) = mylast xs

chartostr :: Char -> String
chartostr c = [c]

t1 :: String
t1 = "the end"

-- (*) Find the last-but-one (or second-last) element of a list.
butlast :: [a] -> a
butlast []  = error "need a few"
butlast (_ : []) = error "need a few"
butlast (x : _ : []) = x
butlast (_:xs) = butlast xs

-- (*) Find the K'th element of a list.

elt :: Int -> [a] -> a
elt _ [] = error "n bigger than list"
elt 0 (x:_) = x
elt n (_:xs) = elt (n-1) xs

-- (*) Find the number of elements in a list.

mylen :: [a] -> Int
mylen [] = 0
mylen (_:xs) = 1 + mylen xs

-- (*) Reverse a list.
myrev :: [a] -> [a]
myrev ls = help [] ls
  where
    help acc [] = acc
    help acc (x:xs) = help (x:acc) xs

-- (*) Find out whether a list is a palindrome.

ispal :: (Eq a) => [a] -> Bool
ispal xs = xs == myrev xs

t2 :: String
t2 = "racecar"

-- (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a] 

t3 :: NestedList Char
t3 = List [Elem 'h', List [Elem 'i', Elem '!'] ]

myconcat :: [a] -> [a] -> [a]
myconcat [] ys = ys
myconcat (x:xs) ys = x : (myconcat xs ys)

flatten :: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = myconcat (flatten x) (flatten (List xs))

(**) Eliminate consecutive duplicates of list elements.
t4 :: String
t4 = "aaabbbcddd"
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:xs) = if x == y then
                      compress (x:xs)
                    else
                      x : compress (y:xs)

(**) Pack consecutive duplicates of list elements into sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []


someFunc :: IO ()
someFunc = do
  let tr1 = chartostr $ mylast t1
  putStrLn tr1
  let tr2 = chartostr $ butlast t1
  putStrLn tr2
  let tr3 = chartostr $ elt 1 t1
  putStrLn tr3
  let tr4 = show $ mylen t1
  putStrLn tr4
  let tr5 = show $ myrev t1
  putStrLn tr5
  let tr6 = show $ ispal t2
  putStrLn tr6
  let tr7 = show $ flatten t3
  putStrLn tr7
  let tr8 = show $ compress t4
  putStrLn tr8