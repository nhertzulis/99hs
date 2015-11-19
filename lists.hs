-- Alternative solutions for a single problem keep the name of the first solution except they are
-- numbered accordingly. Auxiliary functions are marked with apostrophes.

-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myLast2 :: [a] -> a
myLast2 = head . reverse

-- Problem 2: Find the last but one element of a list.
myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: (Integral b) => [a] -> b -> a
elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4: Find the number of elements of a list.
myLength :: (Integral b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength2 :: (Integral b) => [a] -> b
myLength2 xs = myLength2' xs 0

myLength2' :: (Integral b) => [a] -> b -> b
myLength2' [] accum = accum 
myLength2' (_:xs) accum = myLength2' xs (1+accum)

-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse xs = myReverse' xs []

myReverse' :: [a] -> [a] -> [a]
myReverse' [] ys = ys
myReverse' (x:xs) ys = myReverse' xs (x:ys)

myReverse2 xs = foldl (flip (:)) [] xs

-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward;
-- e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs)
	| x /= last xs = False
	| otherwise = isPalindrome $ init xs

isPalindrome2 [] = True
isPalindrome2 (x:[]) = True
isPalindrome2 (x:xs)
	| x /= lastX = False
	| otherwise = isPalindrome2 initXS
	where (initXS, lastX) = initAndLast xs

initAndLast :: [a] -> ([a], a)
initAndLast (x:[]) = ([], x)
initAndLast (x:xs) = let (ys, y) = initAndLast xs in ((x:ys), y)

-- Problem 7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = x:[]
flatten (List []) = []
flatten (List (xs:xss)) = flatten xs ++ flatten (List xss)

-- Problem 8: Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = x:[]
compress (x:x':xs)
	| x == x' = compress (x':xs)
	| otherwise = x : compress (x':xs)
	
-- Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains
-- repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:[]) = (x:[]):[]
pack (x:xs)
	| x == y = (x:y:ys):zs
	| otherwise = (x:[]):(y:ys):zs
	where ((y:ys):zs) = pack xs

-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates of elements are
-- encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a, Integral b) => [a] -> [(b, a)]
encode [] = []
encode xs =
	map (\ys -> (fromIntegral (length ys), head ys)) $ pack xs 

-- The following implementation does not use the function pack
encode2 :: (Eq a, Integral b) => [a] -> [(b, a)]
encode2 [] = []
encode2 (x:[]) = (1, x):[]
encode2 (x:xs)
	| x == y = (n+1, y):zs
	| otherwise = (1, x):(n, y):zs
	where (n, y):zs = encode2 xs

encode3 :: (Eq a, Integral b) => [a] -> [(b, a)]
encode3 xs = foldr addElementToEncodedList [] xs
	where
	addElementToEncodedList x [] = (1, x):[]
	addElementToEncodedList x ((n, y):zs)
		| x == y = (n+1, y):zs
		| otherwise = (1, x):(n, y):zs

-- Problem 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply
-- copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Integer a deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map f . encode
	where
	f (1, x) = Single x
	f (n, x) = Multiple n x

-- Problem 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed
-- version.
decodeModified  :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple 2 x):xs) = x : x : decodeModified xs
decodeModified ((Multiple n x):xs) = x : decodeModified ((Multiple (n-1) x):xs)

-- Problem 13: Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't
-- explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:[]) = (Single x):[]
encodeDirect (x:xs)
	| e == x = (Multiple (n+1) x):ys
	| otherwise = (Single x):y:ys
	where
	(y:ys) = encodeDirect xs
	tupleOfListItem (Single x) = (1, x)
	tupleOfListItem (Multiple n x) = (n, x)
	(n, e) = tupleOfListItem y

