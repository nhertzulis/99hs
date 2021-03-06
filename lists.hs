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

myButLast2 :: [a] -> a
myButLast2 = last . init

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: (Integral b) => [a] -> b -> a
elementAt (x:_) 1 = x
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

isPalindrome3 :: (Eq a) => [a] -> Bool
isPalindrome3 xs = xs == reverse xs

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

compress2 :: (Eq a) => [a] -> [a]
compress2 [] = []
compress2 (x:xs) = x:(compress2 $ dropWhile (== x) xs)
   
-- Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains
-- repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:[]) = (x:[]):[]
pack (x:xs)
	| x == y = (x:y:ys):zs
	| otherwise = (x:[]):(y:ys):zs
	where ((y:ys):zs) = pack xs

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) =
    let (ys, zs) = span (== x) xs
    in (x:ys):pack2 zs 

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
decodeModified ((Multiple n x):xs)
    | n == 2 = x : x : decodeModified xs
    | n > 2 = x : decodeModified ((Multiple (n-1) x):xs)

decodeModified2  :: (Eq a) => [ListItem a] -> [a]
decodeModified2 = concat . map decodeElement
    where
        decodeElement (Single x) = x:[]
        decodeElement (Multiple n x) = take (fromIntegral n) $ repeat x 

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
	decodeListItem (Single x) = (1, x)
	decodeListItem (Multiple n x) = (n, x)
	(n, e) = decodeListItem y

encodeDirect2 :: (Eq a) => [a] -> [ListItem a]
encodeDirect2 [] = []
encodeDirect2 (x:[]) = (Single x):[]
encodeDirect2 (x:x':xs)
    | x == x' = (addOneMore y):ys
    | otherwise = (Single x):y:ys
    where
        (y:ys) = encodeDirect2 $ x':xs
        addOneMore (Single x) = Multiple 2 x
        addOneMore (Multiple n x) = Multiple (n+1) x

-- Problem 14: Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15: Replicate the elements of a list a given number of times.
repli :: (Integral b) => [a] -> b -> [a]
repli [] _ = []
repli _ 0 = []
repli (x:xs) n = repliElem x n ++ repli xs n

repliElem :: (Integral b) => a -> b -> [a]
repliElem _ 0 = []
repliElem x n = x:repliElem x (n-1)

repli2 :: (Integral b) => [a] -> b -> [a]
repli2 [] _ = []
repli2 _ 0 = []
repli2 (x:xs) n = repli2' x n $ repli2 xs n
	where
	repli2' _ 0 xs = xs
	repli2' x n xs = x:repli2' x (n-1) xs

repli3 :: (Integral b) => [a] -> b -> [a]
repli3 [] _ = []
repli3 (x:xs) n = (take (fromIntegral n) $ repeat x) ++ repli3 xs n

-- Problem 16: Drop every N'th element from a list.
dropEvery :: (Integral b) => [a] -> b -> [a]
dropEvery _ 1 = []
dropEvery xs k = dropEvery' xs k k

dropEvery' :: (Integral b) => [a] -> b -> b -> [a]
dropEvery' [] _ _ = []
dropEvery' (x:xs) k 1 = dropEvery' xs k k
dropEvery' (x:xs) k remaining = x:dropEvery' xs k (remaining-1) 

dropEvery2 :: [a] -> Int -> [a]
dropEvery2 [] _ = []
dropEvery2 xs k =
    let (ys, zs) = splitAt (k-1) xs
    in ys ++ dropEvery2 (drop 1 zs) k

-- Problem 17: Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: (Integral b) => [a] -> b -> ([a], [a])
split xs n
    | n <= 0 = ([], xs)
    | null xs = ([], [])
    | otherwise = (head xs:ys, zs)
	where (ys, zs) = split (tail xs) (n-1)
	
-- Problem 18: Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and
-- k'th element of the original list (both limits included). Start counting the elements with 1.
slice :: (Integral b) => [a] -> b -> b -> [a]
slice [] _ _ = []
slice (x:_) 1 1 = x:[]
slice (x:xs) 1 k = x:slice xs 1 (k-1)
slice (x:xs) i k = slice xs (i-1) (k-1)

slice2 :: (Integral b) => [a] -> b -> b -> [a]
slice2 xs i k = fst $ split (snd $ split xs (i-1)) (k-i+1)

slice3 :: [a] -> Int -> Int -> [a]
slice3 xs i k = take (k-i+1) $ drop (i-1) xs

-- Problem 19: Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate xs n = zs++ys
	where (ys, zs) = split xs (n `mod` length xs)
	
rotate2 :: [a] -> Int -> [a]
rotate2 xs n
	| n < 0 = rotate2 xs (length xs + n)
	| n > 0 = let (ys, zs) = split xs n in zs++ys
	| otherwise = xs

-- Problem 20: Remove the K'th element from a list.
removeAt :: (Integral a) => a -> [b] -> (b, [b])
removeAt 1 (x:xs) = (x, xs)
removeAt k (x:xs) = (y, x:ys)
	where (y, ys) = removeAt (k-1) xs

removeAt2 :: (Integral a) => a -> [b] -> (b, [b])
removeAt2 k xs =
    let (ys, (z:zs)) = split xs (k-1)
    in (z, ys++zs)

-- Problem 21: Insert an element at a given position into a list.
insertAt :: (Integral b) => a -> [a] -> b -> [a]
insertAt x' xs 1 = x':xs
insertAt x' (x:xs) n = x:insertAt x' xs (n-1)

insertAt2 :: a -> [a] -> Int -> [a]
insertAt2 x xs n = fst $ foldr f ([], length xs) xs 
	where
		f y (ys, i) = ((if i == n then x:y:ys else y:ys), i-1)
	
insertAt3 :: a -> [a] -> Int -> [a]
insertAt3 elem list pos = foldr addElem [] $ zip [1..] list
	where addElem (i, x) xs
		| i == pos  = elem:x:xs
		| otherwise = x:xs

insertAt4 :: a -> [a] -> Int -> [a]
insertAt4 x xs k =
    let (ys, zs) = split xs (k-1)
    in ys ++ (x:zs)

-- Problem 22: Create a list containing all integers within a given range.
range :: Integer -> Integer -> [Integer]
range a b
	| a <= b = a:range (a+1) b
	| otherwise = []
	