-- Problem 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply
-- copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Integer a deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified (x:[]) = (Single x):[]
encodeModified (x:xs)
	| e == x = (Multiple (n+1) x):ys
	| otherwise = (Single x):y:ys
	where
		(y:ys) = encodeModified xs
		tuple (Single x) = (1, x)
		tuple (Multiple n x) = (n, x)
		(n, e) = tuple y