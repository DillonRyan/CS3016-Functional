-- Name: Patrick Dillon Ryan,  Username: ryanp30
module Ex01 where
import Data.Char (toUpper) -- needed for Part 1

{- Part 1

Write a function 'raise' that converts a string to uppercase

Function 'toUpper :: Char -> Char' converts a character to uppercase
if it is lowercase. All other characters are unchanged

-}
--raise _ = error "raise not yet implemented"
raise lowerToUpper = map toUpper lowerToUpper


{- Part 2

Write a function 'nth' that returns the nth element of a list

-}
--nth :: Int -> [a] -> a
--nth _ _ = error "raise not yet implemented"
nth :: Int -> [a] -> a
nth 1 (a:as) = a
nth n (a:as) = nth (n - 1) as

{- Part 3

write a function commonLen that compares two sequences
and reports the length of the prefix they have in common.

-}
--commonLen :: Eq a => [a] -> [a] -> Int
--commonLen _ _ = error "commonLen not yet implemented"
commonLen :: Eq a => [a] -> [a] -> Int
commonLen (a:as) (b:bs)
	| a == b = 1 + commonLen as bs
commonLen _ _ = 0
