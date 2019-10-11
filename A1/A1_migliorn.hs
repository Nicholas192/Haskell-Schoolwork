{-# OPTIONS_GHC -Wall #-}
module A1_migliorn where


import Prelude hiding (sum, product , elem)
import Test.QuickCheck
import Data.List ((\\))
import Text.Show.Functions


{-
README######################################################################################################################

By Nicholas Migliore, migliorn, 400071112

run this file with ghci, ghci A1_migliorn.hs

use the main function to run all of the property checks

note that there are many helper functions throughout the program that make the questions themselves easier
I initially tried placing them all at the top for convienience, but some required the use of functions in the questions, so I
had to put those underneath the functions somewhere. I apologize if this makes my code seem messy.
-}


--Unrelated properties that may be used for more than one question##########################################################

-- Property that asserts that all elements ar in increasig order, comment like this on assignment
sorted :: [Integer] -> Bool
sorted [] = True
sorted (_:[]) = True
sorted (x:y:xs) = x<=y && sorted (y:xs)

--this does the same thing but only gives the indexes
mark' :: a -> (a -> a) -> [b] -> [ a ]
mark' a f [] = []
mark' a f (x:xs) = a : mark' (f a) f xs

--markInd indecies that match the given integer value and return that list of indecies
--this is an auxhilary function used in pos for Q1
markInd :: Num a => Integer -> a -> (a -> a) -> [Integer] -> [ a ]
markInd y a f [] = []
markInd y a f (x:xs) = if y == x then a : markInd y (f a) f xs else markInd y (f a) f xs


--reverses a list
rev :: [a] -> [a]
rev l = doit [] l
  where
    doit ans [] = ans
    doit ans (x:xs) = doit (x:ans) xs

--Question 1 #######################################################################################################################################

{-
make three functions
matches returns list of all elements in the integer array that match the first element
elem returns a boolean which tells you if the ﬁrst integer is an element of the list
pos returns the list of 0-indexed positions where the ﬁrst integer occurs



Out of the three functions, I think matches is the most useful as in prop_elem I use matches along with first_int in an equality statement 
to create the same output as elem. elem is useful for the isin function though, which is used in prop_pos to determine if the results 
of pos are in the list of all the indexes therefore, pos is the least useful of the three.
-}

{-
I use a helper function markInd for pos, and more helper functions for the properties like sorted, isin, mark' and first_int
-}

--matches returns list of all elements in the integer array that match the first element
matches :: Integer -> [Integer] -> [Integer] 
matches _ [] = []
matches x (y:ys) = if x == y then (y: (matches x ys)) else matches x ys

--elem returns a boolean which tells you if the ﬁrst integer is an element of the list
elem :: Integer -> [Integer] -> Bool 
elem _ [] = False
elem x (y:ys) = if x == y then True else elem x ys--False

--pos returns the list of 0-indexed positions where the ﬁrst integer occurs
pos :: Integer -> [ Integer ] -> [ Integer ] 
pos y l = markInd y 0 (+1) l


--prove that the result of matches for a given array only contains the first number, or is an empty array
prop_matches_eq :: Integer -> [Integer] -> Bool
prop_matches_eq x xs = pme x (matches x xs)
  where pme _ [] = True--this evaluates to true because the empty list input means that the result of matches is an empty list, if there is no matches then it should return empty, so this is correct behaviour
        pme x (y:ys) = if x == y then pme x ys else False


-- This property returns true if every element in the first list exists in the second, 
-- and false if it finds an element in the first that is not in the second
--uses elem and to be honest it is falsifiable when run with quickcheck but in prop_pos_sorted, it works as intended
-- due to controlled input
isin :: [Integer] -> [Integer] -> Bool
isin [] []     = True
isin _ []      = False
isin [] (y:ys) = True
isin (x:xs) ys = elem x ys && isin xs ys


--return a list containing only the first integer of a list of integers
first_int :: [Integer] -> [Integer]
first_int []     = []
first_int (x:xs) = [x]

--Test to see if the first element matches an element of the list. In cases where this holds true, it should be the case that
--elem also returns true
prop_elem :: Integer -> [Integer] -> Bool 
prop_elem _ [] = True
prop_elem x l  = if ([x] == first_int (matches x l)) == (elem x l) then True else False

--Prove that the indexes of pos are in the right order, and is in the complete list of indexes
prop_pos_sorted :: Integer -> [Integer] -> Bool
prop_pos_sorted x xs = sorted (pos x xs) && isin (pos x xs) (mark' 0 (+1) xs)


-- Questoion 2 #########################################################################################################################

--Takes a list of function, and applies all of them to all the elements of the second list. 
--For example, if the list of functions has 5 elements, and the list of values has 3, the resulting list has 15 elements. 
applyAll :: [ a -> b] -> [ a ] -> [ b ]
applyAll [] _          = []
applyAll _ []          = []
applyAll [f] (x:xs)    = (f x) :(applyAll [f] xs)
applyAll (f:fs) (x:xs) = (applyAll [f] (x:xs)) ++ (applyAll fs (x:xs))


--We need to test that the resulting list has the proper length (i.e. 5*3 = 15)
prop_aa_len :: [a -> b] -> [a] -> Bool
prop_aa_len f l = (length f) * (length l) == length (applyAll f l)

--apply only one function to a list to get a list b
applyOne :: (a -> b) -> [a] -> [b]
applyOne _ []     = []
applyOne f []     = []
applyOne f (x:xs) = (f x) : (applyOne f xs)

--test that the output of applyAll is the same as each function being individually applied to the list
prop_aa_one :: (Eq b) => [a -> b] -> [a] -> Bool
prop_aa_one []  _      = prop_aa_one [(+1)] [1]
prop_aa_one [f] l      = applyAll [f] l == applyOne f l
prop_aa_one (f:fs) l = (applyAll [f] l == applyOne f l) && (prop_aa_one fs l)

-- Question 3 ############################################################################################################################

--write a function goes over a list of numbers and doubles each positive one twice,
--do it with recursion and using either and map

--This one uses explicit recursion over the list
doublePos1 :: (Ord a , Num a) => [ a ] -> [ a ] 
doublePos1 []     = []
doublePos1 (x:xs) = if x >= 0 then (2*x) : (doublePos1 xs) else (x) : doublePos1 xs

--helperDouble converts a num x to an Either type, Left if x >= 0 and Right otherwise
helperDouble :: (Ord a, Num a) => a -> Either a a
helperDouble x | x >= 0    = Left x 
               | otherwise = Right x 

--This one uses the Haskell functions either and map
doublePos2 :: (Ord a , Num a) => [ a ] -> [ a ] 
doublePos2 l = map (\y -> either (*2) (*1) (helperDouble y) ) l


--this property checks that the two versions of doublePos are equivalent
prop_doub_eq :: (Ord a, Num a) => [a] -> Bool
prop_doub_eq l = (doublePos1 l) == (doublePos2 l)

--This property checks that the result of doublePos1 with reversed input is the same as the normal output reversed
prop_doub_rev :: (Ord a, Num a) => [a] -> Bool
prop_doub_rev l = rev (doublePos1 l) == doublePos1 (rev l)


-- Question 4 ################################################################################################################################

{-
with the use of a single magic function alone I define sum, product and flatten

sum returns the sum of every number in the list
  sum [3,6,9] = 18

product returns the product of every number in the list
  product [3,6,9] = 162

flatten turns a list of lists into a single list
  flatten [[1,2], [3,4]] = [1,2,3,4]

-}


--much like fold, magic applies a function to a list. If the list is empty, then the result is the second value z
magic :: (a -> a -> a) -> a -> [ a ] -> a 
magic f z []     = z
magic f z (x:xs) = f x (magic f z xs)

--computes the sum of a given list of numbers
sum :: Num a => [ a ] -> a 
sum l = magic (+) 0 l

--computes the product of a given list of numbers
product :: Num a => [ a ] -> a 
product l = magic (*) 1 l

--flatten a list of lists
flatten :: [[b]] -> [b]
flatten xss = magic (++) [] xss

--property checks that Sum of a list is the same as the sum of it's reverse
prop_sum_rev :: (Eq a, Num a) => [a] -> Bool
prop_sum_rev l = sum (rev l) == sum l

--property checks that Sum of a single number and zero is equal to the original number
prop_sum_identity :: (Eq a, Num a) => a -> Bool
prop_sum_identity x = sum [x, 0] == x

--property checks that Product of a list is the same as the product of it's reverse
prop_prod_rev :: (Eq a, Num a) => [a] -> Bool
prop_prod_rev l = product (rev l) == product l

--property checks that the product of a single number and one is equal to the original number
prop_prod_identity :: (Eq a, Num a) => a -> Bool
prop_prod_identity x = product [x, 1] == x

--property checks that the list containing a list l and an empty list, when flattened, will be the list l
prop_flat_identity :: (Eq a, Num a) => [a] -> Bool
prop_flat_identity l = flatten [l, []] == l

--property checks that a list containing two lists l r the flattened result is l ++ r
prop_flat :: (Eq a, Num a) => [a] -> [a] -> Bool
prop_flat l r = flatten [l, r] == l ++ r


-- Question 5 ##############################################################################################################################

{-
Eitherboth is a datatype that contains either a or b, or it contains both a and b. I define two consume functions that apply functions
to Eitherboth to get a result c. I do this once with pattern matching and once with function composition.

Both versions take 3 functions and the Eitherboth type and returns c.

the first is a function a -> c, 
this gets applied to a if the Eitherboth type given is L a

the second is a function b -> c, 
this gets applied to b if the Eitherboth type given is L a


in the B a b case, the definition of the third function is different in the two consume functions

The first function, consume, only uses pattern matching:
  the third function a -> b -> c
  is applied to both a and b if the Eitherboth type is B a b
  consume will apply a function to both a and b
  thus a call to consume must have the 3rd function be applicable to two values a and b
  an example call for consume would be:
    consume (*2) (*3) (+) (B 2 3)

The second function consume' uses pattern matching and function composition:
  the third function b -> a
  is applied to b if the Eitherboth type is B a b,
  then the first function a -> c is then applied to that result.
  That's how function composition works!
  

  in the both case it is assumed that a and b are the same
  I also assume that the third function is 
  thus a call to consume' must have the 3rd function be applicable to one value
  an example call for consume' would be:
    consume' (+2) (*3) (+3) (B 2 2)



  i think that the better of the two is the first one, consume. This is because the way it applies a different kind of function to
  both a and b is more practical than the options the second version gives you.
-}
data Eitherboth a b = L a | R b | B a b 
    deriving (Show)


  

--the first definition of consume that uses pattern matching
consume :: (a -> c) -> (b -> c) -> (a -> b -> c) -> Eitherboth a b -> c
consume f _ _ (L a) = f a
consume _ g _ (R b) = g b
consume _ _ h (B a b) = h a b



--This version of consume uses function composition
consume' :: (a -> c) -> (b -> c) -> (b -> a) -> Eitherboth a b -> c
consume' f _ _ (L a) = f a
consume' _ g _ (R b) = g b
consume' f g h (B a b) = f $ h b--in the both case it is assumed that a and b are the same


--this is done so testing can work
instance (Arbitrary a, Arbitrary b) => Arbitrary (Eitherboth a b) where
  arbitrary = do
      -- aa and bb comes from arbitrary, must be of type Gen aa and Gen bb
      aa <- arbitrary
      bb <- arbitrary
      -- combinator that picks one of a list of things
      oneof [return $ L aa, return $ R bb, return $ B aa bb]


--properties
{-
MORE PROSE HERE!
since there are so few concrete invariants that can be used for the consume function there is very little to test here
-}

--as long as the both case is not called, consume is equivalent to consume'
prop_cons_eq :: Eq c => (a -> c) -> (b -> c) -> (a -> b -> c) -> (b -> a) -> Eitherboth a b -> Bool
prop_cons_eq f g h j (L a) = consume f g h (L a) == consume' f g j (L a)
prop_cons_eq f g h j (R b) = consume f g h (R b) == consume' f g j (R b)
prop_cons_eq f g h j (B a b) = consume f g h (R b) == consume' f g j (R b)



--in the left and right cases, both consume cases are identical to either
prop_cons_eith :: Eq c => (a -> c) -> (b -> c) -> (a -> b -> c) -> (b -> a) -> Eitherboth a b -> Bool
prop_cons_eith f g h j (L a) = consume f g h (L a) == consume' f g j (L a) && consume f g h (L a) == either f g (Left a)
prop_cons_eith f g h j (R b) = consume f g h (R b) == consume' f g j (R b) && consume f g h (R b) == either f g (Right b)
prop_cons_eith f g h j (B a b) = consume f g h (R b) == consume' f g j (R b) && consume f g h (R b) == either f g (Right b)


-- Question 6 ################################################################################################################################################

{-
This question makes use of emily's notes

I define a rose tree datatype

then create a function that mirrors the tree by changing the position of it's children
i.e. by going from:
    1
  2   3
to:
    1
  3   2


  we also create another function that flattens the tree
  i.e. from:
                1
        11                12
  111       112     121   122   123

  to:
  [1, 11, 111, 112, 12, 121, 122, 123]


  
--example trees
--let s = MkRose 5 [MkRose 3 []]
--let r = MkRose 1 [MkRose 11 [], MkRose 12 []]
--let q = MkRose (-1) [MkRose 4 [MkRose (-1) [],MkRose 0 [MkRose (-2) [],MkRose (-1) [],MkRose (-4) []]],MkRose 1 [MkRose (-3) [],MkRose 2 [MkRose 4 []]]]
--let t = (MkRose 1 [MkRose 11 [MkRose 111 [], MkRose 112[]], MkRose 12 [MkRose 121[], MkRose 122[], MkRose 123 [] ]])

-}


data Rose a = MkRose a [Rose a] deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Rose a) where
    arbitrary = 
      let
        numChildren :: Int -> Int
        numChildren =
          floor . sqrt . fromIntegral
  
        makeChild :: Arbitrary a => Int -> Gen (Rose a)
        makeChild n =
          tree $ n `div` 2
  
        branch :: Arbitrary a => Int -> Int -> Gen [Rose a]
        branch n limit =
          mapM (const (makeChild n)) [0..numChildren limit]
  
        leaf :: Arbitrary a => Gen (Rose a)
        leaf =
          MkRose <$> arbitrary <*> return []
  
        tree :: Arbitrary a => Int -> Gen (Rose a)
        tree 0 =
          leaf
        tree n =
            -- Choose either a leaf node or a branch with size ∼ n.
            oneof
              [ leaf
              , MkRose <$> arbitrary <*> (arbitrary >>= branch n)
              ]
      in
        sized tree

--mirror the tree
mirror :: Rose a -> Rose a 
mirror (MkRose a l) = (MkRose a (rev l))

--flatten the tree
flattenRose :: Rose a -> [ a ] 
flattenRose (MkRose a l) = [a] ++ (concat $ map flattenRose l)


--properties

--this property assesrt that the mirror of a mirrored tree is the original tree
prop_mirror_cancel :: Eq a => Rose a -> Bool
prop_mirror_cancel r = r == mirror (mirror r)

--mirroring a rose tree with only a single leaf node gives the original tree
prop_mirror_empty :: Eq a => a -> Bool
prop_mirror_empty a = (MkRose a []) == mirror (MkRose a [])



--property checks that the tree containing a and an empty list, when flattened, will be the list [a]
prop_flat_R_identity :: (Eq a, Num a) => a -> Bool
prop_flat_R_identity a = flattenRose  (MkRose a []) == [a]

--property checks that for a tree with head a containing two leaf children l and r the flattened result is a ++ l ++ r
prop_flat_R :: (Eq a, Num a) => a -> a -> a -> Bool
prop_flat_R a l r = flattenRose (MkRose a [MkRose l [], MkRose r []]) == [a] ++ [l] ++ [r]

--main function #########################################################################################################################################
main :: IO ()
main = do
    --question 1
    quickCheck prop_matches_eq
    quickCheck prop_elem
    quickCheck prop_pos_sorted
    --question 2
    quickCheck (prop_aa_len :: [Integer -> Integer] -> [Integer] -> Bool)-- [(+2), (+3), (*2), (*1), (*0)]
    quickCheck (prop_aa_one :: [Integer -> Integer] -> [Integer] -> Bool)
    --question 3
    quickCheck (prop_doub_eq ::  [Integer] -> Bool)
    quickCheck (prop_doub_rev ::  [Integer] -> Bool)
    --Question 4
    quickCheck (prop_sum_rev :: [Integer] -> Bool)
    quickCheck (prop_sum_identity :: Integer -> Bool)
    quickCheck (prop_prod_rev :: [Integer] -> Bool)
    quickCheck (prop_prod_identity :: Integer -> Bool)
    quickCheck (prop_flat_identity :: [Integer] -> Bool)
    quickCheck (prop_flat :: [Integer] -> [Integer] -> Bool)
    --question 5
    quickCheck (prop_cons_eq :: (Int -> Int) -> (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int) -> Eitherboth Int Int -> Bool)
    quickCheck (prop_cons_eith :: (Int -> Int) -> (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int) -> Eitherboth Int Int -> Bool)
    --question 6
    quickCheck (prop_mirror_cancel :: Rose Integer -> Bool)
    quickCheck (prop_mirror_empty :: Integer -> Bool)
    quickCheck (prop_flat_R_identity :: Integer -> Bool)
    quickCheck (prop_flat_R :: Integer -> Integer -> Integer -> Bool)