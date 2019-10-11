module A5 where

-- The following are useful for the assignment
import Data.List (intersperse, nub)

-- Useful extra imports for the Bonus only
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Test.QuickCheck
-- Here is the class that the assignment is all about: a representation
-- of boolean expressions with variables.
-- Note: 
-- 1) true is represented as 'andB []' 
-- 2) false as 'orB []'
-- 3) 'andB [e]' means the same as 'e', same with 'orB [e]'.
class BoolExpr repr where
  varB :: String -> repr
  notB :: repr -> repr
  andB :: [repr] -> repr
  orB :: [repr] -> repr

-- Some useful test cases:
ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: BoolExpr repr => repr
ex1 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z"]]
ex2 = andB [andB [], notB $ varB "x", orB [varB "y", varB "z", varB "x"]]
ex3 = andB [andB [orB [varB "w"]]]
-- You should add more test cases, enough to cover all the 'corner' cases,
-- and then run them through all your examples.
ex4 = notB $ orB []
ex5 = orB [notB $ varB "a", varB "b"]
ex6 = notB $ andB [varB "a", varB "b"]
ex7 = notB $ orB [andB [], andB [varB "a", varB "b"], varB "c", varB "c"]
ex8 = varB "w"

----------------------------------------------------------------------------
-- First interpretation: as a String.
newtype Pr = Pr {view :: String}

instance BoolExpr Pr where
  varB s      = Pr { view = "var \"" ++ s ++ "\"" }
  notB r      = Pr { view = "(notB " ++ view r ++")"}
  orB []      = Pr { view = "(orB [])"}
  orB rs      = Pr { view = "(orB [" ++ (concat $ intersperse "," $ map view rs) ++ "])"}
  andB []     = Pr { view = "(andB [])"}
  andB rs     = Pr { view = "(andB [" ++ (concat $ intersperse "," $ map view rs) ++ "])"}

-- Test case:
-- view ex2 should return the String
-- "(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\",var \"x\"])])"
-- so that 'putStrLn $ view ex2' gives
-- (andB [(andB []),(notB var "x"),(orB [var "y",var "z",var "x"])])

q_1 :: IO ()
q_1 = do 
  putStrLn $ view ex1
  putStrLn $ view ex2
  putStrLn $ view ex3
  putStrLn $ view ex4
  putStrLn $ view ex5
  putStrLn $ view ex6
  putStrLn $ view ex7
  putStrLn $ view ex8

----------------------------------------------------------------------------
-- Second interpretation: pulling out the (free) variables:
newtype FV = FV {fv :: [String]}

instance BoolExpr FV where
  varB s      = FV {fv = [s]}
  notB r      = FV {fv = fv r}
  orB []      = FV {fv = nub []}
  orB rs      = FV {fv = nub (concat $ nub $ map fv rs) }
  andB []     = FV {fv = nub []}
  andB rs     = FV {fv = nub (concat $ nub $ map fv rs) }
  
-- Test case:
-- fv ex2 should return exactly
-- [ "x", "y", "z" ]
-- Hint: Data.List.nub

q_2 :: IO ()
q_2 = do 
  putStrLn $ show $ fv ex1
  putStrLn $ show $ fv ex2
  putStrLn $ show $ fv ex3
  putStrLn $ show $ fv ex4
  putStrLn $ show $ fv ex5
  putStrLn $ show $ fv ex6
  putStrLn $ show $ fv ex7
  putStrLn $ show $ fv ex8

----------------------------------------------------------------------------
-- Third interpretation: as 'syntax'
data BE = Var String | Not BE | And BE BE | Or BE BE | TrueB | FalseB
  deriving Show
asBE :: BE -> BE
--asBE = id
asBE (TrueB)   = TrueB
asBE (FalseB)  = FalseB
asBE (Var s)   = (Var s)
asBE (Not b)   = Not (asBE b)
asBE (And b c) = And (asBE b) (asBE c)
asBE (Or b c)  = Or (asBE b) (asBE c)

-- Hint: this instance has 8 cases rather than the usual 4
-- Hint: foldr1
instance BoolExpr BE where
  varB s      = asBE (Var s)
  notB r      = asBE (Not r)
  orB []      = FalseB
  orB [r]     = asBE r
--  orB (r:rs)  = asBE $ Or r $ map asBE rs
  orB (r:rs)  = asBE $ Or r $ (asBE $ orB rs)
  --orB rs      = foldr1 Or asBE rs 
  andB []     = TrueB
  andB [r]    = asBE r
  andB (r:rs) = asBE $ And r $ (asBE $ andB rs)

-- Test cases:
-- asBE ex1
-- And TrueB (And (Not (Var "x")) (Or (Var "y") (Var "z")))
-- asBE ex2
-- And TrueB (And (Not (Var "x")) (Or (Var "y") (Or (Var "z") (Var "x"))))
-- asBE ex3
-- Var "w"

q_3 :: IO ()
q_3 = do 
  putStrLn $ show $ asBE ex1
  putStrLn $ show $ asBE ex2
  putStrLn $ show $ asBE ex3
  putStrLn $ show $ asBE ex4
  putStrLn $ show $ asBE ex5
  putStrLn $ show $ asBE ex6
  putStrLn $ show $ asBE ex7
  putStrLn $ show $ asBE ex8

----------------------------------------------------------------------------
-- Fourth question: the other direction!
toBoolExpr :: BoolExpr repr => BE -> repr
--toBoolExpr _ = undefined
toBoolExpr (TrueB)            = andB []
toBoolExpr (FalseB)           = orB []
toBoolExpr (Var s)            = varB s
toBoolExpr (Not b)            = notB $ toBoolExpr b
--toBoolExpr (And b (And c d))  = andB $ [toBoolExpr b] ++ [toBoolExpr c] ++ [toBoolExpr d] -- this is flattening, we are not required to do it on the assignment
toBoolExpr (And b c)          = andB $ [toBoolExpr b] ++ [toBoolExpr c]
--toBoolExpr (Or b (Or c d))    = orB $ [toBoolExpr b] ++ [toBoolExpr c] ++ [toBoolExpr d] -- this is flattening, we are not required to do it on the assignment
toBoolExpr (Or b c)           = orB $ [toBoolExpr b] ++ [toBoolExpr c]

-- Part of this question: give an example that shows that you can
-- go in both directions (between BE and (BoolExpr repr => repr))
-- but that the translation is not the identity.



{-
below is an example showing that you can go in both directions, but the translation is not the identity
*A5> putStrLn $ view ex2
(andB [(andB []),(notB var "x"),(orB [var "y",var "z",var "x"])])
*A5> view $ toBoolExpr $ asBE ex2
"(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",(orB [var \"z\",var \"x\"])])])])"
-}

{-
if we included flattening then the result would be different

*A5> putStrLn $ view $ toBoolExpr $ asBE ex2
(andB [(andB []),(notB var "x"),(orB [var "y",var "z",var "x"])])
*A5> putStrLn $ view ex2
(andB [(andB []),(notB var "x"),(orB [var "y",var "z",var "x"])])

"(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",(orB [var \"z\",var \"x\"])])])])"
-}
--BoolExpr repr => repr -> 

{-
below is an example showing that you can go in both directions, but the translation is not the identity-}
prop_both1 :: Bool
prop_both1 = (view $ toBoolExpr $ asBE ex1) /= view ex1

prop_both2 :: Bool
prop_both2 = (view $ toBoolExpr $ asBE ex2) /= view ex2

prop_both3 :: Bool
prop_both3 = (view $ toBoolExpr $ asBE ex3) /= view ex3

prop_both4 :: Bool
prop_both4 = (view $ toBoolExpr $ asBE ex4) == view ex4

prop_both5 :: Bool
prop_both5 = (view $ toBoolExpr $ asBE ex5) == view ex5

prop_both6 :: Bool
prop_both6 = (view $ toBoolExpr $ asBE ex6) == view ex6

prop_both7 :: Bool
prop_both7 = (view $ toBoolExpr $ asBE ex7) /= view ex7

prop_both8 :: Bool
prop_both8 = (view $ toBoolExpr $ asBE ex8) == view ex8

q_4 :: IO ()
q_4 = do 
  putStrLn $ "for ex1"
  putStrLn $ "(view $ toBoolExpr $ asBE ex1), converting and then going back: "
  putStrLn $ (view $ toBoolExpr $ asBE ex1)
  putStrLn $ "view ex1, the original: "
  putStrLn $ (view ex1)
  putStrLn $ "the translation is not the identity because because of a lack of flattening, this is a choice"
  putStrLn $ "\nfor ex2"
  putStrLn $ "(view $ toBoolExpr $ asBE ex2), converting and then going back: "
  putStrLn $ (view $ toBoolExpr $ asBE ex2)
  putStrLn $ "view ex2, the original: "
  putStrLn $ (view ex2)
  putStrLn $ "the translation is not the identity because because of a lack of flattening, this is a choice"
  putStrLn $ "\nfor ex3"
  putStrLn $ "(view $ toBoolExpr $ asBE ex3), converting and then going back: "
  putStrLn $ (view $ toBoolExpr $ asBE ex3)
  putStrLn $ "view ex3, the original: "
  putStrLn $ (view ex3)
  putStrLn $ "the translation is not the identity because information is lost, because of choices when converting back"
  putStrLn $ "\nfor ex4"
  putStrLn $ "(view $ toBoolExpr $ asBE ex4), converting and then going back: "
  putStrLn $ (view $ toBoolExpr $ asBE ex4)
  putStrLn $ "view ex4, the original: "
  putStrLn $ (view ex4)
  putStrLn $ "In this case there are no choices that are needed when going back. No information is lost for ex4"
  putStrLn $ "\nfor ex5"
  putStrLn $ "(view $ toBoolExpr $ asBE ex5), converting and then going back: "
  putStrLn $ (view $ toBoolExpr $ asBE ex5)
  putStrLn $ "view ex5, the original: "
  putStrLn $ (view ex5)
  putStrLn $ "In this case there are no choices that are needed when going back. No information is lost for ex5"
  quickCheck prop_both1
  quickCheck prop_both2
  quickCheck prop_both3
  quickCheck prop_both4
  quickCheck prop_both5
  quickCheck prop_both6
  quickCheck prop_both7
  quickCheck prop_both8
  

----------------------------------------------------------------------------
-- Fifth question: compute the 'size' of an expression.
-- More precisely: every 'constructor' of the BoolExpr language counts
-- for 1.
-- size ex1 = 7
-- size ex2 = 8
-- size ex3 = 4
newtype Size = Sz {size :: Int}

instance BoolExpr Size where
  varB s      = Sz {size = 1}
  notB r      = Sz {size = 1 + size r}
  orB []      = Sz {size = 1}
  orB (r:rs)  = Sz {size = 1 + size r + sum (map size rs)}
  andB []     = Sz {size = 1}
  andB (r:rs) = Sz {size = 1 + size r + sum (map size rs)}

q_5 :: IO ()
q_5 = do 
  putStrLn $ show $ size ex1
  putStrLn $ show $ size ex2
  putStrLn $ show $ size ex3
  putStrLn $ show $ size ex4
  putStrLn $ show $ size ex5
  putStrLn $ show $ size ex6
  putStrLn $ show $ size ex7
  putStrLn $ show $ size ex8
----------------------------------------------------------------------------
-- Sixth question: compute the 'depth' of an expression (as a tree)
-- except that varB counts as 0 depth.
-- depth ex1 = 2
-- depth ex2 = 2
-- depth ex3 = 3
-- Hint: maximum
newtype Depth = De {depth :: Int}

instance BoolExpr Depth where
  varB s      = De {depth = 0}
  notB r      = De {depth = 1 + depth r}
  orB []      = De {depth = 1}
  orB (r:rs)  = De {depth = 1 + foldl (max) (depth r) (map depth rs)}
  andB []     = De {depth = 1}
  andB (r:rs) = De {depth = 1 + foldl (max) (depth r) (map depth rs)}
--depth andB [] == 1, depth orB [] == 1

-- Lastly, give an explicit example where going to BE and then back
-- to repr changes the depth of the results.


-- view $ toBoolExpr $ asBE ex1
-- "(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",var \"z\"])])])"
{-view $ toBoolExpr $ asBE ex1
"(andB [(andB []),(andB [(notB var \"x\"),(orB [var \"y\",var \"z\"])])])"
*A5> view ex1
"(andB [(andB []),(notB var \"x\"),(orB [var \"y\",var \"z\"])])"-}
prop_depth1 :: Bool
prop_depth1 = (depth $ toBoolExpr $ asBE ex1) /= depth ex1

prop_depth2 :: Bool
prop_depth2 = (depth $ toBoolExpr $ asBE ex2) /= depth ex2

prop_depth3 :: Bool
prop_depth3 = (depth $ toBoolExpr $ asBE ex3) /= depth ex3

prop_depth4 :: Bool
prop_depth4 = (depth $ toBoolExpr $ asBE ex4) == depth ex4

prop_depth5 :: Bool
prop_depth5 = (depth $ toBoolExpr $ asBE ex5) == depth ex5

prop_depth6 :: Bool
prop_depth6 = (depth $ toBoolExpr $ asBE ex6) == depth ex6

prop_depth7 :: Bool
prop_depth7 = (depth $ toBoolExpr $ asBE ex7) /= depth ex7

prop_depth8 :: Bool
prop_depth8 = (depth $ toBoolExpr $ asBE ex8) == depth ex8

q_6 :: IO ()
q_6 = do 
  putStrLn $ "\nfor ex1"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex3), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex1)
  putStrLn $ "depth ex3, the original: "
  putStrLn $ show (depth ex1)
  putStrLn $ "the translation is not the identity because information is lost, because of choices when converting back"
  putStrLn $ "This loss of information leads to a different depth"
  putStrLn $ "\nfor ex2"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex2), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex2)
  putStrLn $ "depth ex2, the original: "
  putStrLn $ show (depth ex2)
  putStrLn $ "the translation is not the identity because information is lost, because of choices when converting back"
  putStrLn $ "This loss of information leads to a different depth"
  putStrLn $ "\nfor ex3"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex3), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex3)
  putStrLn $ "depth ex3, the original: "
  putStrLn $ show (depth ex3)
  putStrLn $ "the translation is not the identity because information is lost, because of choices when converting back"
  putStrLn $ "This loss of information leads to a different depth"
  putStrLn $ "\nfor ex4"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex4), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex4)
  putStrLn $ "depth ex4, the original: "
  putStrLn $ show (depth ex4)
  putStrLn $ "In this case there are no choices that are needed when going back. No information is lost for ex4"
  putStrLn $ "\nfor ex5"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex5), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex5)
  putStrLn $ "depth ex5, the original: "
  putStrLn $ show (depth ex5)
  putStrLn $ "In this case there are no choices that are needed when going back. No information is lost for ex5"
  putStrLn $ "\nfor ex6"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex6), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex6)
  putStrLn $ "depth ex6, the original: "
  putStrLn $ show (depth ex6)
  putStrLn $ "In this case there are no choices that are needed when going back. No information is lost for ex6"
  putStrLn $ "\nfor ex7"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex7), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex7)
  putStrLn $ "depth ex7, the original: "
  putStrLn $ show (depth ex7)
  putStrLn $ "the translation is not the identity because information is lost, because of choices when converting back"
  putStrLn $ "This loss of information leads to a different depth"
  putStrLn $ "\nfor ex8"
  putStrLn $ "(depth $ toBoolExpr $ asBE ex8), converting and then going back: "
  putStrLn $ show (depth $ toBoolExpr $ asBE ex8)
  putStrLn $ "depth ex8, the original: "
  putStrLn $ show (depth ex8)
  putStrLn $ "In this case there are no choices that are needed when going back. No information is lost for ex8"
  quickCheck prop_depth1
  quickCheck prop_depth2
  quickCheck prop_depth3
  quickCheck prop_depth4
  quickCheck prop_depth5
  quickCheck prop_depth6
  quickCheck prop_depth7
  quickCheck prop_depth8

main :: IO ()
main = do
  putStrLn $ "for Q1"
  q_1
  putStrLn $ "\nfor Q2"
  q_2
  putStrLn $ "\nfor Q3"
  q_3
  putStrLn $ "\nfor Q5"
  q_5
  putStrLn $ "\nfor Q4"
  q_4
  putStrLn $ "\nfor Q6"
  q_6
------------------------------------------------------------------------
-- Bonus questions
--

-- Bonus 1: implement a (potentially failing) evaluator from
-- a Valuation (an assignment of Bool to variables).

-- Use the following definitions:
type Valuation = Map.Map String Bool

newtype NotFound = VarNotFound String

newtype Eval = Ev { val :: ExceptT NotFound (Reader Valuation) Bool}

-- Hint: ask, liftEither, sequence, Data.Foldable.and, and monads
instance BoolExpr Eval where
  --varB s      = Ev {val = ExceptT  (ReaderT (Map.fromList [(s, True)]) (Either (VarNotFound s)  True))}
  --notB r      = Ev {val = 1 + depth r}
  --orB []      = Ev {val = 1}
  --orB (r:rs)  = Ev {val}
  --andB []     = Ev {val}
  --andB (r:rs) = Ev {val}
  
  --runReader (runExceptT $ val (varB "x" :: Eval)) (Map.fromList [("y", False), ("x", False), ("z", True)])

-- For the rest of the bonus questions, the 'tutorial' at
-- http://okmij.org/ftp/tagless-final/index.html#course-oxford
-- contains a *lot* of useful advice. The most relevant is in
-- section 2, but section 3 and 4 are full of fascinating stuff too!
--
-- Bonus 2: implement another "printer" like that of Pr but minimize
-- the number of () in the results. 
newtype Bonus_2 = Bonus_2 {bonus_2 :: String} deriving Show

instance BoolExpr Bonus_2 where
  varB s      = Bonus_2 { bonus_2 = "var \"" ++ s ++ "\"" }
  notB r      = Bonus_2 { bonus_2 = "notB (" ++ bonus_2 r ++ ")"}
  orB []      = Bonus_2 { bonus_2 = "orB []"}
  orB rs      = Bonus_2 { bonus_2 = "orB [" ++ (concat $ intersperse "," $ map bonus_2 rs) ++ "]"}
  andB []     = Bonus_2 { bonus_2 = "andB []"}
  andB rs     = Bonus_2 { bonus_2 = "andB [" ++ (concat $ intersperse "," $ map bonus_2 rs) ++ "]"}
