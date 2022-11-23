-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).


{-
calculator :: Int -> Int -> String

calculator hu du
  | monthCalc > monthlyMax = "OVERUSED"
  | monthCalc == monthlyMax = "ON POINT"
  | monthCalc < monthlyMax = "UNDER USED"
  | True = "Nothings special"
  where
    monthCalc = hu * du * 30 
    monthlyMax = 1000
    -}

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 

{-
--Patern Matching
birthday :: Int -> [Char]
birthday 1 = "First"
birthday 18 = "Adult"
birthday 60 = "Old"
birthday age = "Must be good, and " ++ show age  --to capture out of range entries, should be at the bottom --catch all
-}

--pattern matchin a list
{-
whatsinthelist :: [Int]->String
whatsinthelist [] = "Its Empty"
whatsinthelist [x] = "Has a single element "++show x
whatsinthelist [x,y] = "Has two elements "++show x ++" and "++ show y
whatsinthelist (x:y:z:[]) = "The list has tree elements " ++show [x,y,z]
whatsinthelist (x:rest) = "The first element is: "++show x++" and there are a few more"

--skippng entities in the list
firstandthird :: [Bool]->String
firstandthird (x:_:z:_) = "the first and third elemts are: "++show x ++" and "++show z  --and ignoting the rest
firstandthird _ = "Dont have them"      --on any other pattern

firstandthird [True,False,Falsee]


initials :: String -> String -> String 
initials (f:_) (l:_) = [f]++ "."++[l]++"."  --selecting the first letters - can also be done by head
initials _ _ = "How was your name again?"


---tuple pattern matching
secondandfourth :: (a,b,c,d) -> (b,d)
secondandfourth (_,x,_,y) = (x,y)

secondandfourth  (1,2,3,4)



--CASE
checkforzeros :: (Int, Int, Int) -> String
--checkforzeros check = case check of                                             -- thhis works too
checkforzeros check = "The " ++show check ++ " has zero on.. " ++   
  case check of                                                   
  (0,_,_) -> "The first element"
  (_,0,_) -> "The second element"
  (_,_,0) -> "The third element"
  _       -> "we're good"

checkforzeros (2,0,2)
-}
