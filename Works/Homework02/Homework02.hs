
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

{-
f2 :: Double -> Double -> Double -> Double 
f2 x y z = x ** (y/z)

-- f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

-- f4 x y z = x == (y ++ z)

-}
-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?


-- Question 3
-- Why should you define type signatures for variables? How can they help you?


-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?

--listOfNames :: [[[Char]]]
--listOfNames = [["Andre", "Angelo"], ["Bruno", "Bianca"], ["Carlos", "Claudia"]]

{-

checkname :: String -> String
checkname name =
  if name == "nate"
    then "welcome nate"
    else "who are yu"

checkname "natee"

birthday :: Int -> String  --works too

birthday :: Int -> [Char]
birthday age
  | age == 1 = "First"
  | age == 18 = "adult"
  | age == 60 = "old"
  | True = "Nothings special"

birthday 60


hotterInKelvin' :: Double -> Double -> Double
hotterInKelvin' c f =
  let fToC t = (t - 32) * 5 / 9
      cToK t = t + 273.16
      fToK t = cToK (fToC t)
   in if c > fToC f then cToK c else fToK f

hotterInKelvin' 40 100


hotterInKelvin'' :: Double -> Double -> Double
hotterInKelvin'' c f = if c > fToC f then cToK c else fToK f
  where
    fToC t = (t - 32) * 5 / 9
    cToK t = t + 273.16
    fToK t = cToK (fToC t)

hotterInKelvin'' 40 100


--these two work in the console directly 
let s1 = 10; s2 = 20; s3 = 30; in s1*s2*s3 

24 * (let seconds = 60 in seconds * 60) 
-}

greeter :: String -> String -> String 
greeter fname lname =
  if fname == "" || lname == ""
  then "Can you tell me your name again.."
   else let welcome = "welcoem " ++ fname ++ " " ++lname++" ! "
            plan = " What do you plan to do today? Mr." ++ fname
        in welcome ++ plan