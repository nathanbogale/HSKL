
{-

To launch .hs scripts:  

Ghci 
:l filename.hs 

 

-- Basic -- 

--comment 

 

{- 

Multiline comment 

-} 

functionName  variable = variable * 2   --body of function comes after =, and name or head before = 

 

Types: 

myexpression :: MyType  --if MyType was a type like Int, Float, Bool.. 

:type or :t in GHCI with expression tells type ( :t True    Bool or :t 'a' char) 

function :: Int -> Int  --function takes int as input and gives int as output  --this line is a signature   --the last ->int is the output 

function f = f*f  --this is the definition of the funtion 

Name :: String    --representing the function type 

Name = "Bob" 

Infix and Prefix: 

Prefix: function comes before argument 

function x y = x * y 

Infix: function with in the arg 

3 + 1 -- + is the function 

Mixing: (+) 3 0 

4 'function' 5 

 

Data types: 

Int is limited to ±263 - Use Integer for bigger numbers – but used more power  

Doouble is more precise than float – but takes more processing power 

In Char, a single word 'hi' can nly be written 

List is ['a','b']  --can have one type only 

Strng reps. List of chars. And "Helooooo" or as ['H','e',..]  

Tuple can store elements of diff type and have fixed size, reped.  ('a', 3, True) 

Ploymorphic typs: that have multiple types 

function :: (a,b) -> a    --on signature --specifying the output is of type a  --inbuilt haskle fuction – fst(x,y) gives x, snd(x,y) gives y 

function (x,y)=x    --on definition—telling output is x 

funtion ('a',3)    --gonna give a 

LISTS: 

Head and tail 

On a list [1,2,3], head list gives the first value 

While tail list gives all but the first value 

Indexs in list: use !! To call by index from list 

[1,2,3] !! 2 gives out 3 

"ABC" !! 0 gives 'A' 

[3,5..22] generate all number in the list till 22, works on alphabet too 

Take value generate the specifiedd amt 

Take 4 [1,3..]  gives off [1,3,5,7] 

To prepend use : 

2: [3,4,5] gives [2,3,4,5] 

Join two lists by ++ 

[2,3,4,5] ++ [8,9,0] gives [2,3,4,5,8,9,0] 

Length list give te length 

Null list checks if list is null 

Sum list will add everythin in list 

5 elem list checks if 5 is inside the list  

 

-- Conditions and helpers --

If <> then <> else<> 

GUARDS For multi layr conditions 

 
------------------------------------------------------------------------------------------------------------------------------------------

-- HIGER ORDER FUNCTIONS -- 
Are functions that take other fucntions as an input or give off as a result, and assigne them to variables


cfunc :: Int -> Int
cfunc x = x + 1

func1 :: Int -> Int
func1 x = cfunc (cfunc x)

func2 :: Int -> Int
func2 x = (cfunc(cfunc x)) + (cfunc(cfunc x)) 

-- the above can be shortned by, a higher order function
applytwice :: (a -> a) -> a -> a
-- in the header of the function:
-- the first part (a -> a) means, the first part is a function, taking a value of type 'a' and returning value of type 'a'
-- the second part, a -> a, means the input is of type 'a' and applytwice returns a value of type 'a' 

applytwice f x = f(f x)
-- in the body:
-- the first parameter is a function 'f', applies it to the second parameter 'x' in (f x) and then applies the function again to the result by f(f x)

func3 :: Int -> Int --alternative of func1
func3 x = applytwice cfunc x

func4 :: Int -> Int  --alternate of func 2
func4 x = (applytwice cfunc x) + (applytwice cfunc x)

------------------------------------------------------------------------------------------------------------------------------------------

-}
-- FILTER HIGHER ORDER FUNCTIONS -- an internal haskel function
      -- :t filter
      -- filter :: (a -> Bool) -> [a] -> [a]  -- takes a predicate / a function (a->bool) that returns a bool, and a list of elements of type 'a' and filters the elements of the list given the predicate

      --filter even [1..20] --to filter out even numbers only, from 1 to 20, here 'even' is the predicate

      --to create a filter that only returns values with lower case 'a'
--filterA = filter tempFunc ["apple","mango","pine","vodk"]
--            where tempFunc x='a' `elem` x
      --then call with filterA
      --`elem` x means for all the eleemtns in each value, where value is every letter in the words in the list 


-- ANY HOFs --- selecting any of the values where the predicate holds
    -- :t any
    --any :: Foldable t => (a -> Bool) -> t a -> Bool  -- uses predicates
    -- ex. to check if any value in a list is greater than 4
    -- IN TERMINAL
-- greaterthan4 x = x > 4
-- any greaterthan4 [1,2,3,4]
      -- should give false

      --ex. to check if there are any cars left
      --IN TERMINAL
--cars = [("TOyota",3),("Nissan",1),("BMW",0)]  --this is a Tuple, or a pair with in the list 
--checkcars (_,x) = x > 0  --using pattern matching to skip the car names and only count the amount
--any checkcars cars 
      --should return True


-- LAMBDA FUNCTIONS -- are anonymous functions, that doenst have names -- used to avoid naming for func only used once
          -- ex.a lambda function takes 2 args and multiplies them
          -- \x y -> x * y    --the '\' tells that its a lambda func. then comes args then after arrow the body

          -- the greaterthan4 in lambda
          -- IN TERMINAL
          -- any (\x -> x > 4) [1,2,3,4]

          --the fruit example in lambda
          --IN TERMINAL
          --filter (\x -> 'a' `elem` x) ["apple","mango","pine","vodk"]

          --lambda on its own
          --(\x -> x * 2 + 1 ) 3  --here 3 is the input



---  PRECEDENCE AND ASSOCIATIVITY  ---
-- PRESEDENCE : indicates the priority of an operator, denoted by 0 to 9, one with higher operators gets applied first
-- :i(+) gives infixl 6 and multiplication 7, multiplication has a higher presidence, infixl are called fixity declarations
-- if two operators have the same presidence, associativity tells which side will be evaluated first, based on infixl or infixr
  -- 1+2+3+4 -- infixl: same as ((1+2)+3)+4 - meaning addition evaluates the left side first
  --1:2:3:[] -- infixr: same as 1:(2:(3:[])) --evaluates the right side first 
  --True==(False==False) -- infix: no sides, needs parenthesis to give focus, if not error
  --CREATING A NEW OPERATOR:
    -- x+++y = x+y
    -- infixl 7 +++  --setting the infix to left
    -- 1 +++ 2 * 3  -- gives 9, cause got same presedence as multiplication

    -- the whitespace operator has the highest left associative presidence 


-- CURRIED FUNCTION : is proces of changing a function to accept a single input than multiple, and returns a function that accepts a second input and so on
-- all functions in haskell are considered curried

--add3 :: Int -> (Int -> (Int -> Int))  -- same as: add :: Int -> Int -> Int -> Int 
-- ((add x)y)z = x + y + z              -- same as: add x y z = x + y + z
-- the above re-written in lambda
-- (add x) y = \z -> x + y + z   --add taking 2 arg, and giving back a lambda func. 'z' that takes another arg
-- add x = \y -> (\z -> x + y + z)  -- add taking 1 arg 'x', giving back a func. 'y' taking one number y, and giving back out 'z' which takes another number z and calcualtes
-- add = \x -> (\y -> (\z -> x + y + z))
    -- since the arrow '->' is right assocaitive we can remove the useless paranthesis in the signature and definition for a cleaner code
--add3 = \x -> \y -> \z -> x + y + z
--error if provided fewer arg. than needed



-- PARTIAL APPLICATIONS: providing fewer args than the one the function sets
-- order of the parameters matter 
-- ex. creating email with domain, name and last name, but providing ontlt domain in the function, and taking the two as an arg

createEmail :: String -> String -> String -> String
createEmail domain name lname = name++"."++lname++"@"++domain

create1 :: String -> String -> String 
create1 = createEmail "google.com"   --this is partially applied 

create2 :: String -> String -> String 
create2 = createEmail "yahoo.com"

--create1 "nathan" "bogale"
--"nathan.bogale@google.com"

-- Partially applying on this example: any (\x -> x > 4) [1,2,3,4]
-- any (>4) [1,2,3,4]  --gives fales

--(++ "ing") "Think"  --same as \x -> x ++"ing" --partially applying on the left side  --Thinking
--("anti"++) "Socail"  --same as \x -> "Anti"++x  --partially applying on the right   --Antisocial

      --CANNOT PARTIALLY APPLY ON A MINUS OPERATOR



--  APPLYTING AND COMPOSING FUNCTIONS ----
--the function application operator '$'  --its primary use is to omit paranthesis
-- the function operator has the lowest right associative presidence  -- infixr 0 $

--ex.
{-

(2*) 3 + 4  --same as ((2*)3)+4 --because the whitespace between (2*) 3 exectes the left side first  --10
(2*) $ 3 + 4 --same as (2*)(3+4) --the right side of '$' executes first the 2 is multiplied    --14
max 5 4 * 2  --same as ((max 5)4) *2  -- 7
max 5 $ 4 * 2 --same as (max 5)(4 + 2) --6

-}

--omitting paranthesis by
--show ((2**)(max 3(2 + 2)))   --16
--show $ (2**) $ max 3 $ 2 + 2  --16



--FUNCTION COMPOSITION-- uses the dot (.) operator
--when composing 2 func, produces a new func that is equivalent to calling the 2 funcs in sequence where the first one takes as input the output of the second one 
-- f(g x) --f taking the outout of g as an input

complicatedF :: [Int] -> Bool
--complicatedF x = any even (filter(>25)(tail(take 10 x)))  --takes 10 from the list, takes the last 9 (with tail) filters which is >25 and chooses even and gives bool answer

-- can be simplified by abstrating function compostion to an operator (.)
-- takes 2 func. f of type b to c and g of type a to b
-- f takes an input of type of g's output which is b
-- the value has an input of type g which is a and an output of type f which is c
{-
(.) :: (b -> c)-(a -> b) -> a -> c
f . g = \x -> f (g x)
infixr 9 .
-}
--so the above example in composition looks liek the below
complicatedF x = any even . filter(>25) . tail . take 10 $ x


-- POINT-FREE STYLE -- tacit programming, function definitions donot declare the argument 
    -- to avoid redundancy 
    -- so instead of this
--add1 :: Int -> Int
--add1 x = 1 + x -- declaring x and using it in the body

--forurorlarger :: Int -> Int
--forurorlarger x = max 4 x

    --can do something like the below
--add1 = (1+)
--forurorlarger = max 4
    --and this can change to
--complicatedF = any even . filter(>25) . tail . take 10  -removed x from both sides









-------- /*  LESSON SIX  */ ----------

---- RECURSION AND FOLDS
-- have to defiene type of list
summer :: [Int] -> Int
summer [] = 0  --with out this, it will thrown an error, its because it needs the final product of the recurstion - look at vid if u dont rememenr this
    -- summer [] = 1 -- for multiplicaition, since it'll multiply by 0, all will give 0, so change the empty list to 1 like here 
summer (x:xs) = x + summer xs  --here,x is the first entity in the list, while xs is the rest of the list
 -- summer [1,2,3,4,5] gives 15  --proceesing it as 1+2+3+4+5+0 -- zero being the empt list




-- DEFINING OWN RECURRSIONS
--the below functions are already a part of haskel functions so using '
and' :: [Bool] -> Bool  --and' will take a list of boolean adn give of a singel bool result
and' [] = True  --since its an and function, and is all is not true it'll give false
and' (x:xs) = x && and' xs
-- and'[True, False] --gives false as it should, cause all aint true
-- and' [2==2, 4>5] --will give fales


--a function to calculate the length of a list
length' :: [a] -> Int  --the a here can be anything, just not empty, since its only going to calculate the length of the entry, can be polymorphic
length' [] = 0
--length' (x:xs) = 1 + length' xs 
--above, since we're not using x in the couting, we can omit it with patern matching
length' (_:xs) = 1 + length' xs
--length' [1,2,23,2,4] --gives 5
--length' ['a'..'z'] --gives 26

--reverese function, that reverses the entity of the list 
reverese' :: [a] -> [a]
reverese' []=[]
reverese' (x:xs) = reverese' xs ++ [x]
--reverese' [1,2,3,4,5] --gives [5,4,3,2,1]
--reverese' "Nathan" --gives "nahtaN"

--the drop fucntion, to remove entities from list based on their position, not exactly index where it starts from 0
drop' :: Int -> [a] -> [a]
-- now for cases exist
drop' _ [] = [] --to drop 0 from an empty list -- should give the empty list itself --and with paternmatching, can be any index, just gonna give the empty lists itself
drop' n xs | n <=0 = xs --to drop an entity from empty list --either error or empty list
--what happened above is, making sure n is not less than 0, | is a guard, sorta lif an if condition in above note
drop' 0 (x:xs) = xs --to drop 0 from the list  --shoudl give the list itself back
--drop' n (x:xs)= drop' (n-1) xs --to drop entity from actual list
--with patern matching, since not using x above
drop' n (_:xs) = drop' (n - 1) xs
--drop' (2) [1,2,3,4] and drop' 2 [1,2,3,4]  --are same, gives off [3,4]
--drop' (-2) [1,2,3,4] --gives the list itself, since handled negative entries

ido :: String -> String
ido = ("I DO "++).drop' 10 
-- ido "i dont do THAT!" --gives "I DO THAT"

ido2 :: Int -> String -> String
ido2 nn = drop' (nn-1) 
-- ido2 (11) "i dont do THAT!" --gives "THAT"


--the take function, takes the first n elements of a list 
take' :: Int -> [a] -> [a]
-- take' 0 []= [] --simplified as below
take' _ [] = []
take' n _ | n <= 0 = []
take' n (x:xs) = x:take' (n - 1) xs   --prefixed x here
-- take' 4 "someone new" --gives "some"


------------ RECURSIONS THAT TAKE FUNCTIONS AS AN ARGUMENT ------------------------
--- map is a HOF, that applies a function to every element in the list 
map' :: (a -> b) -> [a] -> [b] --(a-b) is a function that takes a and gives b, also taking a list of [a] and giving out a list of [b]
map' _ [] = []  --this is called the base, because its th edefualt response
map' f (x:xs) = f x: map' f xs  --here, it applies function f on x and prependes it to map of function f on xs

--map' (+2) [2,4,2,4] --gives [4,6,4,6]
--map'(++" !") ["NATHAN","COME","HERE"] --gives ["NATHAN !","COME !","HERE !"]


--filter function, filters the elemnts of the list that diesnt satisfy that predicate 
filter' :: (a -> Bool) -> [a] -> [a] --giving out the same result type [b] because it only does filter and doesnt change the value --the predicate here is the fun. that takes a and returns a boolean
filter' _ [] = []
filter' p (x:xs)  --below, using guards insead of if statements 
    | p x =  x: filter' p xs --if predicate satisfied by element, applied to the first element, then recursively apply to the other elemetnts or xs
    | otherwise = filter' p xs --not satisfied, then apply to the rest of the element except x
--filter' (==True) [True, False, True] --gives [True, True]
--filter' (==2) [1+1, 2-1, 4-2]  --gives [2,2]
--filter' ('!' `elem`) ["Hey!", "Hi"] --gives ["Hey!"]
--filter' (\x -> x**2 < 40) [1,2,3,4,5,6,7,8,9,10]  --gives [1.0,2.0,3.0,4.0,5.0,6.0]





-------------------- PATTERNS -----------------
--using the patterns from the recursive functions and modifying

----in the Sum function
--WAS  --summer (x:xs) = x + summer xs
--NOW --summer (x:xs) = (+) x (summer xs)  --defining the function as a prefix

----in the Multiplication function
--WAS  --product (x:xs) = x + product xs
--NOW --product (x:xs) = (*) x (product xs)  --defining the function as a prefix

----in the AND function
--WAS  --and (x:xs) = x + and xs
--NOW --and (x:xs) = (&&) x (and xs)  --defining the function as a prefix


---to abstract the patterns in the function, and simplify it and extract the premitive recursion pattern
fodlr :: (a -> b -> b) -> b -> [a] -> b -- starts with the function, taking a and b
                                        -- the 3rd b is the type of the based value
                                        -- and [a] is the list entity type
                                        -- and the final b is the output
fodlr _ v [] = v --the base value
fodlr f v (x:xs) = f x (fodlr f v xs) --function combining value and recursion
-- fodlr (+) 0 [1,2,3,4] --gives 10
-- fodlr (*) 1 [1,2,3,4] --gives 24
-- fodlr (&&) True [True,False] --gives False


--to simplify it in the main and prev. functions
summer' :: [Int] -> Int 
summer' = fodlr (+) 0 --pattially applying foldlr
-- summer' [1,2,3,4] --gives 10
-- same applies for multiplication and boolean
-- foldr already ccomes in Haskel as a package


---REPLACEING LENGTH with foldr
-- length' (_:xs) = 1 + (length' xs) --initial approach
-- length' (_:xs) = (+) 1 (length' xs) --prefixing the operator/function
-- length' (_:xs) = (\_ n -> 1 + n) x (length' xs)  --substituting it with lambda function --ignoring the first entity
-- length' (_:xs) = foldr (\_ n -> 1 + n) 0  --replacing it with the foldr function

--REPLACING REVERSE
-- reverese' = fodlr (\x xs -> xs ++ [x]) []

----- THE foldl function
--same as foildr, but traverses the list from left to right 
-- Replacing REVERSE
--reverese' = foldl (\x y -> y:x) [] --same as foldl (flip(:)) []  --uses semicollons : instead of concat operator ++
--foldl depends on the operator, like is different value from foldr on substraction

--foldl' is more efficent , while foldl throws stack exception, foldl' process is, needs importing dependencies
--check when to use foldr, foldl and foldl'











-------- /*  LESSON SEVEN  */ ----------

----------------TYPE CLASSES-------------
--Polymorphic value types offer flexibilty while losing safety -- Something :: (a -> b) -> b
--Type offer a lot of safetly and a bit of flexibility -- something :: Int -> Int
--Type classes combine both, use restrictive polymorphic values or ad-hoc polymorphism or overloading 


--Common type classes (in-built)

    -- the Eq typeclass: 
        --uses the == and /=, to basically check equality, returning boolean
        -- takes all as an input, EXCEPT FUNCTION
        -- (==) or (/=) :: Eq a => a -> a -> Bool
        --ex:
            -- func :: Eq a => a -> a -> a  --the fat arrow => tells compiler that the inputs are constrained or are instance of the typeclass
            -- func x y = if x = y then x else y
        
    -- the Ord typeclass
        -- is all about ordering, the types of the instances of Ord can order their values and say which value is the biggest 
        -- orders with nomerical and alphabetical ordering 
        -- uses >,<,>=,<= and min, max and compare

    -- the Num types or numberic 
        -- these types act as numbers, can also be operators, but encapsulates all numbers like Int, float...

    --the Integral typeclass
        --can aonly include whole numbers, like 4 and not 4.3

    --the Fractional typeclass
        --only representing and modifyong fractional numbers 

    --the Show typeclass
        --converting to a string representation of the value 

    --the REad typeclass
        --opossite of show
        --takes String and returns the value

    --multiple typeclasses
        -- have to be surrounded with paranthesesi and devided by comma like a tupple
        -- skip :: (Eq p, Num p) => p -> p 









-------- /*  LESSON EIGHT  */ ----------


----------------NON PARAMETRIZED TYPES-------------

-- instead of using somethig like
--generateTx :: String -> String -> Int -> String
--using 
--generateTx :; Address -> Address -> value -> Id 
--by using type synonyms to assign a name to the signature , THIS IS NOT CREATING NEW TYPE

--type Name = String
--type Address  = (String, Int)
--type Person = (Name, Address) --setting a person type is a tuple of name and add

--nate = ("Nathan Bogale", ("Addis Abeba",555)) :: Person --tellign that nate is of a person type

-- *Main> :t nate
-- nate :: Person



-----------TO CREATE NEW TYPES USE DATA
-- data Color = REd | Green | Blue   --both type name and value constructos must start with UPPERCASE 

type Name = String
type Address  = (String, Int)

data PaymentMethod = Cash | Card | Check

type Person = (Name, Address, PaymentMethod)

nate = ("Nathan Bogale",("Addis", 2254586), Card) :: Person

--TERMINAL
-- *Main> nate
-- ("Nathan Bogale",("Addis",2254586))

--using patern matching
paysWith :: Person -> String 
paysWith (_, _, Cash) = "PAYS WITH CASH"
paysWith (_, _, Card) = "PAYS WITH CASRD"
paysWith (_, _, Check) = "PAYS WITH CHECK"

-- TERMINAL
-- *Main> paysWith nate
-- "PAYS WITH CASRD"




--------------VALUE PARAMETERS

data Shape = Circle Float | Rectangle Float Float --rectangle is a funcion taking 2 floats as inpt, and gives a value of type shape
rect1 = Rectangle 2 4
circ1 = Circle 5

-- *Main> :t rect1
-- rect1 :: Shape

area::Shape -> Float
area (Circle r) = pi * r * 2
area (Rectangle l1 l2) = l1 * l2

-- *Main> area rect1
-- 8.0
-- *Main> area circ1
-- 31.415928




-----------RECORD SYNTAX --alternative of definin datatpes
--withoutrecord syntax
-- data Employee = Employee String Float
--With
data Employee = Employee {name::String, expirence:: Float} deriving (Show) --assingin it to the show data type, can be seen in console

nathan = Employee {name="Nathan Boglae", expirence=15.5}  --another way below
bogale = Employee "Bogale" 8

-- *Main> bogale
-- Employee {name = "Bogale", expirence = 8.0}


team = [Employee "One" 5, Employee "Two" 10, Employee "Three" 4.5]
combinedExp :: [Employee] -> Float
combinedExp = foldr (\e acc -> expirence e + acc ) 0

-- *Main> combinedExp team
-- 19.5







-------- /*  LESSON NINE  */ ----------


----------------CREATING PARAMETEIZED RECURSIVE TYPES-------------

type Name1 = String
type Address1 = (String, Int)
type CompanyId = Int

--by creating a parametrized type constructor

type Entity a = (a, Address1,CompanyId)

nate1 = ("Nathan Bogale", ("Addis", 555), 123) :: Entity Name1
office1 = (123, ("addis", 555),123) :: Entity CompanyId

-- *Main> office1
-- (123,("addis",555),123)





------------- parametrising data type

data Box a = Empty | Has a deriving (Show)

-- box = Has (1 :: Int)  --here box already has 1

addN :: Num a => Box a -> Box a -> Box a --takign 2 boxed as an input and returning one as output

addN _ Empty = Empty  --if second box is empty return empty
addN Empty _ = Empty

addN (Has a) (Has b)= Has (a + b)


-- *Main> addN (Has 2) (Has 4)
-- Has 6
-- *Main> addN (Has 2) Empty
-- Empty

extract :: a -> Box a -> a
extract def Empty = def  --if box is empty return the default !!
extract _ (Has x) = x


-- *Main> extract 0 Empty
-- 0
-- *Main> extract 0 (Has 5)
-- 5
-- *Main> extract 'a' Empty
-- 'a'
-- *Main> extract [] (Has [1,2,3])
-- [1,2,3]





------------------RECURSIVE DATA TYPES
data Tweet = Tweet {
    contents :: String,
    likes :: Int,
    comments :: [Tweet]   --each comment is another tweet
}deriving (Show)


tweet :: Tweet
tweet = Tweet "Hey i'm famous!" 5 
    [
        Tweet "Me TOO!" 0 [],
        Tweet "I'm not!" 3 [
            Tweet "So wahat??" 2 []
        ]
    ]

engagement :: Tweet -> Int
engagement Tweet {likes = l, comments = c} = l + length c + sum (map engagement c)  --patern matching likes and commment and adding likes and amount of comment 

-- *Main> engagement tweet
-- 13





-----------------------BINARY SEARCH TREE
--each child's of nodes on the left are smaller than the node, while all childern on the right on the node are larger than the node

data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving (Show)  --checking on the left and right of the tree, hhas 2 constructors the empty and the node

emptyTree :: Tree a
emptyTree = EmptyT

oneLevelTree :: Tree Char
oneLevelTree = Node 'a' EmptyT EmptyT

twoLevelTree :: Tree Integer
twoLevelTree = Node 8
    (Node 3 EmptyT EmptyT)
    (Node 10 EmptyT EmptyT)

threeLevelTree :: Tree Integer
threeLevelTree = Node 10
    (Node 8 
        (Node 2 EmptyT EmptyT) 
        EmptyT
    )
    (Node 12 
        EmptyT 
        (Node 15 EmptyT EmptyT)
    )

-- *Main> threeLevelTree
-- Node 10 (Node 8 (Node 2 EmptyT EmptyT) EmptyT) (Node 12 EmptyT (Node 15 EmptyT EmptyT))

--checker, checking if the value is with in the list or tree
elemTree :: (Ord a) => a -> Tree a -> Bool 
elemTree v EmptyT = False  -- if empty then false
elemTree v (Node x left right)
    | v == x = True
    | v > x = elemTree v right  --recursively check the right side
    | v< x = elemTree v left

-- *Main> elemTree 10 threeLevelTree
-- True
-- *Main> elemTree 11 threeLevelTree
-- False



-- to check the type of a type constructor like Box, dont use :t use :i or info


-----------newType, types created with this eneed to have exactly one constructor with exxactly ne parameter/field
-- newType Color a = Color a
-- newType Poduct a = Product { getProduct :: a}











