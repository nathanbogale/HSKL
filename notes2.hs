


-- -------------BINARY TREES ------------------
-- each nodes have exactly two children
-- data Tree a = Leaf a | Node (Tree a) (Tree a) --are recusively Trees, and children of nodes are trees again

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show  --the Tree datatype needs to be defined first 

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node l r) = flatten l ++ flatten r
--IN TERMINAL
--  tree1 = Node (Node (Leaf "Nate")(Leaf "Wine")) (Leaf "Bogale")
-- tree1  --gives  Node (Node (Leaf "Nate") (Leaf "Wine")) (Leaf "Bogale")

-- applying flatten
-- flatten tree1 --gives the list from left to right order'

--hiehgt of a tree, leaf has 0 height, node has hight of max from l and r child +1

treehieght :: Tree a -> Int
treehieght (Leaf _) = 0
treehieght (Node l r) = 1 + ((treehieght l) `max` (treehieght r))
--treehieght tree1 --gives 2

--          .
--         / \
--        .   Bogale
--       / \
--   Nate  Wine
 
-- chekc the most left tree (Nate) whch has height 1, Wine and Bogale are height 0 +1 gives 2 


----------------- Expressions--------------------
data Expr =
    Lit Int --of type litteral number extendint integers
  | Add Expr Expr  --addition taking 2 arguments
  | Neg Expr --negation, giving back the opposite of negative forma
  | IfZero Expr Expr Expr --checking if the expr is of 0 value 
  deriving Show

expr1 :: Expr
expr1 = IfZero (Add(Lit 3) (Neg (Lit 3)))
               (Lit 43)
               (Add (Lit 3) (Lit 2))

expr2 :: Expr
expr2 = IfZero (Lit 3)
               (Lit 43)
               (Add (Lit 3) (Lit 3))

eval :: Expr -> Int 
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Neg x) = - (eval x)
-- eval (IfZero x y z) = if eval x == 0 then eval y else eval z --re-writing this line with guards
eval (IfZero x y z)
  | eval x == 0 = eval y
  | otherwise = eval z

--IN TERMINAL
-- eval expr 1 and eval expr 2 --gives 43 and 5



--  ------------------------MODULES---------------------- --
-- if a standalone program is written it must have a mudule called main


  -- module MyModule where --has to be the first line 
                          -- if module declaration is missed, the default module is Main
                          -- can have an export list, to ristrict what can be exported

  -- import something,something   --comes right after moduled definitions
                                  -- module called Perlude is imported if no import defiend 
                                  -- can restrict importing by using 'hiding' key siply saying everything eexcept the one in 'hiding ()'
                                  -- imported modules are not automatically exportable or accisible by others, they have to be defined in the exort list to be so

-- package is a collection of modules, the default packages is called base (has perlude and more) and automactically available 
-- Hackage is the main package repo for haskel, all available modules foudn there

--cabal -is a haskel pacakage format and a tool/library for package management and building packages 
-- in a cabal.project file, things like ghc version, dependencies, and other libraries included are stored there
-- commands like cabal build, cabal update, cabal freeze (to show a list of used things), cabal init (to initialize the cabal project files)

--use Haddock for proper commenting and documentaation

--use Hoogle is a searchable documentation for haskell



-- -------------------------- IO ----------------------------
--  data IO a --abstract type, and returns something of type a
-- main :: IO ()  --is the only thing that can execute it !
-- data () = () --special syntaxt, the () is called unit , returns unit too

-- HELLO WORLD EXAMPLE IN SCRIPTS


