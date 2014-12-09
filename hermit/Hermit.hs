{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Hermit where


--import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Monoid
import Graphics.Storyboard

import Utils

slides :: [Slide ()]
slides =
 [ title "Remote Shell for our Haskell compiler?" $ fontSize 28 $ do
     ul $ do
        li $ "There is often a trade-off between the clarity and efficiency of a program."
        li $ "Useful to transform a clear program (specification) into an efficient program (implementation)."
        li $ "This idiom has many instantiations: faster code; using a different interface; space usage; semi-formal verification."
        li $ "We want to mechanize such transformations on Haskell programs:"
        ul $ do
          li $ "Less time-consuming and error prone than pen-and-paper reasoning"
          li $ "No need to modify the source file"
          li $ "Several existing transformation systems for Haskell programs, e.g. HaRe, HERA, PATH, Ultra."
          li $ "They all operate on Haskell source code."
        li $ b $ "We take a different approach, and provide commands to transforming" <+> 
                   color "red" "GHC Core" <> ", GHC's intermediate language."
 , title "Demonstration: Unrolling Fibonacci" $ fontSize 24 $ do
   p $ "As a first demonstration, let's transform the" <+> q"fib" <+> "function by unrolling the" <+>
        "recursive calls once."

   fontSize 20 $ codeBox hs $
        [ "fib :: Int -> Int"
        , "fib n = if  n < 2"
        , "        then 1"
        , "        else fib (n-1) + fib (n-2)"
        ]

   pause

   p $ "after unrolling..."

   fontSize 20 $ codeBox hs $
         [ "fib :: Int -> Int"
	 , "fib n = if  n < 2  then 1"
	 , "                   else  (if  (n-1) < 2  then 1"
	 , "                                         else fib (n-1-1) + fib (n-1-2)"
	 , "                         )"
	 , "                         +"
	 , "                         (if  (n-2) < 2  then 1"
	 , "                                         else fib (n-2-1) + fib (n-2-2)"
	 , "                         )"
         ] 

 , title "First Demonstration" $ fontSize 28 $ do

   let s1 = ul $ do 
              li $ q "resume"
              p $ "resume the compile"

              li $ q "binding-of 'main"
              p $ "goto the" <+> q"main" <+> "definition"

              li $ q "binding-of 'fib"
              p $  "goto the" <+> q"fib" <+> "definition"
              
              li $ q"remember \"myfib\""
              p $ "remember a definition"

              li $ q"show-remembered"
              p $ "show what has been remembered"

   let s2 = ul $ do 
              li $ q"any-call (unfold-remembered \"myfib\")"
              p $ "try unfold" <+> quote "myfib"

              li $ q"bash"
              p $ "bash a syntax tree with simple rewrites"

              li $ q"top"
              p $ "go back to the top of the syntax tree"
      

              li $ q"load-and-run \"Fib.hss\""
              p $ "load and run a script"
   twocol s1 0.45 s2 0.55

 , title "What did we do?" $ fontSize 28 $ do

   p $ "HERMIT requires a recent GHC (I am using GHC 7.8.3)"

   vspace 50 

   ul $ do
     li $ q"cabal update"
     li $ q"cabal install hermit"
     li $ q"hermit Main.hs"

   vspace 50 

   p $ "The" <+> q"hermit" <+> "command just invokes GHC with some default flags:"


   fontSize 20 $ codeBox verb $
     [ "$ hermit Main.hs"
     , "ghc Main.hs -fforce-recomp -O2 -dcore-lint"
     , "            -fexpose-all-unfoldings"
     , "            -fsimple-list-literals -fplugin=HERMIT"
     , "            -fplugin-opt=HERMIT:main:Main:"
     ]


 ,title "HERMIT Use Cases" $ fontSize 24 $ do

  ul $ do
    li $ "We want to explore the use of the worker/wrapper transformation for program refinement"
    ul $ do
      li $ "We need mechanization to be able to scale the idea to larger examples"
      li $ "Medium-sized case study: Changing representations in the Conway's Game of Life"
      li $ "Large case study: Low Density Parity Checker (LDPC), transforming math equations into Kansas Lava programs"
    vspace 20
    li $ "HERMIT is for library writers"
    ul $ do
      li "Authors show equivalence between clear (specification) code, and efficient (exported) code."
    vspace 20
    li $ "HERMIT is a vehicle for prototyping GHC passes"
    ul $ do
      li $ "Optimization: Stream Fusion"
      li $ "Optimization: SYB"
      li $ "Staging: Translating Core into CCC combinators. (Elliott, et. al.)"
    vspace 20
    li $ "Scripting Haskell-based Equational Reasoning - Bird's Century"
    vspace 20   
    li $ "(Your project goes here)"

 ,title "HERMIT Architecture" $ fontSize 24 $ do

  p $ "We draw inspiration from UNIX and operating systems."

  h2 $ "Shell Level (UNIX Shell style commands)"

  ul $ do
    li $ "Dynamically typed, variable arguments"
    li $ "Help (man) for each command"
    li $ "Control flow commands (';', retry, etc.)"
  vspace 10

  h2 $ "Rewrite Level (UNIX man(2) system commands)"

  ul $ do
    li $ "Haskell functions, strongly typed"
    li $ "Think type ::" <+> q"CoreExpr" <+> rightarrow <+> q"M CoreExpr"
    li $ "Higher-order functions for tunneling into expressions"
    li $ "Many function tunnel into GHC (example:" <+> q"substExpr" <> ")"
    li $ "All GHC" <+> quote "RULES" <+> "are directly invokable."

  h2 $ "Stratego-style library for rewrites (DSL for rewrites)"

  ul $ do
    li $ "Haskell DSL call KURE"
    li $ "Basic idea: rewrites can succeed or fail"
    li $ "Higher-order combinators for search, catching fail, retry"

 , title "HERMIT Architecture Diagram" $ fontSize 24 $ do

   t1 <- scaledImageTile "hermit/images/HermitArch.png" 0.8
   place top $ nudge top center $ box defaultBoxStyle $ t1


 , title "HERMIT Commands" $ fontSize 28 $ do
   let s1 = do
         h2 "Core-specific rewrites, e.g."
         ul $ do
                 li $ "beta-reduce"
                 li $ "eta-expand 'x"
                 li $ "case-split 'x"
                 li $ "inline"

   let s2 = do
         h2 "Strategic traversal combinators (from KURE), e.g."
         ul $ do
                 li "any-td r"
                 li "repeat r"
                 li "innermost r"

   twocol s1 0.45 s2 0.55
   let s1 = do
         h2 "Navigation, e.g."
         ul $ do
           li "up, down, left, right, top"
           li "binding-of 'foo"
           li "app-fun, app-arg, let-body, ..."

   let s2 = do
         h2 "Version control, e.g."
         ul $ do
           li "log"
           li "back"
           li "step"
           li "save \"myscript.hss\""

   twocol s1 0.45 s2 0.55

 , title "Worker/Wrapper" $ fontSize 28 $ do

   leftMargin 100 $ rightMargin 100 $ frame $ align center $ p $ big $ 
     "The Worker/Wrapper Transformation is a rewrite technique" <+>
     "which changes the type of a (recursive) computation"

   vspace 20

   ul $ do
     li $ "Worker/wrapper has been used inside the Glasgow Haskell compiler since its inception to rewriting functions that use lifted values (thunks) into equivalent and more efficient functions that use unlifted values."
     li $ "Much, much more general that just exploiting strictness analysis."
     li $ "Worker/wrapper is about changing types."
     p $ "(We will apply worker/wrapper to the Game of Life.)"

   vspace 10

   h2 "Changing the type of a computation..."
   ul $ do
     li "is pervasive in functional programming"
     li "is useful in practice"
     li "is the essence of turning a specification into an implementation"

 , title "Example: Avoiding Needless Deconstruction" $ fontSize 28 $ do



   fontSize 20 $ codeBox' hs "Before" $
        [ "last       :: [a] -> a"
        , "last []       = error \"last: []\""
        , "last (x:[])   = x"
        , "last (x:xs)   = last xs"
        ]

   pause
   vspace 30

   fontSize 20 $ codeBox' hs "After" $
        [ "last []     = error \"last: []\""
        , "last (x:xs) = work x xs"
        , ""
        , "work :: a -> [a] -> a"
        , "work x []     = x"
        , "work x (y:ys) = work y ys"
        ]



 ] ++ [ title (prose $ "Worker/Wrapper on last (" ++ (show n) ++ ")") $ fontSize 28 $ do

   let o ns xs = if n `elem` ns then xs else map (const ' ') xs
   
   when (n `elem` [1..2]) $ do
     fontSize 20 $ codeBox hs $
        [ "last :: [a] -> a"
        , "last = " ++ o [2..4] "\\ v -> case v of"
        , o [2..4] "        []     -> error \"last: []\""
        , o [2..4] "        (x:xs) -> last_work x xs"
        , ""
        , o [2..4] "last_work :: a -> [a] -> a"
        , o [2..4] "last_work = \\ x xs ->"
        , o [2..4] "    (" ++ "\\ v -> case v of"
        , "                 []     -> error \"last: []\""
        , "                 (x:xs) -> case xs of"
        , "                            []    -> x"
        , "                            (_:_) -> last xs" ++ o [2..4] ") (x:xs)"
        ]

   when (n `elem` [3]) $ do
     fontSize 20 $ codeBox hs $
       [ "last :: [a] -> a"
       , "last = \\ v -> case v of"
       , "                []     -> error \"last: []\""
       , "                (x:xs) -> last_work x xs"
       , ""
       , "last_work :: a -> [a] -> a"
       , "last_work = \\ x xs ->"
       , "        (\\ v -> case v of"
       , "                  []     -> error \"last: []\""
       , "                  (x:xs) -> x"
       , "                  (_:_) ->"
       , "               (\\ v -> case v of"
       , "                         []     -> error \"last: []\""
       , "                         (x:xs) -> last_work x xs) xs) (x:xs)"
       ]

   when (n `elem` [4]) $ do
     fontSize 20 $ codeBox hs $
       [ "last :: [a] -> a"
       , "last = \\ v -> case v of"
       , "                []     -> error \"last: []\""
       , "                (x:xs) -> last_work x xs"
       , ""
       , "last_work :: a -> [a] -> a"
       , "last_work = \\ x xs ->"
       , "         case xs of"
       , "           []       -> x"
       , "           (x:xs) -> last_work x xs"
       ]

   vspace 20
   
   when (n == 2) $ ul $ sequence_ $ 
          [ li $ "Create the worker out of the body and an invented coercion to the target type"
          , li $ "Invent the wrapper which call the worker"
          , li $ "These functions are mutually recursive"
          ]
   when (n >= 3) $ ul $ sequence_ $ 
           [ li $ "We now inline" <+> q"last" <+> "inside" <+> q"last_work"
           , li $ q"last_work" <+> "is now trivially recursive"
           ]
   when (n >= 4) $ ul $ sequence_ $ 
           [ li $ "We now simplify the worker, reaching our efficient implementation"
           ]

   return ()

 | n <- [1..4]
 ] ++
 [ title "Worker/Wrapper Methodology" $ fontSize 28 $ do

   p $  "From a recursive function, construct two new functions"

   let s1 = rightMargin 100 $ frame $ do
         h2 "Wrapper"
         ul $ do
           li "Replacing the original function"
           li "Coerces call to Worker"

   let s2 = frame $ do
         h2 "Worker"
         ul $ do
           li "Performs main computation"
           li "Syntactically contains the body of the original function"
           li "Coerces call from Wrapper"
           li "Has an invented type"

   vspace 20
   twocol s1 0.45 s2 0.55
   vspace 20

   ul $ do
     li "The initial worker and wrapper are mutually recursive"
     li "We then inline the wrapper inside the worker, and simplify"
     li "We end up with"
     ul $ do
             li "An efficient recursive worker"
             li "An impedance matching non-recursive wrapper"


 , title "Worker/Wrapper Transformation" $ fontSize 28 $ do
   let s1 = do
           h2 $ "Prerequisites"    
           p $ "comp :: A"
           p $ "comp = fix body for some body :: A → A"
           p $ "abs :: B → A is a coercion from type B to A"
           p $ "rep :: A → B is a coercion from type A to B"
   let s2 = do
           h2 "Preconditions (1 of)"
           ul $ li $ "abs ◦ rep = id" <> sub "A"
           ul $ li $ "abs ◦ rep ◦ body = body"
           ul $ li $ "fix (abs ◦ rep ◦ body) = fix body"


   twocol s1 0.45 s2 0.55
   vspace 20

   p $ "If the above prerequisites hold, then" <+> q "comp" <+> "can be rewritten as"

   vspace 30   

   font "Courier New" $ fontSize 40 $ trueSpace $ do
     leftMargin 150 $ rightMargin 150 $ frame $ margin 10 $ do
      p $ "comp = abs work"
      p $ "   where work :: B"
      p $ "         work = fix (rep ◦ body ◦ abs)      "


 , title "The Game of Life" $ fontSize 28 $ do

   t1 <- scaledImageTile "hermit/images/LifeGrid.png" 0.4
   place top $ nudge top center $ t1

   ul $ do
           li $ "Well-known cellular simulation"
           li $ "Life and Death based on simple mathematical rules"
           li $ "Can we using worker/wrapper and HERMIT to enable agility regarding our key data-types in the Game of Life?"

 , title "Hutton's Life" $ fontSize 28 $ do
 
   p $ "From Graham Hutton's" <+> quote "Programming Haskell" <> " book:"

   let s1 = fontSize 14 $ codeBox hs $
             [ "width         =  20"
             , "height        =  20"
             , ""
             , "type Pos      = (Int,Int)"
             , "type Board    = [Pos]"
             , ""
             , "wrap          :: Pos -> Pos"
             , "wrap (x,y)    =  (((x-1) `mod` width) + 1, ((y-1) `mod` height + 1))"
             , ""
             , "neighbs      :: Pos -> [Pos]"
             , "neighbs (x,y) =  map wrap [(x-1,y-1), (x,y-1),"
             , "                           (x+1,y-1), (x-1,y),"
             , "                           (x+1,y)  , (x-1,y+1),"
             , "                           (x,y+1)  , (x+1,y+1)]"
             , ""
             , "isAlive       :: Board -> Pos -> Bool"
             , "isAlive b p   =  elem p b"
             , ""
             , "isEmpty       :: Board -> Pos -> Bool"
             , "isEmpty b p   =  not (isAlive b p)"
             ]

   let s2 = fontSize 14 $ codeBox hs $
             [ "liveneighbs   :: Board -> Pos -> Int"
             , "liveneighbs b =  length . filter (isAlive b) . neighbs"
             , ""
             , "survivors     :: Board -> [Pos]"
             , "survivors b   =  [p | p <- b, elem (liveneighbs b p) [2,3]]"
             , ""
             , "births        :: Board -> [Pos]"
             , "births b      =  [p | p <- nub (concat (map neighbs b)),"
             , "                      isEmpty b p,"
             , "                      liveneighbs b p == 3]"
             , ""
             , "nextgen      :: Board -> Board"
             , "nextgen b    =  survivors b ++ births b"
             ]
   twocol s1 0.49 s2 0.49

 , title "Preparation" $ fontSize 28 $ do

   let s1 = do
           p $ "First step, abstract slightly: "
           ul $ do
              li $ "We tagged Hutton's Board structure with a wrapping boolean and size"
              li $ "Hutton's Life plugged into the" <+> q"Life" <+> "class, almost verbatim"
             
           vspace 10
           p $ "This is getting the model into the form we want to implement"


   let s2 = fontSize 18 $ codeBox hs $
                [ "type Pos = (Int,Int)"
                , "type Size = (Int,Int)"
                , "type Config = (Size,Bool)"
                , ""
                , "class Life b where"
                , "     -- create"
                , "     empty :: Config -> b "
                , "     -- board operations"
                , "     diff :: b -> b -> b"
                , "     next :: b -> b"
                , "     -- point operations"
                , "     inv :: Pos -> b -> b"
                , "     -- projections"
                , "     dims :: b -> Size"
                , "     alive :: b -> [Pos]"
                , ""
                , "-- Hutton's new Board"
                , "type Board = LifeBoard Config [Pos]"
                ]

   twocol s1 0.49 s2 0.49

 , title "Provide abs and rep" $ fontSize 28 $ do

   let s1 = fontSize 18 $ codeBox hs $
              [ "-- The new data structure to be used in the implementation"
              , "type Board' = LifeBoard Config (Set Pos)"
              , ""
              , "{-# NOINLINE repb #-}"
              , "repb :: [Pos] -> Set Pos"
              , "repb = fromList"
              , ""
              , "{-# NOINLINE absb #-}"
              , "absb :: Set Pos -> [Pos]"
              , "absb = toList"
              ]
   let s2 = fontSize 18 $ codeBox hs $
              [ "-- repB and absB change the entire Board structure"
              , "{-# NOINLINE repB #-}"
              , "repB :: Board -> Board'"
              , "repB b = LifeBoard (config b) $ repb (board b)"
              , ""
              , "{-# NOINLINE absB #-}"
              , "absB :: Board' -> Board"
              , "absB b = LifeBoard (config b) $ absb (board b)"
              , ""
              , "repBx :: (Board -> a) -> Board' -> a"
              , "repBx f = f . absB"
              , ""
              , "absBx :: (Board' -> a) -> Board -> a"
              , "absBx f = f . repB"
              ]
   twocol s1 0.49 s2 0.49

 , title "Apply worker/wrapper using HERMIT" $ fontSize 28 $ do

   p $ "(Warning! real code)"

   fontSize 18 $ codeBox verb $  
      [ "binding-of 'nextgen"
      , "fix-intro"
      , "down"
      , "split-1-beta nextgen [|absBB|] [|repBB|]"
      , "{ "
      , "  rhs-of 'g"
      , "  repeat (any-call (unfold ['repBB, 'births, 'survivors, 'absBB]))"
      , "  simplify"
      , "  any-call (unfold-rule nextgen)"
      , "}"
      , "let-subst"
      , "alpha-let ['nextgen']"
      , "{ let-bind ; nonrec-rhs ; unfold ; bash }"
      , "top"
      , "innermost let-float"
      ]
 , title "Other Datastructures" $ fontSize 32 $ do

   p $ "We have converted the Game of Life to 3 different representations."

   vspace 30
   ul $ do
      li $ q"Set (Int,Int)" <+> "- Haskell's " <+> q"Set" <+> "representation."
      li $ q"QuadTree Bool" <+> "- 2D binary hierarchical space subdivision of a region."
      li $ q"UVector Bool" <+> "- a packed unboxed vector of bits."

   vspace 30
   p $ "Can we translate to more novel structures?"

 ]
