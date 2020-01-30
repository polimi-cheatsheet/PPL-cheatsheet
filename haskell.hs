module Haskell where

-- Types
-- 5 :: Integer
-- 'a' :: Char
-- inc :: Integer -> Integer
-- [1,2,3] :: [Integer]  -- equivalent to 1:(2:(3:[]))
-- ('b',4) :: (Char, Integer)

-- Function
inc n = n + 1
-- match top to bottom
len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs

-- Lambda
lambda = \x y -> 1+x+y

-- Lists
l = [1,2,3]
l2 = [4,5,6]
l3 = l ++ l2 -- => [1,2,3,4,5,6]
h = head [1,2,3] -- => 1
t = tail [1,2,3] -- => [2,3]

-- Union and struct
data Bool' = False' | True'
data Pnt a = Pnt a a
-- Pnt 2.3 5.7 is a value
-- Pnt Bool is a type
data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
aTree = Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))
data List a = Null | Cons a (List a)
-- equivalent to data [a] = [] | a : [a]
data Point = Point {pointx, pointy :: Float}
-- p = Point 1.0 2.0  --  pointx p => 1.0
type String = [Char] -- type alias

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch left right) = fringe left ++ fringe right

-- Type class
class Listoid l where
    listoidcons :: a -> l a -> l a
    listoidunit :: a -> l a
    listoidappend :: l a -> l a -> l a
    listoidfirst :: l a -> a
    listoidlast :: l a -> a
    listoidrest :: l a -> l a
data LL a = Head a (LL a) | Node a (LL a) | Tail a deriving Show
lconcat (Tail a) (Head l xs) = Head l (Node a xs)
lconcat (Node a xs1) r@(Head l xs2) = Head l (Node a (lconcat xs1 r))
lconcat (Head _ xs1) r = lconcat xs1 r

instance Listoid LL where
    listoidcons a (Head l xs) = Head l (Node a xs)
    listoidunit a = Head a (Tail a)
    listoidappend l r = lconcat l r
    listoidfirst (Head _ (Node a _)) = a
    listoidfirst (Head _ (Tail a)) = a
    listoidlast (Head l _) = l
    listoidrest (Head l (Node _ rest)) = Head l rest
    listoidrest (Head _ (Tail _)) = error "listoidrest on unit"

-- map :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs
-- partially applied infix operator, also: (+ 1) (+)
r1 = map (1 +) [1,2,3] -- => [2,3,4]
-- foldl
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs
r10 = foldl (+) 0 [1,2,3] -- => ((0+1)+2)+3
-- foldr
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)
r11 = foldr (+) 0 [1,2,3] -- => 1+(2+(3+0))
-- concat l = foldr (++) [] l
r12 = concat [[1,2],[3],[4,5]] -- => [1,2,3,4,5]

-- zip :: [a] -> [b] -> [(a,b)]
r7 = zip [1,2,3] "ciao" -- => [(1,'c'), (2,'i'), (3,'a')]

-- Composition
dd = (*2) . (1+) -- dd(x) = (*2)(1+)(x) = 2*(1+x)
r2 = dd 6 -- => 14
-- $ operator for avoiding parentheses
r3 = (10*) $ 5+3 -- => 80

-- Never-ending Computations
numsFrom n = n : numsFrom (n + 1)
squares = map (^2) (numsFrom 0)
r4 = take 5 squares -- => [0,1,4,9,16]
r5 = [1,1..] -- => [1,1,1,1,...]
r6 = [6..] -- => [6,7,8,9,...]

-- List Comprehensions
r8 = [(x,y) | x <- [1,2], y <- [3,4]] -- => [(1,3),(1,4),(2,3),(2,4)]
fib = 1 : 1 : [a+b | (a,b) <- zip fib (tail fib)]

-- Pattern Matching
sign x | x > 0 = 1
       | x == 0 = 0
       | x < 0 = -1
mytake 0 _ = []
mytake _ [] = []
mytake n (x:xs) = x : mytake (n-1) xs
mytake2 m ys = case (m,ys) of
    (0,_) -> []
    (_,[]) -> []
    (n,x:xs) -> x : mytake2 (n-1) xs
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x
    then x : takeWhile' p xs
    else []

-- let
r9 = let x = 3
         y = 12
     in x+y -- => 15
powerset set = powerset' set [[]] where
    powerset' [] out = out
    powerset' (e:set) out = powerset' set (out ++ [e:x | x <- out])

-- seq a b -- returns b only after a completed
-- $! :: (a -> b) -> a -> b
f $! x = seq x (f x)

-- infix operators
infixr 9 -*- -- right associative, max precedence
x -*- y = [(i,j) | i <- x, j <- y]

-- class Eq
instance (Eq a) => Eq (Tree a) where
    Leaf a == Leaf b = a == b
    (Branch l1 r1) == (Branch l2 r2) = (l1 == l2) && (r1 == r2)
    _ == _ = False
-- Eq defines automatically /=
-- Ord is subclass of Eq:
-- class (Eq a) => Ord a where
--    (<), (<=), (>=), (>) :: a -> a -> Bool
--    min, max :: a -> a -> Bool
-- only <= is required

-- class Show
instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch x y) = "<" ++ show x ++ " | " ++ show y ++ ">"

-- Maps
--import Data.Map
--exmap = let m = fromList [("nose", 11), ("emerald", 27)]
--          n = insert "rust" 99 m
--          o = insert "nose" 9 n
--      in (m ! "emerald", n ! "rust", o ! "nose") -- (27,99,9)
-- // is for update/insert m // [(1, "Alpha")]

-- Foldable: a type that can be used with foldr
instance Foldable Tree where
    foldr f z (Leaf x) = f x z
    foldr f z (Branch l r) = foldr f (foldr f z r) l

-- Functor: a type that can be mapped
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
-- functor laws:
--  fmap id = id
--  fmap (f . g) = fmap f . fmap g

-- Applicative Functor:
--   pure a --> an instance of a container with a
--   fs <*> xs --> like fmap but instead of a function, a container
--                 with the functions
concatMap' f l = concat $ map f l
--instance Applicative [] where
--    pure x = [x]
--    fs <*> xs = concatMap' (\f -> map f xs) fs
tconc Empty t = t
tconc t Empty = t
tconc l r = Branch l r
tconcat t = foldr tconc Empty t
tconcatMap f t = tconcat $ fmap f t
instance Applicative Tree where
    pure x = Leaf x
    fs <*> xs = tconcatMap (\f -> fmap f xs) fs
-- replace each function f in fs with f(xs), obtaining a tree of trees,
-- then concatenate all those trees obtaining a tree

-- Monad: algebraic data type containing computation, it can be chained
-- to build ordered sequence
-- class Applicative m => Monad m where
--    -- chain 2 monads passing the output of the first to the second
--    (>>=) :: m a -> (a -> m b) -> m b
--    -- chain 2 monads ignoring the output of the first
--    (>>) :: m a -> m b -> m b
--    m >> k = m >>= \_ -> k
--    -- inject a value into the monad
--    return :: a -> m a
--    return = pure
--    -- fail with a message
--    fail :: String -> m a
--    fail = error

-- Monad laws:
--   return is the identity element:
--       (return x) >>= f  <=>  f x
--       m >>= return  <=>  m
--   associativity for binds
--       (m >>= f) >>= g  <=>  m >>= (\x (f x >>= g))

-- do notation
-- do e1 ; e2      <=>  e1 >> e2
-- do p <- e1; e2  <=>  e1 >>= \p -> e2

esp :: IO Integer
esp = do x <- return 4
         return (x+1) -- => 5
--instance Monad [] where
--    xs >>= f = concatMap f xs
--    fail _ = []
instance Monad Tree where
    xs >>= f = tconcatMap f xs
    fail _ = Empty
exmon :: (Monad m, Num r) => m r -> m r -> m r
exmon m1 m2 = do x <- m1
                 y <- m2
                 return $ x-y
r13 = exmon [10,11] [1,7] -- => [9,3,10,4]

-- State Monad
data State st a = State (st -> (st, a))
instance Functor (State st) where
    fmap f (State g) = State (\s -> let (s', x) = g s
                                    in (s', f x))
instance Applicative (State st) where
    pure x = State (\t -> (t, x))
    (State f) <*> (State g) =
        State (\state -> let (s, f') = f state
                             (s', x) = g s
                         in (s', f' x))
instance Monad (State state) where
    State f >>= g = State (\olds ->
                            let (news, value) = f olds
                                State f' = g value
                            in f' news)
runStateM :: State state a -> state -> (state, a)
runStateM (State f) st = f st
ex = runStateM
     (do x <- return 5
         return (x+1))
     333  -- => (333,6)
getState = State (\state -> (state, state))
putState new = State (\_ -> (new, ()))
ex' = runStateM
      (do x <- getState; return (x+1))
      333 -- => (333,334)
ex'' = runStateM
       (do x <- getState
           putState (x+1)
           x <- getState
           return x)
       333 -- => (334,334)
mapTreeM f (Leaf a) = do
    b <- f a
    return (Leaf b)
mapTreeM f (Branch l r) = do
    l' <- mapTreeM f l
    r' <- mapTreeM f r
    return (Branch l' r')
numberTree tree = runStateM (mapTreeM number tree) 100
    where number v = do cur <- getState
                        putState (cur+1)
                        return (v,cur)
logTree tree = runStateM (mapTreeM log tree) "Log\n"
    where log v = do cur <- getState
                     putState (cur ++ "[node] " ++ (show v) ++ "\n")
                     return v
