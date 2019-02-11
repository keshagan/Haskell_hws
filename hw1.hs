-- Serdyukov Innokentiy

module HW01 where

euclid :: Integer -> Integer -> Integer
--euclid x y = if (x == 0) \/ (y == 0) then x else ( if x > y then euclid (x - y) y else euclid x (y - x) )
euclid x y | x == 0 = abs y
           | y == 0 = abs x
           | (abs x) == (abs y) = abs x
           | abs x < abs y = euclid (abs x) (abs y - abs x)
           | otherwise = euclid (abs x - abs y) (abs y)


eulerTHelper :: Integer -> Integer -> Integer
eulerTHelper x y | y == 0 = 0
                 | x == 1 = 1
                 | euclid x y == 1 = (1 + eulerTHelper (x - 1) y)
                 | otherwise = eulerTHelper (x - 1) y

eulerTotient :: Integer -> Integer
eulerTotient x = eulerTHelper (abs x) (abs x)

-- now I know that we have only positive Integers, so exp works for x, y > 0


myexp :: Integer -> Integer -> Integer
myexp x y | y == 0 = 1
          | otherwise = x * ( myexp x (y - 1) )


integrateHelper :: (Double -> Double) -> Double -> Double -> Integer -> Double
integrateHelper (f) a b n | n == 1 = (f a + f b) * (b - a) / 2.0
                          | otherwise = ( integrateHelper (f) (a + ((b - a) / fromIntegral n)) b (n - 1)) + ((f a + (f (a + ((b - a) / fromIntegral n)))) * ((b - a) / fromIntegral n) / 2.0)


integrate
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
integrate (f) a b = integrateHelper (f) a b 100

-- =================================================================================


permute :: (a -> b -> c) -> b -> a -> c
permute (f) x y = ((f y) x)


pairProd :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairProd (f) (g) x = (f (fst x), g (snd x))


fix :: (a -> a) -> a
fix (f) = f (fix f)


weirdFunction
    :: (d -> d -> b)
    -> (a -> b -> c)
    -> (d -> b)
    -> d -> b
weirdFunction (f) (g) (h) x = h x


data CoList a = Nil | Snoc (CoList a) a
    deriving (Show, Eq)


coListToList :: CoList a -> [a]
coListToList Nil = []
coListToList (Snoc xs x) = (coListToList xs) ++ [x]


coListConcat :: CoList a -> CoList a -> CoList a
coListConcat l Nil = l
coListConcat l (Snoc xs x) = Snoc (coListConcat l xs) x


listToCoList :: [a] -> CoList a
listToCoList [] = Nil
listToCoList (x:xs) = coListConcat (Snoc Nil x) (listToCoList xs)



data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)


instance Functor Tree where
    -- fmap :: Functor Tree => (a -> b) -> Tree a -> Tree b
    fmap (f) Leaf = Leaf
    fmap (f) (Node l v r) = Node (fmap (f) l) (f v) (fmap (f) r)


treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node l v r) = (treeToList l) ++ [v] ++ (treeToList r)


treeToCoList :: Tree a -> CoList a
treeToCoList t = listToCoList (treeToList t)

-- another way to do it
treeToCoListAlter :: Tree a -> CoList a
treeToCoListAlter Leaf = Nil
treeToCoListAlter (Node l v r) = coListConcat (coListConcat (treeToCoListAlter l) (Snoc Nil v)) (treeToCoListAlter r)


isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty (Node l v r) = False



eitherCommute :: Either a b -> Either b a
eitherCommute (Left x) = Right x
eitherCommute (Right x) = Left x


eitherAssoc :: Either a (Either b c) -> Either (Either a b) c
eitherAssoc (Left x) = Left (Left x)
eitherAssoc (Right (Left x)) = Left (Right x)
eitherAssoc (Right (Right x)) = Right x


listSum :: Num a => [a] -> a
listSum l = case l of
    [] -> 0
    (x:xs) -> x + listSum xs


filterList :: (a -> Bool) -> [a] -> [a]
filterList p l = case l of
    [] -> []
    (x:xs) -> if p x then (x : filterList p xs) else filterList p xs


safeHead :: [a] -> Maybe a
safeHead l = case l of
    [] -> Nothing
    (x:xs) -> Just x

--  

distributivity :: (a, Either b c) -> Either (a, b) (a, c)
distributivity (x, y) = case y of
    Left y -> Left (x, y)
    Right y -> Right (x, y)


--main = print $ eulerTotient 24