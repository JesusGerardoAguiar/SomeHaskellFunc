{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
module Lab1 where
import Prelude (Show)
data Bool where {False::Bool ; True::Bool}
 deriving Show
--Jesus Aguiar (216242)
--ejercicio1
id :: a -> a
id = \x -> x
--ejercicio2
k :: a -> b -> a
k = \x -> \y -> x

kid :: a -> b -> b
kid = \x -> \y -> y

apply ::  (a -> b ) -> a -> b
apply = \f -> \x -> f x

twice :: (a -> a ) -> a -> a
twice = \f -> \x -> f (f x)

flip :: (a -> b -> c) -> b -> a -> c
flip = \f -> \x -> \y -> f y x
--ejercicio3
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = \f -> \g -> \x -> f (g x)
--ejercicio4 ya lo hice
--ejercicio5
f1 :: a -> (a -> b) -> b
f1 = \x -> \f -> f x

f2 :: (a -> a) -> a -> a
f2 = \f-> \x -> f (f (f x))

f3 :: a -> (a -> b) -> (b -> b -> c) -> c
f3 = \x -> \y -> \z -> z (y x) (y x)

f4 :: a -> b -> (b -> b -> c) -> c
f4 = \x-> \y -> \z-> z y y

f5 :: (c -> b -> a) -> (c -> c -> b) -> c -> a
f5 = \x-> \y-> \z-> x z (y z z)
--ejercicio6
h1 :: (a -> b -> c) -> a -> b -> c
h1 = \f -> \x -> \y -> f x y

h2 :: (a -> b) -> (b -> c) -> a -> c
h2 = \f -> \g -> \x -> g (f x)

h3 ::  (a -> b) -> (a -> b -> c) -> a -> c
h3 = \f -> \g -> \x -> g x (f x)

h4 :: (b -> c) -> (a -> c -> d) -> a -> b -> d
h4 = \f -> \g -> \x -> \y -> g x (f y)

h5 ::  (a -> b -> a) -> b -> (b -> a) -> a
h5 = \f -> \y -> \g -> f (g y) y
--h5 = \f -> \y -> \g -> (g y)

--ejercicio7
not :: Bool -> Bool
not = \x-> case x of { False -> True ;
True -> False }
--ejercicio8
(||) :: Bool -> Bool -> Bool
(||) = \x -> \y -> case x of { False -> y ; True -> True }

(&&) :: Bool -> Bool -> Bool
(&&) = \x -> \y -> case x of {False -> False; True -> x}

(>>) :: Bool -> Bool -> Bool
(>>) = \x -> \y -> case x of {False -> True; True -> y}

xor :: Bool -> Bool -> Bool
xor = \x y -> case x of {False -> y ; True -> not y}

ni :: Bool -> Bool -> Bool
ni = \x y -> case x of {False -> not y ; True -> False}

--ejercicio9

--(==) :: Bool -> Bool -> Bool
--(==) = \x y -> case x of {True -> y ;
                          --False -> not y}

fequal :: Bool -> Bool -> Bool
fequal = \x y -> (x&&y) || ni x y

(/=) :: Bool -> Bool -> Bool
(/=) = \x y -> case x of {True -> not y;
                          False -> y}
--(/=) = xor

--Ejercicio10
unanimidad ::  Bool -> Bool -> Bool -> Bool
unanimidad = \x y z -> x && y && z || not x && not y && not z



mayoria :: Bool -> Bool -> Bool -> Bool
mayoria = \x y z -> case x of {True -> case y of {True -> True ; False -> z} ; False -> case y of {True -> z ; False -> False}}

ifthenelse :: Bool -> a -> a -> a
ifthenelse = \x y z -> case x of {True -> y ; False -> z}
