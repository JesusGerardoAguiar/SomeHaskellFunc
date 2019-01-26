{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
module Lab2 where
import Prelude (Show)
import Lab1

data Carga where {Positivo::Carga ; Negativo::Carga }
 deriving Show
data Dia where {Lu::Dia;Ma::Dia;Mi::Dia;Ju::Dia;Vi::Dia;Sa::Dia;Do::Dia}
 deriving Show
data PPT where {Paper::PPT ; Rock::PPT ; Scis::PPT }
  deriving Show

--ejercicio1
notC :: Carga -> Carga
notC = \x-> case x of { Positivo -> Negativo ;
Negativo -> Positivo }
opuesto :: Carga -> Carga
opuesto = \ x -> notC x;

--ejercicio2
siguiente :: Dia -> Dia
siguiente = \x -> case x of{Lu -> Ma ; Ma -> Mi ; Mi -> Ju ; Ju -> Vi ; Vi -> Sa;Sa -> Do; Do -> Lu}

laborable :: Dia -> Bool
laborable = \x -> case x of{Sa -> False ; Do -> False ; _ -> True}

--ejercicio3
gana::PPT->PPT->PPT
gana = \x y -> case x of{Paper -> case y of {Paper -> Paper ; Rock -> x ; Scis -> y};
                         Rock -> case y of{Paper -> y; Rock -> y; Scis -> x};
                         Scis -> case y of{Paper -> x; Rock -> y; Scis -> y}}

--ejercicio4
class Eq a where
  { (==),(/=)::a->a->Bool ;
    (/=) = \x y -> not (x==y) }
class Eq a => Ord a where
  { (<=),(<),(>=),(>)::a->a->Bool ;
    (>) = \x y -> not (x <= y) ;
    (>=) = \x y -> (x > y) || (x == y);
    (<) = flip (>)}

--ejercicio5
instance Eq Bool where
 (==) = \x y -> case x of {False -> not y ; True -> y}

instance Ord Bool where
 (<=) = \x y -> case x of {False -> case y of {True -> True ; False -> True};
                            True -> y}
--ejercicio6
instance Eq PPT where
 (==) =  \x y -> case x of {Rock -> case y of {Rock -> True; _ -> False} ; Paper -> case y of{Paper -> True; _ -> False}
                            ; Scis -> case y of {Scis -> True ; _ -> False}}

instance Ord PPT where
 (<=) =  \x y -> case x of {Rock -> case y of{Rock -> True; Paper -> True; _ -> False};
                            Paper -> case y of{Paper -> True; Rock->False; Scis -> True};
                            Scis -> case y of {Scis -> True; Rock -> True; _ -> False}}
--ejercicio7

minimo :: Ord a => a -> a -> a
minimo = \x y -> case x < y of{True -> x;
                                False -> y}
maximo :: Ord a => a -> a -> a
maximo = \x y -> case x > y of{True -> x;
                              False -> y}
--ejercicio8
min3::Ord a => a -> a -> a -> a
min3 = \ x y z -> minimo (minimo x y) z
