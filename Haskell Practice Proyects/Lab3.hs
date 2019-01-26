{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
module Lab3 where
import Prelude (Show)
import Lab1
import Lab2
data N where{O::N; S::N->N}
 deriving Show
--Ejercicio 1
pos :: N -> Bool
pos = \n-> case n of {O-> False;
                      S x -> True}
--Definicion de algunos naturales
uno :: N
uno = S O

dos :: N
dos = S uno

tres::N
tres = S dos

cinco :: N
cinco =  tres + dos
--ejercicio2
pred::N->N
pred = \n -> case n of {O -> O; S x-> x}

impar::N->Bool
impar = \n -> case n of{O->False;
                        S x -> not(impar x)}
par :: N->Bool
par = \n-> not(impar n)
impar2::N->Bool
impar2 = \n-> not(par n)

doble :: N->N
doble = \n -> case n of{O->O;
                        S x -> S(S(doble x))}

triple :: N-> N
triple = \n -> case n of{O->O;
                          S x -> S(S(S(triple x)))}
existe::N->(N->Bool)->Bool
existe = \n f -> case n of{O-> f O;
                            S x -> f(S x) || (existe n f)}
todos::N ->(N->Bool)->Bool
todos = \n f -> case n of{O-> f O;
                            S x -> f(S x) && (existe n f)}

contar::N->(N->Bool)->N
contar = \n f -> case n of{O -> case f O of{True -> S O; False -> O};
                            S x -> case f x of{True -> S (contar x f); False -> contar x f}}
(+)::N->N->N
(+) = \n z -> case n of{O -> z;
                        S x -> S(x + z)}

(*)::N->N->N
(*) = \n z -> case n of{O -> O;
                         S x -> z +(x * z)}

(^)::N->N->N
(^) = \n z -> case n of{O -> S O; S x -> n * (n^x)}

fact::N->N
fact = \n -> case n of{O -> S O;
                        S x -> (S x) * (fact x)}

sumi::N->N
sumi = \n -> case n of{O-> O;
                        S x -> S x + (sumi x)}
sumidobles :: N->N
sumidobles = \n -> case n of{O-> O;
                              S x -> S(S x) + (sumidobles x)}

sumfi::(N->N)->N->N
sumfi = \f n->case n of{O -> f O;
                        S x -> f(S x) + sumfi f x}
sumpi ::(N->Bool)->N->N
sumpi = \f n -> case n of{O->O;
                          S x-> case f x of{True -> S x + sumpi f x; False -> sumpi f x}}

sumpares::N->N
sumpares = \n -> case n of{O->O;
                            S x -> case par (S x) of{True -> S x + sumpares x;
                                                      False -> sumpares x}}
instance Eq N where
 (==) = \n m ->  case n of {O -> case m of{O->True; S _ -> False};
                    S x -> case m of {O-> False; S z -> x == z}}

instance Ord N where
  (<=) = \n m -> case n of{O -> True;
                           S x -> case m of{O->False; S y -> x <= y}}


data Estado where {A :: Estado ; B :: Estado ; C :: Estado }
  deriving Show

transicciones :: N -> Estado -> Estado
transicciones = \n e->case n of{O->e;
                                S x -> case e of{A -> transicciones x B; B -> transicciones x C ; C -> transicciones x A}}

data Sem where {Red :: Sem ; Yel :: Sem ; Gre :: Sem }
 deriving Show

cambios :: N->Sem->Sem
cambios = \n e -> case n of{O->e;
                            S x -> case e of{Red -> cambios x Gre; Gre -> cambios x Yel ; Yel -> cambios x Red}}


find:: N->(N->Bool)->N
find = \n p -> case n of{O->O;
                        S x->case p (S x) of
                          {True->S x;False->find x p} }
