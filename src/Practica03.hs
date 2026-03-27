module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Cons b) = Cons b
fnn (Var x) = Var x

fnn (Not (Cons b)) = Cons (not b)
fnn (Not (Var x)) = Not (Var x)

fnn (Not (Not p)) = fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))

fnn (Not (Impl p q)) = fnn (And p (Not q))
fnn (Not (Syss p q)) = fnn (Or (And p (Not q)) (And (Not p) q))
fnn (Not p) = Not (fnn p)

fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)

fnn (Impl p q) = fnn (Or (Not p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))


--Ejercicio 2
fnc :: Prop -> Prop
fnc p = dist (fnn p)

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]

clausulas (And p q) = clausulas p ++ clausulas q

clausulas p = [myNub (lit p)]

lit :: Prop -> [Literal]

lit (Or p q) = lit p ++ lit q

lit p = [p]


--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
--toma cada clausula en c1 y en c2 y si su complemento no esta en la otra clausula la agreaga a la lista de resultados
resolucion c1 c2 =
    case [(l1,l2) | l1 <- c1, l2 <- c2, l1 == Not l2 || Not l1 == l2] of
        [] -> myNub(c1 ++ c2)
        ((l,_):_) ->
            myNub(myFilter (/= l) c1 ++ myFilter (/= Not l) c2)

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
-- Caso base: si la primera cláusula está vacía,ent ya no hay literales que revisar por lo que no hay resolvente
hayResolvente [] _ = False

-- Caso recursivo:Se toma un literal l de la primera cláusula
hayResolvente (l:ls) cl2 =
    -- Verificamos si su complemento (¬l) está en la segunda cláusula
    if Not l `elem` cl2
    then True  -- Si existe, sí hay resolvente
    else hayResolvente ls cl2 || hayResolvente cl2 ls -- Si no, seguimos revisando el resto de la lista contra cl2

--Ejercicio 2   
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion = undefined

--version completa de distributividad, se engloban casos del ejercicio de fnc, (Or p (And q r),Or (And p q) r)
dist :: Prop -> Prop
dist (And p q) = And (dist p) (dist q)
dist (Or (Or p q) r) = dist (Or p (Or q r))

dist (Or p q) =
    case (dist p, dist q) of
        (p', And q1 q2) ->
            And (dist (Or p' q1)) (dist (Or p' q2))
        (And p1 p2, q') ->
            And (dist (Or p1 q')) (dist (Or p2 q'))
        (p', q') ->
            Or p' q'

dist p = p

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)

    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

myNub :: Eq a => [a] -> [a]
myNub []     = []
myNub (x:xs) = x : myNub (myFilter (/= x) xs)
