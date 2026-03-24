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
fnn (Var p) = Var p
fnn(Not p) = Not (fnn p)
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Not(Not p)) = fnn p
fnn (Not(And p q)) = Or (fnn(Not p)) (fnn(Not q))
fnn (Not(Or p q)) = And (fnn(Not p)) (fnn(Not q))
fnn (Not(Impl p q)) = fnn (Or (Not p) q)
fnn (Not (Syss p q)) = fnn (And (Impl p q) (Impl q p))


--Ejercicio 2
fnc :: Prop -> Prop
fnc p = p
fnc (Not p) = Not (fnc p)
fnc (And p q) = And (fnc p) (fnc q)
fnc (Or p q) = Or (fnc p) (fnc q)
fnc (Or p (And q r)) = And (fnc (Or p q)) (fnc (Or p r))
fnc (Or (And p q) r) = And (fnc (Or p r)) (fnc (Or q r))
--equivalencia de la implicacion
fnc (Impl p q) = fnc (Or (Not p) q)
--equivalencia del bicondicional
fnc (Syss p q) = fnc (And (Impl p q) (Impl q p))


{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas p =[[p]]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas (Or p q) = [literales (Or p q)]
literales :: Prop -> [Literal]
literales (Or p q) = literales p ++ literales q
literales p = [p]


--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
--toma cada clausula en c1 y en c2 y si su complemento no esta en la otra clausula la agreaga a la lista de resultados
resolucion c1 c2 = [l | l <- c1, not (Not l `elem` c2)] ++ [l | l <- c2, not (Not l `elem` c1)]

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
