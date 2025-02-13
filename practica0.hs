module Practica0 where 


{-- Recursion y recursion de Cola --}

--Funcion buscar : Dada una lista de Enteros y elemento , Regresa verdadero en caso de que el elemento se encuentre en la lista
--En otro caso regresa False 

buscar::[Int]->Int->Bool
buscar [] _ = False
buscar (y:ys) x
    | x == y = True
    | otherwise = buscar ys x



--Funcion sumar_lista : Dada una Lista de Entero , regresa la suma de sus elementos
--Implementala con recursion de Cola
sumar_lista::[Int]->Int
sumar_lista p = sumar_lista_aux p 0

sumar_lista_aux:: [Int] -> Int -> Int
sumar_lista_aux [] sumador = sumador
sumar_lista_aux (y:ys) sumador = sumar_lista_aux ys (sumador + y)



--Implementa una funcion recursiva de forma "ordinaria" y despues implementala con recursion de cola
--Usa los comandos vistos en clase para comparar tiempo y memoria usados y dado el resultado describe que sucedio
--Y porque crees que haya sido asi
-- :s +t (en el ghci  para ver la estadisticas )

--Máximo de una lista con recursión ordinaria
maximoListaOrd :: [Int] -> Int
maximoListaOrd [] = error "Lista vacía"
maximoListaOrd [x] = x
maximoListaOrd (x:xs) = max x (maximoListaOrd xs)

--Máximo de una lista con recursión de cola
maximoListaCola :: [Int] -> Int
maximoListaCola [] = error "Lista vacía"
maximoListaCola (x:xs) = maximoListaAux xs x --Llamamos la función auxiliar con el resto de la lista y el primer elemento como acumulador

maximoListaAux :: [Int] -> Int -> Int
maximoListaAux [] acumulador = acumulador
maximoListaAux (x:xs) acumulador = maximoListaAux xs (max x acumulador) --Llamamos recursivamente con el resto de la lista, y el máximo entre el primer elemento actual y el acumulador

--La explicación viene en el README.md


--
{--funciones--}

--Funcion filter toma un predicado (funcion que regresa booleano) y filtra los elementos la lista de entrada  dada la condicion
filterB:: (a -> Bool) -> [a] -> [a]
filterB p [] = []
filterB p (x:xs)
    | p x = x : (filterB p xs)
    | otherwise = filterB p xs

--Implementa una funcion llamada mapear que reciba como argumento una funcion y aplique esta funcion a una lista
mapear:: (a->b) -> [a] -> [b]
mapear f []  = []
mapear f (x:xs) = f x : (mapear f xs)


--Decima extra : .2
--Forma comprehension
mapear_:: (a->b) -> [a] -> [b]
mapear_ f list  = [f x | x <- list]


{--Tipos,clases y Estructuras de Datos --}

--Arbol 
data Tree a = Empty 
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)
 


--Dada la definicion de arbol binario has una funcion que haga un recorrido pre order
preorder:: Tree a -> [a]
preorder Empty =  []
preorder (Node root left right) =  [root] ++  preorder left ++ preorder right

--Hacer una funcion que calcule la altura del arbol ,regresa verdadero en caso de encontrar el eelemento en el arbol
buscar_tree:: Eq a => Tree a -> a -> Bool
buscar_tree Empty e  =  False
buscar_tree (Node root left right) e
    | root == e = True
    | otherwise = buscar_tree left e || buscar_tree right e


--Punto Extra:  Implementa  una funcion que cuente la cantidad de hojas del arbol 
hojas:: Tree a -> Int
hojas Empty  = 0
hojas (Node root Empty Empty) = 1
hojas (Node root left right) = (hojas left) + (hojas right)


--Definicion de Grafica 

type Vertex = Int
type Graph = [(Vertex, [Vertex])]

vecinos :: Graph -> Vertex -> [Vertex]
vecinos [] _ = []  
vecinos ((v, ns):xs) x
    | v == x    = ns 
    | otherwise = vecinos xs x

dfs :: Graph -> Vertex -> [Vertex] -> [Vertex]
dfs graph v visited
    | v `elem` visited = visited  
    | otherwise = foldl (\acc n -> dfs graph n acc) (v : visited) (vecinos graph v)

--Dada la siguiente defincion de grafica , crea una funcion que verifique si la grafica es conexa 
--Tip: USA la funcion auxiliar dfs, (si quieres puedes usar otra de tu propio diseño)

isConnected :: Graph -> Bool   --Funcion a Implementar
isConnected [] = True
isConnected graph =     
    let vertices = map fst graph  
        visitados = dfs graph (head vertices) []  
    in length visitados == length vertices 
    

--Ejemplos

connectedGraph :: Graph
connectedGraph = [(1, [2,3]), (2, [4]), (3, [4,5]), (4, [6]), (5, [6]), (6, [])]  --Debe Regresar True

disconnectedGraph :: Graph
disconnectedGraph = [(1, [2]), (2, [1]), (3, [4]), (4, [3])] --Debe regresar False 


--La siguiente funcion verfiica que la grafica es un arbol 
--Tip : Recuerda que un arbol es una grafica conexa y sin ciclos
isTree :: Graph -> Bool
isTree []  = True
isTree graph = 
    if isConnected graph && sinCiclos graph
    then True
    else False


sinCiclos :: Graph -> Bool
sinCiclos [] = True
sinCiclos graph =
    let vertices = map fst graph
    in all (\v -> not (tieneCiclo graph v [] [])) vertices

tieneCiclo :: Graph -> Vertex -> [Vertex] -> [Vertex] -> Bool
tieneCiclo graph v visited path
    | v `elem` path = True  -- Si el vértice está en el camino actual, encontramos un ciclo
    | v `elem` visited = False  -- Si ya fue visitado, no es un ciclo
    | otherwise = 
        let newVisited = v : visited
            newPath = v : path
        in any (\n -> tieneCiclo graph n newVisited newPath) (vecinos graph v)

--Pruebas para isTree

grafoArbol :: Graph
grafoArbol = [(1, [2]), (2, [3]), (3, [])] --True

grafoConexoConCiclo :: Graph
grafoConexoConCiclo = [(1, [2]), (2, [3]), (3, [1])]  -- False

grafoDesconexo :: Graph
grafoDesconexo = [(1, [2]), (2, []), (3, [])]  -- False

grafoVacio :: Graph
grafoVacio = [] -- True

grafoConCiclosMúltiples :: Graph
grafoConCiclosMúltiples = [(1, [2]), (2, [3]), (3, [1]), (4, [2])] -- False

--La siguiente funcion regresa a suma de las hojas del arbol
leafSum:: Tree Int -> Int 
leafSum Empty = 0
leafSum (Node root Empty Empty) = root
leafSum (Node root left right) = (leafSum left) + (leafSum right)