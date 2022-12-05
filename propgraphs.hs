import Data.Char
import Prelude
import Data.Either

type Rho = [(String, String, String)]
type Lambda = [(String, String)]
type Sigma = [(String, String, String)]
type Props = [(String, String)]
type V = [String]
type E = [String]


data PG = PG { rho :: Rho,
            lambda :: Lambda,
            sigma :: Sigma,
            props :: Props
            } deriving (Show)


{-Durante la realizacion de la practica he realizado varias suposiciones, entre las que se encuentran:
    -Los archivos ".pg" que sirven como entrada para la funcion populate en los cuales hay vertices y aristas, en primer lugar estaran los vertices y a continuacion 
    las aristas. Este orden se mantendra cuando se añadan nuevos elementos al Grafo.
    -Las etiquetas de los vertices empezarán con letra mayuscula y las de las aristas con minuscula.
    -Al añadir una arista o una propiedad, esta podra ser añadida aunque los nodos no existan en lambda. Sin embargo, esta no sera mostrada por showGraph
-}

--Las cuatro siguientes funciones reciben listas de listas de Strings y convierten los datos recibidos en las listas de tuplas que formaran el PG
creaRho :: [[String]] -> Rho
creaRho [[a,b,c]] = [(a,b,c)] 
creaRho ([a,b,c] : xs) = [(a,b,c)] ++ creaRho(xs)
             
creaLambda :: [[String]] -> Lambda
creaLambda [[a,b]] = [(a,b)] 
creaLambda ([a,b] : xs) = [(a,b)] ++ creaLambda(xs)

creaSigma :: [[String]] -> Sigma
creaSigma [[a,b,c]] = [(a,b,c)] 
creaSigma ([a,b,c] : xs) = [(a,b,c)] ++ creaSigma(xs)

creaProps :: [[String]] -> Props
creaProps [[a,b]] = [(a,b)] 
creaProps ([a,b] : xs) = [(a,b)] ++ creaProps(xs)

--Recibe los archivos, los mapea y asocia las listas de tuplas obtenidas en un PG
populate :: String -> String -> String -> String -> IO PG
populate a b c d = do
    edg <- readFile a
    lab <- readFile b 
    prop <- readFile c 
    props <- readFile d
    let dt1 = map words $ lines edg
    let dt2 = map words $ lines lab
    let dt3 = map words $ lines prop
    let dt4 = map words $ lines props
    let a = creaRho dt1
    let b = creaLambda dt2
    let c = creaSigma dt3
    let d = creaProps dt4 
    let propGraph = PG a b c d
    return propGraph

--Funcion auxiliar que elimina los corchetes para poder sumar listas de caracteres
quitaCor :: [[Char]] -> [Char]
quitaCor [a] = a

--Las siguientes funciones son auxiliares de la funcion showGraph. Se encargan de buscar e imprimir los datos requeridos
buscaSigma :: Sigma -> String -> [[Char]]
buscaSigma (s:sigma) n = do
    if first(s) == n then do
        if sigma /= [] then do
            let a = quitaCor(buscaSigma sigma n)
            if first(head(sigma)) == n then return ("(" ++ second(s) ++ "," ++ third(s) ++ ")" ++ "," ++ a)
            else return ("(" ++ second(s) ++ "," ++ third(s) ++ ")")
        else return ("(" ++ second(s) ++ "," ++ third(s) ++ ")") 
    else if sigma /= [] then return (quitaCor(buscaSigma sigma n))
    else return ""

buscaRho :: Rho -> String -> (String,String)
buscaRho ((a,b,c):rr) e = do 
    if a == e then (b,c)
    else buscaRho rr e   

buscaNodos :: PG -> IO ()
buscaNodos (PG rho (x:lambda) sigma props) = do
    let nodo = fst(x)
    let tag = head(snd(x))
    --interpretacion: las etiquetas de los vertices empiezan con mayuscula, y las de las aristas con minuscula
    if isUpper tag then do 
        print (fst(x) ++ "[" ++ snd(x) ++ "]{" ++ quitaCor(buscaSigma sigma nodo) ++ "}")  
    else do
        let tup = buscaRho rho nodo
        print ("(" ++ fst(tup) ++ ")-" ++ nodo ++ "[" ++ snd(x) ++ "]->(" ++ snd(tup) ++ "){" ++ quitaCor(buscaSigma sigma nodo) ++ "}")
    if lambda /= [] then buscaNodos(PG rho lambda sigma props)
    else putStrLn "\n"

--Las siguientes funciones devuelven un elemento de una tupla de tres elementos
first :: (String,String,String) -> String
first (a,_,_) = a

second :: (String,String,String) -> String
second (_,b,_) = b

third :: (String,String,String) -> String
third (_,_,z) = z

secAndThi :: (String,String,String) -> (String,String)
secAndThi (_,b,c) = (b,c)

showGraph :: PG -> IO () 
showGraph pg = do 
    buscaNodos pg

--Determina la posicion que debera ocupar el vertice añadido
buscaPosRho :: Rho -> String -> Int -> Int
buscaPosRho [] e pos = pos 
buscaPosRho (r:rho) e pos = do
    if first(r) == e then pos
    else buscaPosRho rho e (pos + 1)
    
tieneEtiqueta :: Lambda -> String -> Bool
tieneEtiqueta [] _ = False
tieneEtiqueta (l:lambda) e = do
    if fst l == e then True
    else tieneEtiqueta lambda e 

defElabelDefault :: PG -> String -> PG
defElabelDefault (PG rho lambda sigma props) e = (PG rho (lambda++[(e,"none")]) sigma props)
    
--si la arista no tiene etiqueta, se definira una predeterminada none. La funcion permite unir dos vertices que no existan en lambda
addEdge :: PG -> String -> String -> String -> PG
addEdge (PG rho b c d) e v1 v2 = do
    let pos = buscaPosRho rho e 0
    let tupla = (e,v1,v2)
    if pos >= length(rho) then do
        if tieneEtiqueta b e == False then defElabelDefault (PG (rho ++ [tupla]) b c d) e
        else PG (rho ++ [tupla]) b c d
    else do
        let (rho1,rho2) = splitAt pos rho
        let rho3 = tail rho2
        let rho4 = (rho1 ++ (tupla:rho3)) 
        (PG rho4 b c d)

--Basandonos en la suposicion numero 2, determina si un nodo es vertice o arista
esVertice :: Lambda -> String -> Bool
esVertice [] _ = False
esVertice (l:lambda) v = do
    if fst(l) == v then do
        if isUpper (head(snd l)) then True
        else False
    else esVertice lambda v

--Busca la posicion apropiada para definir una nueva propiedad de un vertice. Si el vertice ya tiene asociada alguna propiedad, esta se colocara a continuacion. Si no, se colocara antes de las propiedades de las aristas
buscaPosSigmaV :: PG -> String -> String -> Int -> (Int,Bool)
buscaPosSigmaV (PG _ _ [] _) _ _ pos = (pos,False)
buscaPosSigmaV (PG a lambda (s:sigma) d) v prop pos = do
    if first(s) == v then do
        if second(s) == prop then (pos,True) 
        else do
            if first(head(sigma)) == v then buscaPosSigmaV (PG a lambda sigma d) v prop (pos + 1)
            else (pos,False)
    else do 
        if esVertice lambda (first(s)) then do
            buscaPosSigmaV (PG a lambda sigma d) v prop (pos + 1)
        else (pos,False)

buscaPosLambdaVV :: Lambda -> String -> Int -> Int
buscaPosLambdaVV [] _ pos = pos
buscaPosLambdaVV (l:lambda) v pos = do
    if isLower (head (snd l)) then pos
    else buscaPosLambdaVV lambda v (pos + 1)   

defVlabelDefault :: PG -> String -> PG
defVlabelDefault (PG rho lambda sigma props) v = do 
    let pos = buscaPosLambdaVV lambda v 0
    let (lambda1,_:lambda2) = splitAt pos lambda 
    (PG rho (lambda1 ++ ([(v,"None")] ++ lambda2)) sigma props)

defVProp :: PG -> String -> String -> String -> PG
defVProp (PG a b sigma d) v prop val = do
    --interpretacion: la funcion permitira crear una propiedad para el vertice aunque este no exista. A este vertice se le asignara una etiqueta predeterminada None
    let (pos,existeProp) = buscaPosSigmaV (PG a b sigma d) v prop 0
    let tupla = (v,prop,val)
    if existeProp then do
        let (sigma1,sigma2) = splitAt pos sigma
        let sigma3 = tail sigma2
        let sigma4 = (sigma1 ++ (tupla:sigma3))
        if tieneEtiqueta b v == False then defVlabelDefault (PG a b sigma4 d) v
        else (PG a b sigma4 d) 
    else do
        let (sigma1,sigma2) = splitAt pos sigma
        let sigma4 = (sigma1 ++ (tupla:sigma2))
        if tieneEtiqueta b v == False then defVlabelDefault (PG a b sigma4 d) v 
        else (PG a b sigma4 d)

--Similar a los buscaPos comentados con anterioridad. Si la arista ya posee alguna propiedad esta se colocara a continuacion de estas. Si no, se colocara al final.
buscaPosSigmaE :: PG -> String -> String -> Int -> (Int,Bool)
buscaPosSigmaE (PG _ _ [] _) _ _ pos = (pos,False)
buscaPosSigmaE (PG a b (s:sigma) d) e prop pos = do
    if first(s) == e then do
        if second(s) == prop then (pos, True)
        else do 
            if first(head(sigma)) == e then buscaPosSigmaE (PG a b sigma d) e prop (pos + 1)
            else (pos,False)
    else buscaPosSigmaE (PG a b sigma d) e prop (pos + 1)

--Permite definir una propiedad para una arista. Si esta no existe en lambda, creara la propiedad pero no le dara etiqueta.
-- por lo tanto, esta no saldra en showGraph hasta que se cree mediante el addEdge
defEProp :: PG -> String -> String -> String -> PG
defEProp (PG a b sigma d) e prop val = do
    let (pos,existePropE) = buscaPosSigmaE (PG a b sigma d) e prop 0
    let tupla = (e,prop,val)
    if existePropE then do
        let (sigma1,sigma2) = splitAt pos sigma
        let sigma3 = tail sigma2
        let sigma4 = (sigma1 ++ (tupla:sigma3))
        (PG a b sigma4 d)
    else do
        let (sigma1,sigma2) = splitAt pos sigma
        let sigma4 = (sigma1 ++ (tupla:sigma2))
        (PG a b sigma4 d) 

buscaPosLambdaV :: PG -> String -> String -> Int -> (Int,Bool)
buscaPosLambdaV (PG _ [] _ _) _ _ pos = (pos,False)
buscaPosLambdaV (PG rho (l:lambda) sigma props) v lab pos = do
    --interpretacion: si el vertice ya posee una etiqueta (coincida o no con la dada), se emitira un error
    if v == fst(l) && (snd l /= "None") then (pos,True)
    else if v == fst(l) && (snd l == "None") then (pos,False)
    else do
        if esVertice lambda (fst(head lambda)) then buscaPosLambdaV (PG rho lambda sigma props) v lab (pos + 1)
        else (pos,False)

defVlabel :: PG -> String -> String -> Either String PG
defVlabel (PG rho lambda sigma props) v lab = do
    let (pos,existe) = buscaPosLambdaV (PG rho lambda sigma props) v lab 0
    let tupla = (v,lab)
    if existe then Left "El vertice ya esta asociado a una etiqueta"
    else if isUpper (head lab) == False then Left "La etiqueta de un vertice debe empezar con una letra mayuscula"
    else if pos < (length lambda) then do
        let (lambda1,_:lambda2) = splitAt pos lambda
        let lambda4 = (lambda1 ++ (tupla:lambda2))
        Right (PG rho lambda4 sigma props)        
    else do
        let (lambda1,lambda2) = splitAt pos lambda
        let lambda4 = (lambda1 ++ (tupla:lambda2))
        Right (PG rho lambda4 sigma props)

buscaPosLambdaE :: PG -> String -> String -> Int -> (Int,Bool)
buscaPosLambdaE (PG _ [] _ _) _ _ pos = (pos,False)
buscaPosLambdaE (PG rho (l:lambda) sigma props) v lab pos = do
    --interpretacion: si la arista ya posee una etiqueta (coincida o no con la dada), se emitira un error
    if (v == fst(l)) && (snd l /= "none") then (pos,True)
    else if (v == fst(l)) && (snd l == "none") then (pos,False)
    else buscaPosLambdaE (PG rho lambda sigma props) v lab (pos + 1)

defElabel :: PG -> String -> String -> Either String PG
defElabel (PG rho lambda sigma props) v lab = do
    let (pos,existe) = buscaPosLambdaE (PG rho lambda sigma props) v lab 0
    let tupla = (v,lab)
    let posi = buscaPosRho rho v 0
    if existe then Left "La arista ya esta asociada a una etiqueta"
    else if isLower (head lab) == False then Left "La etiqueta de una arista debe empezar con una letra minuscula"
    else if posi == length rho then Left "La arista no tiene asociado ningun vertice. Asignalos mediante la funcion addEdge "
    else if pos < length lambda then do
        let (lambda1,_:lambda2) = splitAt pos lambda
        let lambda3 = lambda1 ++ ([tupla] ++ lambda2)
        Right (PG rho lambda3 sigma props)
    else Right (PG rho (lambda ++ [tupla]) sigma props)



----------------------------------A continuacion se implementaran las funciones consultoras y sus auxiliares


--La funcion recorre sigma y devuelve una lista con los pares "(propiedad,valor)" asociados al nodo dado
sigmaPrima2 :: PG -> String -> [(String,String)] -> [(String,String)]
sigmaPrima2 (PG rho lambda [] props) n l = l
sigmaPrima2 (PG rho lambda (s:sigma) props) n l = do
    if n == first(s) then do
        let l2 = l ++ [(second(s),third(s))]
        sigmaPrima2 (PG rho lambda sigma props) n l2
    else sigmaPrima2 (PG rho lambda sigma props) n l

sigmaPrima :: PG -> String -> [(String,String)]
sigmaPrima (PG rho lambda sigma props) n = do
    sigmaPrima2 (PG rho lambda sigma props) n []

buscaLambda :: Lambda -> String -> String
buscaLambda (l:lambda) n = do
    if fst(l) == n then snd(l)
    else buscaLambda lambda n

--La funcion recorre sigma y devuelve las k primeros pares "(etiqueta,valor)" asociados a esa propiedad. Parara de buscar cuando se acaben los vertices
propV2 :: PG -> Int -> String -> [(String,String)] -> [(String,String)]
propV2 (PG rho lambda sigma props) 0 prop l = l
propV2 (PG rho lambda [] props) k prop l = l
propV2 (PG rho lambda (s:sigma) props) k prop l = do
    if esVertice lambda (first s) then do
        if (second s) == prop then do
            let tupla = ((buscaLambda lambda (first s)), (third s))
            let l2 = l ++ [tupla]
            propV2 (PG rho lambda sigma props) (k-1) prop l2
        else propV2 (PG rho lambda sigma props) k prop l
    else l 

--si el vertice no se encuentra (no existe en sigma), la funcion devolvera una lista vacia
propV :: PG -> Int -> String -> [(String,String)]
propV (PG rho lambda sigma props) k prop = do
    propV2 (PG rho lambda sigma props) k prop [] 

--Similar a propV2
propE2 :: PG -> Int -> String -> [(String,String)] -> [(String,String)]
propE2 (PG rho lambda sigma props) 0 prop l = l
propE2 (PG rho lambda [] props) k prop l = l
propE2 (PG rho lambda (s:sigma) props) k prop l = do
    if (esVertice lambda (first s)) == False then do
        if (second s) == prop then do
            let tupla = ((buscaLambda lambda (first s)), (third s))
            let l2 = l ++ [tupla]
            propE2 (PG rho lambda sigma props) (k-1) prop l2
        else propE2 (PG rho lambda sigma props) k prop l
    else propE2 (PG rho lambda sigma props) k prop l 

--si la arista no se encuentra (no existe en sigma), la funcion devolvera una lista vacia
propE :: PG -> Int -> String -> [(String,String)]
propE (PG rho lambda sigma props) k prop = do
    propE2 (PG rho lambda sigma props) k prop []

--Las funciones siguientes son las auxiliares de la funcion "reachable"

--obtiene la etiqueta de una arista
getLabel :: Lambda -> String -> String -> Bool
getLabel [] _ _ = False
getLabel (l:lambda) e lab = do
    if (fst l) == e then do
        if (snd l) == lab then True
        else False
    else getLabel lambda e lab

--obtiene las adjacencias de un vertice. Estas seran todos los vertices a los que se puede acceder desde este a traves de una arista con etiqueta "lab"
obtenAdj :: PG -> String -> String -> [String] -> [String]
obtenAdj (PG [] lambda sigma props) _ _ l = l
obtenAdj (PG (r:rho) lambda sigma props) v1 lab l = do
    if (second r == v1) && (getLabel lambda (first r) lab) then obtenAdj (PG rho lambda sigma props) v1 lab (l ++ [third r])
    else obtenAdj (PG rho lambda sigma props) v1 lab l

--teniendo en cuenta la posicion del vertice recibida, establece este a visitado
setVis :: String -> [(String,Bool)] -> Int -> [(String,Bool)]
setVis v vis pos = do
    let (vis1,_:vis2) = splitAt pos vis 
    vis1 ++ ((v,True):vis2)

--busca la posicion del vertice en la lista de vertices visitados
buscaPosVis :: String -> [(String,Bool)] -> Int -> Int
buscaPosVis _ [] pos = pos
buscaPosVis n (v:vis) pos = do
    if fst v == n then pos
    else buscaPosVis n vis (pos + 1)

--devuelve si el vertice recibido ya ha sido visitado o no
buscaVis :: String -> [(String,Bool)] -> Bool 
buscaVis v [] = True
buscaVis v (x:vis) = do
    if v == fst x then snd x
    else buscaVis v vis

--recorre las adjacencias recibidas. Si el vertice es el final devuelve True. En caso contrario devuelve el resultado del camino desde ese vertice y hace 
-- una llamada recursiva para comprobar el resto de adjacencias
recorreAdjacencias :: PG -> String -> String -> String -> [String] -> [(String,Bool)] -> Bool
recorreAdjacencias (PG rho lambda sigma props) v1 v2 lab [] vis = False
recorreAdjacencias (PG rho lambda sigma props) v1 v2 lab (a:adj) vis = do
    if buscaVis a vis == False then do
        let pos = buscaPosVis a vis 0
        let vis2 = setVis a vis pos 
        if a == v2 then True
        else do
            if camino (PG rho lambda sigma props) a v2 lab vis2 then True
            else recorreAdjacencias (PG rho lambda sigma props) v1 v2 lab adj vis
    else recorreAdjacencias (PG rho lambda sigma props) v1 v2 lab adj vis

--obtiene las adjacencias de un vertice dado y las recorre
camino :: PG -> String -> String -> String -> [(String,Bool)] -> Bool
camino (PG rho lambda sigma props) v1 v2 lab vis = do 
    let adj = obtenAdj (PG rho lambda sigma props) v1 lab []
    recorreAdjacencias (PG rho lambda sigma props) v1 v2 lab adj vis  

--crea una lista con todos los vertices del Grafo y los declara como no visitados
visit :: Lambda -> [(String,Bool)] -> [(String,Bool)]
visit [] vis = vis
visit (l:lambda) vis = do
    if isUpper $ head (snd l) then visit lambda (vis ++ [((fst l),False)])
    else visit lambda vis

--determina si hay un camino entre vi y vf a traves de aristas con etiqueta lab. Si alguno de los vertices introducidos no existe devolvera False
reachable :: PG -> String -> String -> String -> Bool
reachable (PG rho lambda sigma props) vi vf lab = do
    let visitados = visit lambda []
    camino (PG rho lambda sigma props) vi vf lab visitados

--obtiene un Grafo a partir de un Either String PG
desempaqueta :: Either String PG -> PG
desempaqueta (Right pg) = pg

desempaqueta2 :: Either String PG -> String
desempaqueta2 (Left s) = s

ejecuta :: PG -> IO ()
ejecuta (PG rho lambda sigma props) = do
    putStrLn "\nEscribe el numero asociado a la funcion que desee ejecutar:"
    putStrLn "      1.addEdge: añade una arista al grafo"
    putStrLn "      2.defVProp: añade una propiedad a un vertice"
    putStrLn "      3.defEProp: añade una propiedad a una arista"
    putStrLn "      4.defVLabel: añade una etiqueta a un vertice"
    putStrLn "      5.defELabel: añade una etiqueta a una arista"
    putStrLn "      6.showGraph: imprime el Grafo"
    putStrLn "      7.sigma: imprime los pares (propiedad,valor) asociados a un nodo dado"
    putStrLn "      8.propV: imprime los k primeros pares (etiqueta,valor) asociados a una propiedad de vertices dada"
    putStrLn "      9.propV: imprime los k primeros pares (etiqueta,valor) asociados a una propiedad de aristas dada"
    putStrLn "      11.reachable: determina si existe un camino entre dos vertices pasando por aristas con una etiqueta dada" 
    putStrLn ""
    putStrLn "Para detener la ejecucion escriba end"
    func <- getLine
    if func == "1" then do
        putStrLn "Introduce separado por espacios el nombre de la arista, el vertice origen y el vertice destino"
        params <- getLine
        let ([[e,v1,v2]]) = map words $ lines params
        let pg2 = addEdge (PG rho lambda sigma props) e v1 v2
        showGraph pg2
        ejecuta pg2
    else if func == "2" then do
        putStrLn "Introduce separado por espacios el nombre del vertice, la propiedad y el valor"
        params <- getLine
        let ([[e,p,v]]) = map words $ lines params
        let pg2 = defVProp (PG rho lambda sigma props) e p v
        showGraph pg2
        ejecuta pg2
    else if func == "3" then do
        putStrLn "Introduce separado por espacios el nombre de la arista, la propiedad y el valor"
        params <- getLine
        let ([[e,p,v]]) = map words $ lines params
        let pg2 = defEProp (PG rho lambda sigma props) e p v
        showGraph pg2
        ejecuta pg2
    else if func == "4" then do
        putStrLn "Introduce separado por espacios el nombre del vertice y la etiqueta"
        params <- getLine
        let ([[v,lab]]) = map words $ lines params
        let pg2 = defVlabel (PG rho lambda sigma props) v lab
        if isLeft pg2 then do
            print pg2
            ejecuta (PG rho lambda sigma props)
        else do
            let pg3 = desempaqueta pg2
            showGraph pg3
            ejecuta pg3
    else if func == "5" then do
        putStrLn "Introduce separado por espacios el nombre de la arista y la etiqueta"
        params <- getLine
        let ([[e,lab]]) = map words $ lines params
        let pg2 = defElabel (PG rho lambda sigma props) e lab
        if isLeft pg2 then do
            print pg2
            ejecuta (PG rho lambda sigma props)
        else do
            let pg3 = desempaqueta pg2
            showGraph pg3
            ejecuta pg3
    else if func == "6" then do
        showGraph (PG rho lambda sigma props)
        ejecuta (PG rho lambda sigma props)
    else if func == "7" then do
        putStrLn "Introduce el nodo"
        params <- getLine
        let ([[n]]) = map words $ lines params
        print (sigmaPrima (PG rho lambda sigma props) n)
        ejecuta (PG rho lambda sigma props)
    else if func == "8" then do
        putStrLn "Introduce separado por espacios k y la propiedad"
        params <- getLine
        let ([[k,prop]]) = map words $ lines params
        if isDigit (head k) then do
            let k2 = digitToInt (head k)  
            if k2 < 0 then putStrLn "k debe ser un numero natural" 
            else print (propV (PG rho lambda sigma props) k2 prop)
        else putStrLn "k debe ser un entero"
        ejecuta (PG rho lambda sigma props)
    else if func == "9" then do
        putStrLn "Introduce separado por espacios k y la propiedad"
        params <- getLine
        let ([[k,prop]]) = map words $ lines params
        if isDigit (head k) then do
            let k2 = digitToInt (head k) 
            if k2 < 0 then putStrLn "k debe ser un numero natural"
            else print (propE (PG rho lambda sigma props) k2 prop)
        else print "k debe ser un entero"
        ejecuta (PG rho lambda sigma props)
    else if func == "11" then do
        putStrLn "Introduce separado por espacios el vertice origen, el destino y la etiqueta"
        params <- getLine
        let ([[v1,v2,lab]]) = map words $ lines params
        print (reachable (PG rho lambda sigma props) v1 v2 lab)
        ejecuta (PG rho lambda sigma props)
    else if func == "end" then putStrLn ""
    else do
        print "error"
        ejecuta (PG rho lambda sigma props)


---------------------------------------------------------------------
main = do 
    pg <- (populate "rhoFile.pg" "lambdaFile.pg" "sigmaFile.pg" "propFile.pg")
    ejecuta pg
    putStrLn ""