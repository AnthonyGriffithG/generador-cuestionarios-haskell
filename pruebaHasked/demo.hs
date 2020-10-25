--Obtener por teclado
input :: String -> IO String
input prompt = do
  putStr prompt
  getLine

--Funcion para recibir varios strings
addQuestions :: [a] -> [a] -> [a]
addQuestions x y = x ++ y


preguntas :: [[String]] -> IO [[String]]
preguntas lista = do
    putStrLn $ "Inserte + para insertar otra pregunta, o cualquier otro caracter para terminar preguntas:"
    l <- getLine
    if (l /= "+") 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la pregunta"
            z <- recibir
            
            putStrLn $ "Inserte 1 para tipo de preguntas de escala, o 0 para preguntas normales"
            tipoPregunta <- getLine
            if(tipoPregunta == "0")
              then do
                listaRespuestas <- respuestas []
                let lista2 = lista ++ [z] ++ [listaRespuestas]
                preguntas  lista2 
              else do
                listaRespuestas <- respuestasTipo2 [] 0
                let lista2 = lista ++ [z] ++ [listaRespuestas]
                preguntas lista2
    
respuestas :: [String] -> IO [String]
respuestas lista = do
    putStrLn $ "Inserte + para insertar otra respuesta, o cualquier otro caracter para terminar respuesta:"
    l <- getLine
    if (l /= "+") 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la respuesta"
            z <- recibir
            let lista2 = lista ++ z
            respuestas  lista2

respuestasTipo2 :: [String] -> Int -> IO [String]
respuestasTipo2 lista x = do
    if (x == 4) 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la " ++ show x ++ " respuesta"
            z <- recibir
            let lista2 = lista ++ z
            respuestasTipo2 lista2 (x+1)

encuestas :: [[[String]]] -> IO [[[String]]]
encuestas lista = do
    putStrLn $ "Inserte + para insertar otra encuesta, o cualquier otro caracter para terminar encuesta:"
    l <- getLine
    if (l /= "+") 
        then do
            return (lista)
        else do
            listaPreguntas <- preguntas []
            let lista2 = lista ++ [listaPreguntas]
            encuestas  lista2 

recibir :: IO [String]
recibir = do
  x <- getLine
  return([x])


resPregunta :: [String] -> [String] -> IO [String]
resPregunta enun opc = do
    print(enun)
    putStrLn "Digite el indice de alguna de las siguients opciones: "
    print(opc)

    res <- getLine
    let numRes = read res :: Int
    return([opc !! numRes])

resEncuesta ::  [[String]] -> [String]-> Int -> IO [String]
resEncuesta x y z= do
    let pregunta = x!!0
    let opciones = x!!1
    respuesta <- resPregunta pregunta opciones
    let lista = y ++ respuesta
    let listaparametro = tail x
    let listaparametro2 = tail listaparametro
    if (listaparametro2 /= [])
        then do
            resEncuesta listaparametro2 lista z
            else do
                return (lista ++ [show z])

resEncuestas :: [[[String]]] -> [[String]] -> IO [[String]] 
resEncuestas x y = do 
    putStrLn "Digite + para responder una encuesta"
    condicion <- getLine
    if(condicion == "+")
        then do 
            strnum <- getLine
            let numencuesta = read strnum :: Int
            let encuesta = x !! numencuesta
            respuestas <- resEncuesta encuesta [] numencuesta
            let listaparametros = y ++ [respuestas]
            resEncuestas x listaparametros
    else do 
        return y

main :: IO ()
main = do
    --x <- resEncuesta [["el presidente es imbecil?"],["si","no"],["es funcional?"],["si","no","talvez"]] [] 1
    
    --Lista de encuestas y una lista de indices

    x <- encuestas [[["el presidente es imbecil?"],["si","no"]]]
    y <- resEncuestas x []
    print(y)

{-
print (indices)

    
-}
    



   
  







