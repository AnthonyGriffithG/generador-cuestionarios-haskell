
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
    putStrLn $ "1. Insertar pregunta || 2. Terminar preguntas"
    l <- getLine
    if (l /= "1") 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la pregunta"
            z <- recibir
            
            putStrLn $ "1. Opciones tipo escala || 2. Opciones normales"
            tipoPregunta <- getLine
            if(tipoPregunta == "2")
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
    putStrLn $ "1. Insertar opcion || 2. Terminar opciones"
    l <- getLine
    if (l /= "1") 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la opcion"
            z <- recibir
            let lista2 = lista ++ z
            respuestas  lista2

respuestasTipo2 :: [String] -> Int -> IO [String]
respuestasTipo2 lista x = do
    if (x == 5) 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la " ++ show (x+1) ++ " opcion"
            z <- recibir
            let lista2 = lista ++ z
            respuestasTipo2 lista2 (x+1)

encuestas :: [[[String]]] -> IO [[[String]]]
encuestas lista = do
        listaPreguntas <- preguntas []
        let lista2 = lista ++ [listaPreguntas]
        return lista2

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

resPreguntaRandom ::  [String] -> IO [String]
resPreguntaRandom opc = do
    return [opc !! 0]


resEncuesta ::  [[String]] -> [String]-> Int -> Int -> IO [String]
resEncuesta x y z a = do
    let pregunta = x!!0
    putStr "aqui"
    let opciones = x!!1
    putStr "aqui2"
    if (a == 0)
        then do
            respuesta <- resPregunta pregunta opciones
            let lista = y ++ respuesta
            let listaparametro = tail x
            let listaparametro2 = tail listaparametro
            if (listaparametro2 /= [])
                then do
                    resEncuesta listaparametro2 lista z a
                else do
                    return (lista ++ [show z])
    else do
            respuesta <- resPreguntaRandom opciones
            let lista = y ++ respuesta
            let listaparametro = tail x
            let listaparametro2 = tail listaparametro
            if (listaparametro2 /= [])
                then do
                    resEncuesta listaparametro2 lista z a
                else do
                    return (lista ++ [show z])

    

resEncuestas :: [[[String]]] -> [[String]] -> IO [[String]] 
resEncuestas x y = do 
    putStrLn "Digite el numero de encuesta que desea responder"
    strnum <- getLine
    let numencuesta = read strnum :: Int
    let encuesta = x !! numencuesta
    putStrLn "1. Respuesta manual || 2. Respuesta automatica"
    tipoRespuesta <- getLine
    if(tipoRespuesta == "1") 
        then do
            respuestas <- resEncuesta encuesta [] numencuesta 0
            let listaparametros = y ++ [respuestas]
            return listaparametros
    else do
            respuestas <- resEncuesta encuesta [] numencuesta 1
            let listaparametros = y ++ [respuestas]
            return listaparametros
    

cantResXKesimaEncuesta :: [[String]] -> Int -> Int -> Int  
cantResXKesimaEncuesta lista ind cant = do 
    if(lista == [])
        then do
            cant
    else do
        let cabeza = head lista
        if(head (reverse (cabeza)) == show ind)
            then do
                let num = cant + 1
                cantResXKesimaEncuesta (tail lista) ind num
            else do
                cantResXKesimaEncuesta (tail lista) ind cant
            

cantResPorEncuesta:: [[String]] -> Int -> [Int]
cantResPorEncuesta res tam = do
    let listaIndices = [0..tam-1]
    let resultado = map (\x -> cantResXKesimaEncuesta (res) x 0) listaIndices 
    resultado

menu :: [[[String]]] -> [[String]] -> IO Int
menu enc res = do
    putStrLn "~~~~~~~~~~ Generador de FORMS ~~~~~~~~~~"
    putStrLn "Hecho por: Anthony Griffith"

    putStrLn "Opciones:"
    putStrLn "1. Crear encuesta         2. Responder"
    putStrLn ""
    putStrLn "3. Ver estadisticas       4. Finalizar"
    putStrLn ""
    putStrLn "Digite el indice de la opcion que desea realizar: "

    x <- getLine
    if(x == "1") then do 
        newEnc <- encuestas enc
        menu newEnc res 
    else if(x == "2") then do
        newRes <- resEncuestas enc res
        menu enc newRes
    else if(x == "3") then do
        putStr "Cantidad de encuestas realizadas: "
        print(length enc)
        putStrLn ""

        putStr "Cantidad de respuestas por cada encuesta: "
        let z = cantResPorEncuesta res (length enc)
        print(z)
        putStrLn ""

        putStr "Cantidad total de respuestas: "
        print(length res)
        putStrLn ""

        menu enc res 
    else do
        mapM_ (appendFile "encuestas.txt" . show) [enc]
        mapM_ (appendFile "answers.txt" . show) [res]

        return 0


main :: IO ()
main = do
    menu [[["el presidente es imbecil?"],["si","no"],["es inteligente?"],["si","no"], ["cuanto le queda?"],["10","2"]],
         [["que edad tiene binns"],["18","90"],["es inteligente?"],["si","no"], ["ya va a terminar?"],["si","no"]]] []
    print("")
    
    



   
  







