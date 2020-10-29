
--Obtener por teclado
input :: String -> IO String
input prompt = do
  putStr prompt
  getLine

--Funcion para recibir varios strings
addQuestions :: [a] -> [a] -> [a]
addQuestions x y = x ++ y

--En la siguiente parte lo que se hara es crear las listas de encuestas, las cuales 
--incluyen preguntas y respuestas cada una.

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
                listaRespuestas <- respuestas [] 1
                let lista2 = lista ++ [z] ++ [listaRespuestas]
                preguntas  lista2 
              else do
                listaRespuestas <- respuestasTipo2 [] 0
                let lista2 = lista ++ [z] ++ [listaRespuestas]
                preguntas lista2
    
respuestas :: [String] -> Int -> IO [String]
respuestas lista cont = do
    putStrLn $ "1. Insertar opcion || 2. Terminar opciones"
    l <- getLine
    if (l /= "1") 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la opcion"
            z <- recibir
            let opcion = show cont ++ ". " ++ z!!0
            let lista2 = lista ++ [opcion]
            respuestas  lista2 (cont + 1)

respuestasTipo2 :: [String] -> Int -> IO [String]
respuestasTipo2 lista x = do
    if (x == 5) 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la " ++ show (x+1) ++ " opcion"
            z <- recibir
            let numOpcion = x + 1
            let opcion = show numOpcion ++ ". " ++ z!!0
            let lista2 = lista ++ [opcion]
            respuestasTipo2 lista2 (x+1)

encuestas :: [[[String]]] -> IO [[[String]]]
encuestas lista = do
        listaPreguntas <- preguntas []
        let lista2 = lista ++ [listaPreguntas]
        return lista2

--Funcion para recibir un input y devolverlo en lista.
recibir :: IO [String]
recibir = do
  x <- getLine
  return([x])

--A partir de aca se realizan las funciones necesarias para lograr responder a las encuestas.
resPregunta :: [String] -> [String] -> IO [String]
resPregunta enun opc = do
    print(enun)
    putStrLn "Digite el indice de alguna de las siguients opciones: "
    print(opc)

    res <- getLine
    let numRes = read res :: Int
    return([opc !! (numRes-1)])

resPreguntaRandom ::  [String] -> IO [String]
resPreguntaRandom opc = do
    return [opc !! 0]


resEncuesta ::  [[String]] -> [String]-> Int -> Int -> IO [String]
resEncuesta x y z a = do
    let pregunta = x!!0
    let opciones = x!!1
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

    

resEncuestas :: [[[String]]] -> [[String]] -> [String] -> IO [[String]] 
resEncuestas x y nombres = do 
    print(nombres)
    putStrLn "Digite el numero de encuesta que desea responder"
    strnum <- getLine
    let numencuesta = read strnum :: Int
    let encuesta = x !! (numencuesta-1)
    putStrLn "1. Respuesta manual || 2. Respuesta automatica"
    tipoRespuesta <- getLine
    if(tipoRespuesta == "1") 
        then do
            respuestas <- resEncuesta encuesta [] (numencuesta-1) 0
            let listaparametros = y ++ [respuestas]
            return listaparametros
    else do
            respuestas <- resEncuesta encuesta [] (numencuesta-1) 1
            let listaparametros = y ++ [respuestas]
            return listaparametros
    
--A partir de este punto se realizan algunas funciones para realizar estadisticas
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

--Funcion principal, la cual consiste en un menu que le permite al usuario realizar distintas opciones.
menu :: [[[String]]] -> [[String]] -> [String] -> Int -> IO Int
menu enc res nom cont = do
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
        putStrLn "Inserte el nombre de la encuesta"
        nombre <- recibir
        let nombreReal = nombre!!0
        let numero = show (cont+1) ++ ". "
        let newNombre = numero ++ nombreReal
        let newNom = nom ++ [newNombre]
        newEnc <- encuestas enc
        let newCont = cont + 1
        menu newEnc res newNom newCont
    else if(x == "2") then do
        newRes <- resEncuestas enc res nom
        menu enc newRes nom cont
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

        putStrLn "Inserte cualquier tecla para volver al menu"
        getLine
        menu enc res nom cont
    else do
        mapM_ (appendFile "encuestas.txt" . show) [enc]
        mapM_ (appendFile "answers.txt" . show) [res]

        return 0
        
main :: IO ()
main = do
    --Se envian dos listas quemadas para prueba.
    menu [[["Como ha sido la administracion del presidente?"],["1. buena","2. mala"],["Esta Costa Rica en crisis"],["1. si","2. no"], ["Que tal esta la situacion?"],["1. pesimo","2. mal", "3. regular", "4. buena", "5. excelente"]],
         [["Que tan buenas son las clases"],["1. Muy malas","2. Malas", "3. Regulares", "4. Buenas", "5. Excelentes"],["Le gusta el modo virtual"],["1. si","2. no"], ["Desea volver a clases presenciales"],["1. si","2. no"]]] [] ["1. Situacion del pais","2. Clases virtuales"] 2
    putStr ""
    
    



   
  







