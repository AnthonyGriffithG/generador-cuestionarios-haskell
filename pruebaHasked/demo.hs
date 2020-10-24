import Control.Monad (unless)
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
  
main :: IO ()
main = do
    x <- encuestas [[["el presidente es imbecil?"],["si","no"]],[["el presidente es imbecil?"],["si","no"]]]
    let n = length x
    let indices = [0..n-1]

    print (x)
    print (indices)
    putStr ""


   
  







