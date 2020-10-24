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
    putStrLn $ "Inserte + para insertar otra pregunta, o cualquier tro caracter para terminar preguntas"
    l <- getLine
    if (l /= "+") 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la pregunta"
            z <- recibir
            listaRespuestas <- respuestas []
            let lista2 = lista ++ [z] ++ [listaRespuestas]
            preguntas  lista2 
    
respuestas :: [String] -> IO [String]
respuestas lista = do
    putStrLn $ "Inserte + para insertar otra respuesta, o cualquier tro caracter para terminar respuesta"
    l <- getLine
    if (l /= "+") 
        then do
            return (lista)
        else do
            putStrLn $ "Inserte la respuesta"
            z <- recibir
            let lista2 = lista ++ z
            respuestas  lista2 

encuestas :: [[[String]]] -> IO [[[String]]]
encuestas lista = do
    putStrLn $ "Inserte + para insertar otra encuesta, o cualquier tro caracter para terminar encuesta"
    l <- getLine
    if (l /= "+") 
        then do
            return (lista)
        else do

            listaRespuestas <- preguntas []
            let lista2 = lista ++ [listaRespuestas]
            encuestas  lista2 

recibir :: IO [String]
recibir = do
  x <- getLine
  return([x])
  
main :: IO ()
main = do
    x <- encuestas [[["el presidente es imbecil?"],["si","no"]]]

    print(x)

    putStr ""


   
  







