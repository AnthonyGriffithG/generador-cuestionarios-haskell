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
    putStrLn $ "Enter something different from zero to add more questions"
    l <- getLine
    if (l == "0") 
        then do
            return (lista)
        else do
            putStrLn $ "inserte pregunta"
            z <- recibir
            listaRespuestas <- respuestas []
            let lista2 = lista ++ [z] ++ [listaRespuestas]
            preguntas  lista2 
    
respuestas :: [String] -> IO [String]
respuestas lista = do
    putStrLn $ "Enter something different from zero to add more answers"
    l <- getLine
    if (l == "0") 
        then do
            return (lista)
        else do
            putStrLn $ "inserte repuesta"
            z <- recibir
            let lista2 = lista ++ z
            respuestas  lista2 


recibir :: IO [String]
recibir = do
  x <- getLine
  return([x])
  
main :: IO ()
main = do
    x <- preguntas []

    print(x)

    putStr ""


   
  







