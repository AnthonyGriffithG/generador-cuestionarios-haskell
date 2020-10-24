import Control.Monad (unless)
--Obtener por teclado
input :: String -> IO String
input prompt = do
  putStr prompt
  getLine

--Funcion para recibir varios strings
addQuestions :: [a] -> [a] -> [a]
addQuestions x y = x ++ y


prompt :: IO ()
prompt = do
    -- get input from user
    putStrLn $ "Enter something different from zero to add more questions"
    l <- getLine
    -- unless will execute its block if the condition is False
    unless (l == "0") $ do
        -- echo back to the user
        preguntas
        putStrLn $ "You entered: " ++ l
        prompt  -- recursive step here

preguntas :: IO ()
preguntas = do
  z <- getLine
  print(z)
  
main :: IO ()
main = do
  prompt
  

  