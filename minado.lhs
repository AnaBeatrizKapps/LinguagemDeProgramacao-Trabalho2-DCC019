> gerarMatriz tamanho = replicate tamanho . replicate tamanho

> main :: IO ()
> main = do
>  putStrLn "Digite o tamanho do tabuleiro:"
>  input <- getLine
>  let tamanho = read input :: Int
>  let matriz = gerarMatriz tamanho 0
>  print $ matriz
>  putStrLn ("Tabuleiro criado: " ++ show tamanho)
