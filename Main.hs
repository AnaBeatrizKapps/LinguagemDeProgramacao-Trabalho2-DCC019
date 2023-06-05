-- ****************************************************
-- Ana Beatriz Kapps dos Reis - Matricula: 201835006
-- Rosa M. Ottoni Fernandes  - Matricula: -----
-- ****************************************************

import qualified Data.Char as DC
import qualified Data.List as DL
import System.Random (newStdGen)
import System.IO

data Controller = Controller { minas :: Int , marcados :: Int , abertos :: Int , tamTabuleiro :: Int , flag_invalido :: Bool }

-- Começa o jogo
start :: IO ()
start = do
        putStrLn " "
        putStr " Informe o número de linhas do tabuleiro   (MAX 9) m: "
        tamanhoLinha <- getLine
        if ( (read tamanhoLinha) > 9 || (read tamanhoLinha) < 1 )
            then do
            putStrLn "  "
            putStrLn " *** ERRO *** "
            putStrLn " O número de linhas precisa estar dentro do intervalo (1 até 9). "
            putStrLn " "
            start
            return ()
        else do
            putStr " Informe o número de colunas do tabuleiro  (MAX 9) n: "
            tamanhoColuna <- getLine
            if ( (read tamanhoColuna) > 9 || (read tamanhoColuna) < 1 )
                then do
                putStrLn "  "
                putStrLn " *** ERRO *** "
                putStrLn " O número de colunas precisa estar dentro do intervalor (1 até 9). "
                putStrLn " "
                start
                return ()            else do
                putStr " Informe o número de minas do tabuleiro (MAX m*n/2) k: "
                qtdMinas <- readLn
                let {dim = (read tamanhoLinha, read tamanhoColuna)} -- read: converte valores de string para númerico
                let {maxMinas = (fst dim)*(div (snd dim) 2) } -- calcula o máximo de minas
                if ( qtdMinas > maxMinas || qtdMinas < 1 )
                    then do
                    putStrLn "  "
                    putStrLn " *** ERRO *** "
                    putStrLn (" Insira um valor no intervalo [1," ++ (show maxMinas) ++ "] para o número de minas ")::IO ()
                    putStrLn " "
                    putStrLn " "
                    start
                    return ()
                else do
                    newSeed <- newStdGen -- cria uma nova semente aleatória utilizando a função newStdGen do módulo System.Random
                    let controller = Controller { minas = qtdMinas , marcados = 0 , abertos = 0 , tamTabuleiro = (fst dim)*(snd dim) , flag_invalido = False}

                    -- minas: representa a quantidade de minas no jogo
                    -- marcados: representa o número de células marcadas no jogo.
                    -- abertos: representa o número de células abertas no jogo.
                    -- tamTabuleiro: represneta o tamanho do tabuleiro, calculado como o produto dos valores do primeiro e segundo elemento da variável dim.
                    -- flag_invalido: um indicador booleano para sinalizar se o estado do jogo é inválido ou não.

                    -- criar tabuleiro
                    putStrLn " "
                    putStrLn " "
                    putStrLn " ********************   MENU   ******************** "
                    putStrLn "---------------------------------------------------"
                    putStrLn " _ <posição> => Abrir Posição Ex.: A01, D44, B13 "
                    putStrLn "---------------------------------------------------"
                    putStrLn " + <posição> => Marcar Posição Ex.: +D92, +C04   "
                    putStrLn "---------------------------------------------------"
                    putStrLn " - <posição> => Desmarcar Posição Ex.:-D02, -C40 "
                    putStrLn "---------------------------------------------------"
                    putStrLn " "
                    putStrLn " "
                    -- criar função jogo
                    return ()

main = do 
    start