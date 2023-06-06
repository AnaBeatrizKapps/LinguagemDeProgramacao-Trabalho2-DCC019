-- ****************************************************
-- Ana Beatriz Kapps dos Reis - Matricula: 201835006
-- Rosa M. Ottoni Fernandes  - Matricula: -----
-- ****************************************************

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Tabuleiro as Tb
import System.Random (newStdGen)
import System.IO

data GameState  = Neutral | Invalid | Victory  deriving( Ord, Eq, Show )

data Controller = Controller { minas :: Int , marcados :: Int , abertos :: Int , tamTabuleiro :: Int , flag_invalido :: Bool }

data Game = Game { tabuleiro :: Tb.Tabuleiro , controller :: Controller }

-- putStr :: String -> IO ()  
-- putStr [] = return ()  
-- putStr (x:xs) = do  
--     putChar x  
--     putStr xs  

numberToInt :: String -> Int
numberToInt [a,b]
    | (DC.digitToInt a) >= 0 && (DC.digitToInt a) < 10 && (DC.digitToInt b) >= 0 && (DC.digitToInt b) < 10 = (DC.digitToInt a)*10 + (DC.digitToInt b)
    | otherwise = error "Entrada Inválida! <numberToInt>"
numberToInt theError =  error "Entrada Inválida!"

gameController :: Controller -> GameState
gameController (Controller theMines theMarkeds theOpens theBoardSize theInvalid)
    | theBoardSize - theOpens == theMines = Victory
    | theInvalid = Invalid
    | otherwise = Neutral

inputCommand :: Char -> Int -> Int -> Tb.Tabuleiro -> Controller -> Game
inputCommand '+' i j (Tb.Tabuleiro bMatriz tamanhoLinha tamanhoColuna) (Controller a m b c d) = (Game (Tb.Tabuleiro (Tb.modificarMatriz '+' bMatriz i j ) tamanhoLinha tamanhoColuna) (Controller a (m+1) b c d) )
inputCommand '-' i j (Tb.Tabuleiro bMatriz tamanhoLinha tamanhoColuna) (Controller a m b c d) = (Game (Tb.Tabuleiro (Tb.modificarMatriz '-' bMatriz i j ) tamanhoLinha tamanhoColuna) (Controller a (m-1) b c d) )
inputCommand '_' i j (Tb.Tabuleiro bMatriz tamanhoLinha tamanhoColuna) (Controller a b o c d) = (Game (Tb.Tabuleiro (Tb.modificarMatriz '_' bMatriz i j ) tamanhoLinha tamanhoColuna) (Controller a b (o+1) c d) )
inputCommand  _  _ _  b c = error "Entrada Inválida!"

-- INVERT LINE AND COLUMN TO FIND THE LINE FIRST
handleCommand :: String -> Game -> Game
handleCommand (k:j:i) ( Game bTabuleiro theController@( Controller a b c d e ) )
    | (k == '+' || k == '-' ||  k == '_' ) && (j <= 'z' && j >= 'a' || j <= 'Z' && j >= 'A') && (numberToInt i < 100) =
        inputCommand k   ( numberToInt i  ) ((DC.digitToInt j) - (DC.digitToInt 'a')) bTabuleiro theController
    -- | i == ""                && (k < 'z' && k > 'a' || k < 'Z' && k > 'A') && (numberToInt [j] < 100) = 
    --     inputCommand '_' (DC.digitToInt j ) ((DC.digitToInt k) - (DC.digitToInt 'a')) bTabuleiro theController
    | otherwise = ( Game bTabuleiro ( Controller a b c d True ) ) -- error ("Entrada Inválida! <handleCommand> k=" ++ [k] ++ "; j=" ++ [j] ++ "; i =" ++ [(DC.intToDigit (numberToInt i))] )
handleCommand _ ( Game bTabuleiro theController@( Controller a b c d e ) ) = ( Game bTabuleiro ( Controller a b c d True ) )

currentRound :: Game -> IO ()
currentRound theGame@(Game bTabuleiro ctrler ) = do
    let currentBoard = Tb.mostrarTabuleiro bTabuleiro
    putStrLn " "
    putStrLn "  *** TABULEIRO *** "
    putStrLn " "
    putStrLn currentBoard
    putStr " Informe seu comando: "
    command <- getLine
    let theGame'@(Game bTabuleiro' ctrler ) = handleCommand command theGame
    if (gameController ctrler) == Invalid
        then do
            putStrLn " "
            putStrLn " "
            putStrLn "  COMANDO INVÁLIDO !!!  "
            putStrLn " "
            putStrLn " ********************   MENU   ******************** "
            putStrLn "---------------------------------------------------"
            putStrLn " _ <posição> => Abrir Posição Ex.: A01, D44, B13 "
            putStrLn "---------------------------------------------------"
            putStrLn " + <posição> => Marcar Posição Ex.: +D92, +C04   "
            putStrLn "---------------------------------------------------"
            putStrLn " - <posição> => Desmarcar Posição Ex.:-D02, -C40 "
            putStrLn "---------------------------------------------------"
            currentRound theGame'
    else if (gameController ctrler) == Victory
        then do
            putStrLn " VOCÊ VENCEU! "
            return ()
        else currentRound theGame'

-- *************************************** Começa o jogo ***************************************
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

                    let tabuleiro = Tb.Tabuleiro { Tb.matriz = Tb.criarTabuleiro dim newSeed qtdMinas, Tb.dimLinha = fst dim, Tb.dimColuna = snd dim } -- fst: extrai primeiro elemento, snd: extrai o segundo elemento

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
                    currentRound (Game tabuleiro controller)
                    return ()

main = do 
    start