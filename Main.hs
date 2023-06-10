-- ****************************************************
-- Ana Beatriz Kapps dos Reis - Matricula: 201835006
-- Rosa M. Ottoni Fernandes  - Matricula: 202035506
-- ****************************************************

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Tabuleiro as Tb
import System.Random (newStdGen)
import System.IO

-- Estados do jogo
data EstadoJogo  = Neutro | Invalido | Vitoria  deriving( Ord, Eq, Show )

-- Estrutura que controla o jogo
data Controle = Controle { minas :: Int , marcados :: Int , abertos :: Int , tamTabuleiro :: Int , flag_invalido :: Bool }

-- Estrutura que armazena o jogo
data Jogo = Jogo { tabuleiro :: Tb.Tabuleiro , controller :: Controle }

-- converte uma string contendo 2 caracteres em um número inteiro
numberToInt :: String -> Int
numberToInt [a,b]
    | (DC.digitToInt a) >= 0 && (DC.digitToInt a) < 10 && (DC.digitToInt b) >= 0 && (DC.digitToInt b) < 10 = (DC.digitToInt a)*10 + (DC.digitToInt b)
    | otherwise = error "Entrada Inválida! <numberToInt>"
numberToInt theError =  error "Entrada Inválida!"

-- Verifica os estados do jogo
jogoControle :: Controle -> EstadoJogo
jogoControle (Controle numMinas numMarcados numAbertos tamTabuleiro flagInvalido)
    | tamTabuleiro - numAbertos == numMinas = Vitoria
    | flagInvalido = Invalido
    | otherwise = Neutro

-- Entrada
entrada :: Char -> Int -> Int -> Tb.Tabuleiro -> Controle -> Jogo
entrada '+' linha coluna (Tb.Tabuleiro matriz tamanhoLinha tamanhoColuna) (Controle a m b c d) = (Jogo (Tb.Tabuleiro (Tb.modificarMatriz '+' matriz linha coluna ) tamanhoLinha tamanhoColuna) (Controle a (m+1) b c d) )
entrada '-' linha coluna (Tb.Tabuleiro matriz tamanhoLinha tamanhoColuna) (Controle a m b c d) = (Jogo (Tb.Tabuleiro (Tb.modificarMatriz '-' matriz linha coluna ) tamanhoLinha tamanhoColuna) (Controle a (m-1) b c d) )
entrada '_' linha coluna (Tb.Tabuleiro matriz tamanhoLinha tamanhoColuna) (Controle a b o c d) = (Jogo (Tb.Tabuleiro (Tb.modificarMatriz '_' matriz linha coluna ) tamanhoLinha tamanhoColuna) (Controle a b (o+1) c d) )
entrada  _  _ _  b c = error "Entrada Inválida!"

-- Inverte a entrada do teclado de 'coluna, linha' para 'linha, coluna'
trataComando :: String -> Jogo -> Jogo
trataComando (cmd:col:lin) (Jogo tabuleiro controle@(Controle a b c d e))
    | (cmd == '+' || cmd == '-' || cmd == '_') && DC.isUpper col && (numberToInt lin < 100) =
        entrada cmd (numberToInt lin) (DC.ord col - DC.ord 'A') tabuleiro controle
    | otherwise = Jogo tabuleiro (Controle a b c d True)
trataComando _ (Jogo tabuleiro controle@(Controle a b c d e)) =
    Jogo tabuleiro (Controle a b c d True)

realizaJogada :: Jogo -> IO ()
realizaJogada jogo@(Jogo tabuleiro ctrler ) = do
    let tabuleiroAtual = Tb.mostrarTabuleiro tabuleiro
    putStrLn " "
    putStrLn "  *** TABULEIRO *** "
    putStrLn " "
    putStrLn tabuleiroAtual
    putStr " Informe seu comando: "
    command <- getLine
    let jogo'@(Jogo tabuleiro' ctrler ) = trataComando command jogo
    if (jogoControle ctrler) == Invalido
        then do
            putStrLn " "
            putStrLn " "
            putStrLn "  COMANDO INVÁLIDO !!!  "
            putStrLn " "
            putStrLn " ********************   MENU   ******************** "
            putStrLn "---------------------------------------------------"
            putStrLn " _ <posição> => Abrir Posição Ex.: A01, D04, B03 "
            putStrLn "---------------------------------------------------"
            putStrLn " + <posição> => Marcar Posição Ex.: +D02, +C04   "
            putStrLn "---------------------------------------------------"
            putStrLn " - <posição> => Desmarcar Posição Ex.:-D02, -C40 "
            putStrLn "---------------------------------------------------"
            realizaJogada jogo'
    else if (jogoControle ctrler) == Vitoria
        then do
            putStrLn " Parabéns! Você venceu! "
            return ()
        else realizaJogada jogo'

-- *************************************** Começa o jogo ***************************************
start :: IO ()
start = do
        putStrLn " "
        putStr " Qual o número de linhas do tabuleiro   (MAX 9) m: "
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
            putStr " Qual o número de colunas do tabuleiro  (MAX 9) n: "
            tamanhoColuna <- getLine
            if ( (read tamanhoColuna) > 9 || (read tamanhoColuna) < 1 )
                then do
                putStrLn "  "
                putStrLn " *** ERRO *** "
                putStrLn " O número de colunas precisa estar dentro do intervalor (1 até 9). "
                putStrLn " "
                start
                return ()            else do
                putStr " Qual o número de minas do tabuleiro (MAX ((linhas * colunas)/2)): "
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
                    let controller = Controle { minas = qtdMinas , marcados = 0 , abertos = 0 , tamTabuleiro = (fst dim)*(snd dim) , flag_invalido = False}

                    -- minas: representa a quantidade de minas no jogo
                    -- marcados: representa o número de células marcadas no jogo.
                    -- abertos: representa o número de células abertas no jogo.
                    -- tamTabuleiro: represneta o tamanho do tabuleiro, calculado como o produto dos valores do primeiro e segundo elemento da variável dim.
                    -- flag_invalido: um indicador booleano para sinalizar se o estado do jogo é inválido ou não.

                    let tabuleiro = Tb.Tabuleiro { Tb.matriz = Tb.criarTabuleiro dim newSeed qtdMinas, Tb.numLinha = fst dim, Tb.numColuna = snd dim } -- fst: extrai primeiro elemento, snd: extrai o segundo elemento

                    putStrLn " "
                    putStrLn " "
                    putStrLn " ********************   MENU   ******************** "
                    putStrLn "---------------------------------------------------"
                    putStrLn " _ <posição> => Abrir Posição Ex.: A01, D04, B03 "
                    putStrLn "---------------------------------------------------"
                    putStrLn " + <posição> => Marcar Posição Ex.: +D02, +C04   "
                    putStrLn "---------------------------------------------------"
                    putStrLn " - <posição> => Desmarcar Posição Ex.:-D02, -C04 "
                    putStrLn "---------------------------------------------------"
                    putStrLn " "
                    putStrLn " "
                    realizaJogada (Jogo tabuleiro controller)
                    return ()

main = do 
    start