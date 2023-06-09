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
data EstadoJogo  = Neutro | Invalido | Vitoria  deriving(Ord, Eq, Show)

-- Estrutura que controla o jogo
data Controle = Controle {numMinas :: Int, numMarcados :: Int, numAbertos :: Int, tamTabuleiro :: Int, flagInvalido :: Bool}

-- Estrutura que armazena o jogo
data Jogo = Jogo {tabuleiro :: Tb.Tabuleiro, controle :: Controle}

colunaParaInt :: String -> Int
colunaParaInt [a]
    | (DC.digitToInt a) >= 0 && (DC.digitToInt a) < 10 = DC.digitToInt a
    | otherwise = error "Entrada Inválida! <colunaParaInt>"
colunaParaInt theError =  error "Entrada Inválida!"

-- Verifica os estados do jogo
jogoControle :: Controle -> EstadoJogo
jogoControle (Controle numMinas numMarcados numAbertos tamTabuleiro flagInvalido)
    | tamTabuleiro - numAbertos == numMinas = Vitoria
    | flagInvalido = Invalido
    | otherwise = Neutro

-- Entrada
entrada :: Char -> Int -> Int -> Tb.Tabuleiro -> Controle -> Jogo
entrada '+' linha coluna (Tb.Tabuleiro matriz tamanhoLinha tamanhoColuna) (Controle mi ma a t f) = (Jogo (Tb.Tabuleiro (Tb.modificarMatriz '+' matriz linha coluna) tamanhoLinha tamanhoColuna) (Controle mi (ma + 1) a t f))
entrada '-' linha coluna (Tb.Tabuleiro matriz tamanhoLinha tamanhoColuna) (Controle mi ma a t f) = (Jogo (Tb.Tabuleiro (Tb.modificarMatriz '-' matriz linha coluna) tamanhoLinha tamanhoColuna) (Controle mi (ma - 1) a t f))
entrada '_' linha coluna (Tb.Tabuleiro matriz tamanhoLinha tamanhoColuna) (Controle mi ma a t f) = (Jogo (Tb.Tabuleiro (Tb.modificarMatriz '_' matriz linha coluna) tamanhoLinha tamanhoColuna) (Controle mi ma (a + 1) t f))
-- Quando não vem nada da entrada só o tabuleiro e o controle
entrada  _ _ _ t c = error "Entrada Inválida!"

-- Inverte a entrada do teclado de 'coluna, linha' para 'linha, coluna'
trataComando :: String -> Jogo -> Jogo
-- Casos em que os comandos são de marcação
trataComando (cmd:col:lin) (Jogo tabuleiro controle@(Controle mi ma a t f))
    | (cmd == '+' || cmd == '-') && DC.isUpper col && ((colunaParaInt lin) < 10) =
        entrada cmd (colunaParaInt lin) (DC.ord col - DC.ord 'A') tabuleiro controle
-- Caso de abertura de célula
    | (DC.isUpper cmd) && ((DC.digitToInt col) < 10) = entrada '_' (DC.digitToInt col) (DC.ord cmd - DC.ord 'A') tabuleiro controle
-- Caso onde dá inválido
    | otherwise = Jogo tabuleiro (Controle mi ma a t True)
-- Inválido quando não passa nenhuma string
trataComando _ (Jogo tabuleiro controle@(Controle mi ma a t f)) =
    Jogo tabuleiro (Controle mi ma a t True)

realizaJogada :: Jogo -> IO ()
realizaJogada jogo@(Jogo tabuleiro controle) = do
    -- Mostra tabuleiro e extrai o comando inserido pelo jogador
    let tabuleiroAtual = Tb.mostrarTabuleiro tabuleiro
    putStrLn " "
    putStrLn "  *** TABULEIRO *** "
    putStrLn " "
    putStrLn tabuleiroAtual
    putStr " Informe seu comando: "
    comando <- getLine
    let jogo'@(Jogo tabuleiro' controle) = trataComando comando jogo
    -- Caso o comando inserido seja invalido, é solicitado ao usuário inserir um comando válido novamente
    if (jogoControle controle) == Invalido
        then do
            putStrLn " "
            putStrLn " "
            putStrLn "  COMANDO INVÁLIDO !!!  "
            putStrLn " "
            putStrLn " ********************   MENU   ******************** "
            putStrLn "---------------------------------------------------"
            putStrLn "  <posição> => Abrir Posição Ex.: A1, D4, B3 "
            putStrLn "---------------------------------------------------"
            putStrLn " + <posição> => Marcar Posição Ex.: +D2, +C4   "
            putStrLn "---------------------------------------------------"
            putStrLn " - <posição> => Desmarcar Posição Ex.:-D2, -C4 "
            putStrLn "---------------------------------------------------"
            realizaJogada jogo'
    else if (jogoControle controle) == Vitoria -- Verifica se o controle do jogo resultante indica que o jogador venceu o jogo
        then do
            putStrLn " VOCÊ VENCEU! "
            return ()
        else realizaJogada jogo' -- Senão a função realizaJogada é chamada novamente

-- *************************************** Começa o jogo ***************************************
start :: IO ()
start = do
        putStrLn " "
        putStr " Qual o número de linhas você deseja no tabuleiro (MAX 9): "
        tamanhoLinha <- getLine
        if ((read tamanhoLinha) > 9 || (read tamanhoLinha) < 1)
            then do
              putStrLn "  "
              putStrLn " *** ERRO *** "
              putStrLn " O número de linhas precisa estar dentro do intervalo (1 até 9). "
              putStrLn " "
              start
              return ()
        else do
            putStr "  Qual o número de colunas você deseja no tabuleiro  (MAX 9): "
            tamanhoColuna <- getLine
            if ((read tamanhoColuna) > 9 || (read tamanhoColuna) < 1)
                then do
                putStrLn "  "
                putStrLn " *** ERRO *** "
                putStrLn " O número de colunas precisa estar dentro do intervalo (1 até 9). "
                putStrLn " "
                start
                return ()            else do
                putStr "  Qual o número de minas você deseja no tabuleiro (MAX ((linhas * colunas)/2)): "
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
                    let controller = Controle {numMinas = qtdMinas , numMarcados = 0 , numAbertos = 0 , tamTabuleiro = (fst dim)*(snd dim) , flagInvalido = False}

                    -- minas: representa a quantidade de minas no jogo
                    -- marcados: representa o número de células marcadas no jogo.
                    -- abertos: representa o número de células abertas no jogo.
                    -- tamTabuleiro: represneta o tamanho do tabuleiro, calculado como o produto dos valores do primeiro e segundo elemento da variável dim.
                    -- flag_invalido: um indicador booleano para sinalizar se o estado do jogo é inválido ou não.

                    -- Essa expressão irá criar um novo tabuleiro com a matriz de jogo, o número de linhas e o número de colunas especificados
                    let tabuleiro = Tb.Tabuleiro { Tb.matriz = Tb.criarTabuleiro dim newSeed qtdMinas, Tb.numLinhas = fst dim, Tb.numColunas = snd dim } -- fst: extrai primeiro elemento, snd: extrai o segundo elemento

                    putStrLn " "
                    putStrLn " "
                    putStrLn " ********************   MENU   ******************** "
                    putStrLn "---------------------------------------------------"
                    putStrLn " <posição> => Abrir Posição Ex.: A1, D4, B3 "
                    putStrLn "---------------------------------------------------"
                    putStrLn " + <posição> => Marcar Posição Ex.: +D2, +C4   "
                    putStrLn "---------------------------------------------------"
                    putStrLn " - <posição> => Desmarcar Posição Ex.:-D2, -C4 "
                    putStrLn "---------------------------------------------------"
                    putStrLn " "
                    putStrLn " "
                    realizaJogada (Jogo tabuleiro controller)
                    return ()

main = do 
    start