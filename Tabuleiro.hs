-- ****************************************************
-- Ana Beatriz Kapps dos Reis - Matricula: 201835006
-- Rosa M. Ottoni Fernandes  - Matricula: 202035506
-- ****************************************************

-- Módulo que implementa o tabuleiro e suas funções
module Tabuleiro ( Tabuleiro(..) 
, quantidadeMinas
, mostrarCelula
, getCelula 
, verificaMina
, criarTabuleiro
, mostrarTabuleiro
, modificarLinha
, modificarMatriz
) where

import qualified System.Random as SR
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Celula as Cl

--  Estrutura que armazena o tabuleiro
data Tabuleiro = Tabuleiro { matriz :: [[Cl.Celula]] , numLinha :: Int , numColuna :: Int }

-- Consultando uma célula do tabuleiro
getCelula :: Tabuleiro -> Int -> Int -> Cl.Celula
getCelula ( Tabuleiro matriz tamLinha tamColuna ) x y 
-- Verifica se a célula passada está dentro dos limites da matriz, senão estiver retorna uma célula vazia
    | ((x-1) >= 0 && (x-1) < tamLinha) && ( (y-1) >= 0 && (y-1) < tamColuna) = (matriz!!(x-1))!!(y-1)
    | otherwise    = Cl.Vazia

-- Verifica se tem uma mina
verificaMina :: Cl.Celula-> Int
verificaMina (Cl.Celula _ _ _ True ) = 1
verificaMina _ = 0

criarTabuleiro :: SR.RandomGen gerarAleatorio => (Int, Int) -> gerarAleatorio-> Int -> [[Cl.Celula]]
criarTabuleiro (tamLinha,tamColuna) semente numeroDeMinas = 
-- nub remove os valores repetidos
-- take pega os primeiros valores de uma lista que será gerada
-- randomRs gera uma lista de valores infinitos dentro do intervalo de 1 a numLinhas * numColunas
    foldr (\linha b -> ( auxCriacao (take numeroDeMinas (DL.nub (SR.randomRs (1, tamLinha*tamColuna) semente) ) ) tamColuna linha) : b) [] [1..tamLinha]
        where auxCriacao randomList tamanhoLista indiceColuna =foldr (\indiceLinha a -> Cl.Celula { Cl.posicaoX = indiceLinha, Cl.posicaoY = indiceColuna, Cl.estado = Cl.Fechada , Cl.flagMina = (elem (indiceLinha + (indiceColuna-1)*tamanhoLista) randomList) } : a) [] [1..tamanhoLista]

-- Cria a String de visualização do tabuleiro
mostrarTabuleiro :: Tabuleiro -> String
-- bTabuleiro@ forma de referenciar o tabuleiro (açúcar sintático)
mostrarTabuleiro tabuleiro@(Tabuleiro matriz numLinhas numColunas) =
-- cabeçalho das colunas
    foldr (\linha b -> aux linha tabuleiro numColunas (matriz!!linha) ++ b) 
        ("  " ++ ( DL.intersperse ' '  (take numColunas ['A'..'Z'])) ++ " \n") [numLinhas - 1, (numLinhas - 2)..0]
        -- cabeçalho das linhas e do restante do tabuleiro
            where aux indexLinha tabuleiro tamanhoLista lin = [DC.intToDigit (indexLinha + 1)] ++ foldr (\col a -> (mostrarCelula tabuleiro (lin!!col)) ++ a) " \n" [0..tamanhoLista - 1]

-- Modifica o estado da célula na linha
modificarLinha :: Char -> [Cl.Celula] -> Int -> [Cl.Celula]
-- Take pega os elementos da linha que estão antes do elemento a ser modificado e concatena com o elemento modificado e os elementos que estão depois dele na coluna (drop)
modificarLinha comando linha coluna = take coluna linha ++ [(Cl.modificarCelula comando (linha!!coluna))] ++ drop (coluna + 1) linha

-- Altera a linha do elemento a ser modificado
modificarMatriz :: Char -> [[Cl.Celula]] -> Int -> Int -> [[Cl.Celula]]
-- Take pega as linhas estão antes da linha do elemento a ser modificado e concatena com a linha modificada e as linhas que estão depois dela na matriz (drop)
modificarMatriz comando matriz linha coluna = take (linha - 1) matriz ++ [ (modificarLinha comando (matriz!!(linha - 1)) coluna)] ++ drop linha matriz

-- Calcula a quantidade de minas vizinhas a uma célula
quantidadeMinas :: Tabuleiro -> Cl.Celula -> Int
-- Verifica os quatro vizinhos e para cada mina encontrada soma 1 na quantidade
quantidadeMinas tabuleiro (Cl.Celula linha coluna _ _ ) = verificaMina (getCelula tabuleiro (linha - 1) coluna) + verificaMina (getCelula tabuleiro (linha + 1) coluna) + verificaMina (getCelula tabuleiro linha (coluna - 1)) + verificaMina (getCelula tabuleiro linha (coluna + 1))

-- 
mostrarCelula :: Tabuleiro -> Cl.Celula -> String
-- Células fechadas mostram *
mostrarCelula _ (Cl.Celula _ _ Cl.Fechada _) = " *"
-- Células abertas número de bombas vizinhas
mostrarCelula tabuleiro celula@(Cl.Celula _ _ Cl.Aberta _) = ([' ', (DC.intToDigit (quantidadeMinas tabuleiro celula))] :: String ) 
-- Células marcadas mostram o símbolo B
mostrarCelula _ (Cl.Celula _ _ Cl.Marcada _) = " B"