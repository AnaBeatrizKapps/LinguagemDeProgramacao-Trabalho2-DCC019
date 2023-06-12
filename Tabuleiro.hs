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
, tabuleiroPossuiExplodida
, exibirTabuleiroAberto
) where

import qualified System.Random as SR
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Celula as Cl
import qualified Debug.Trace as DT

--  Estrutura que armazena o tabuleiro
data Tabuleiro = Tabuleiro {matriz :: [[Cl.Celula]], numLinhas :: Int, numColunas :: Int}

-- Consultando uma célula do tabuleiro
getCelula :: Tabuleiro -> Int -> Int -> Cl.Celula
getCelula (Tabuleiro matriz numLinhas numColunas) x y  
-- Verifica se a célula passada está dentro dos limites da matriz, senão estiver retorna uma célula vazia
    | ((x >= 0) && (x < numLinhas)) && ((y >= 0) && (y < numColunas)) = (matriz!!(x))!!(y)
    | otherwise = Cl.Vazia

-- Verifica se tem uma mina
verificaMina :: Cl.Celula-> Int
verificaMina (Cl.Celula _ _ _ True) = 1
verificaMina _ = 0

criarTabuleiro :: SR.RandomGen geraAleatorio => (Int, Int) -> geraAleatorio-> Int -> [[Cl.Celula]]
criarTabuleiro (numLinhas, numColunas) semente numeroDeMinas = 
-- nub remove os valores repetidos
-- take pega os primeiros valores de uma lista que será gerada
-- randomRs gera uma lista de valores infinitos dentro do intervalo de 1 a numLinhas * numColunas
    foldr (\linha b -> (auxCriacao (take numeroDeMinas (DL.nub (SR.randomRs (1, numLinhas * numColunas) semente))) numColunas linha) : b) [] [1..numLinhas]
-- elem é usado para verificar se o elemento está em uma lista, retornando true ou false
-- Nesse caso ele verifica a lista gerada na linha acima, ou seja, verifica se o elemento está na lista que foi gerada
        where auxCriacao listaAleatoria tamanhoLista indiceColuna = foldr (\indiceLinha a -> Cl.Celula {Cl.posicaoX = indiceColuna - 1, Cl.posicaoY = indiceLinha - 1, Cl.estado = Cl.Fechada, Cl.flagMina = (elem (indiceLinha + (indiceColuna - 1) * tamanhoLista) listaAleatoria)} : a) [] [1..tamanhoLista]

-- Verifica se no tabuleiro há uma célula com o estado Explodida, essa função retorna True ou False
tabuleiroPossuiExplodida :: Tabuleiro -> Bool
tabuleiroPossuiExplodida (Tabuleiro matriz _ _) = any isExplodida (concat matriz)
  where
    isExplodida (Cl.Celula _ _ estado _) = estado == Cl.Explodida

-- Cria a String de visualização do tabuleiro
mostrarTabuleiro :: Tabuleiro -> String
-- bTabuleiro@ forma de referenciar o tabuleiro (açúcar sintático)
mostrarTabuleiro tabuleiro@(Tabuleiro matriz numLinhas numColunas) =
-- cabeçalho das colunas
    foldr (\linha b -> aux linha tabuleiro numColunas (matriz!!linha) ++ b) 
        ("  " ++ ( DL.intersperse ' '  (take numColunas ['A'..'I'])) ++ " \n") [numLinhas - 1, (numLinhas - 2)..0]
        -- cabeçalho das linhas e do restante do tabuleiro
            where aux indexLinha tabuleiro tamanhoLista lin = [DC.intToDigit (indexLinha + 1)] ++ foldr (\col a -> (mostrarCelula tabuleiro (lin!!col)) ++ a) " \n" [0..tamanhoLista - 1]

-- Modifica o estado da célula na linha
modificarLinha :: Char -> [Cl.Celula] -> Int -> [Cl.Celula]
-- Take pega os elementos da linha que estão antes do elemento a ser modificado e concatena com o elemento modificado e os elementos que estão depois dele na coluna (drop)
modificarLinha comando linha coluna = take coluna linha ++ [(Cl.modificarCelula comando (linha!!coluna))] ++ drop (coluna + 1) linha

-- Altera a linha do elemento a ser modificado
modificarMatriz :: Char -> [[Cl.Celula]] -> Int -> Int -> [[Cl.Celula]]
-- Take pega as linhas estão antes da linha do elemento a ser modificado e concatena com a linha modificada e as linhas que estão depois dela na matriz (drop)
modificarMatriz comando matriz linha coluna = take linha matriz ++ [(modificarLinha comando (matriz!!linha) coluna)] ++ drop (linha + 1) matriz

-- Calcula a quantidade de minas vizinhas a uma célula
quantidadeMinas :: Tabuleiro -> Cl.Celula -> Int
-- Verifica os quatro vizinhos e para cada mina encontrada soma 1 na quantidade
quantidadeMinas tabuleiro (Cl.Celula linha coluna _ _ ) = verificaMina (getCelula tabuleiro (linha - 1) coluna) + verificaMina (getCelula tabuleiro (linha + 1) coluna) + verificaMina (getCelula tabuleiro linha (coluna - 1)) + verificaMina (getCelula tabuleiro linha (coluna + 1))

-- Funções para exibir tabuleiro completo
abrirTodasAsCelulas :: Tabuleiro -> Tabuleiro
abrirTodasAsCelulas tabuleiro@(Tabuleiro matriz numLinhas numColunas) =
  let abrirCelula celula@(Cl.Celula linha coluna estado flagMina) =
        if flagMina
          then Cl.Celula linha coluna Cl.Explodida flagMina
          else Cl.Celula linha coluna Cl.Aberta flagMina
      matrizAberta = map (map abrirCelula) matriz
  in Tabuleiro matrizAberta numLinhas numColunas

exibirTabuleiroAberto :: Tabuleiro -> String
exibirTabuleiroAberto tabuleiro =
  let tabuleiroAberto = abrirTodasAsCelulas tabuleiro
  in mostrarTabuleiro tabuleiroAberto

mostrarCelula :: Tabuleiro -> Cl.Celula -> String
-- Células fechadas mostram *
mostrarCelula _ (Cl.Celula _ _ Cl.Fechada _) = " *"
-- Células abertas número de bombas vizinhas
mostrarCelula tabuleiro celula@(Cl.Celula x y Cl.Aberta _) = ([' ', (DC.intToDigit (quantidadeMinas tabuleiro celula))] :: String)
-- Células marcadas mostram o símbolo B
mostrarCelula _ (Cl.Celula _ _ Cl.Marcada _) = " B"
-- Células com a mina mostram o símbolo M
mostrarCelula _ (Cl.Celula _ _ Cl.Explodida _) = " M"
