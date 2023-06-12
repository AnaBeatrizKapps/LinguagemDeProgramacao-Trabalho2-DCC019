-- ****************************************************
-- Ana Beatriz Kapps dos Reis - Matricula: 201835006
-- Rosa M. Ottoni Fernandes  - Matricula: 202035506
-- ****************************************************

-- Módulo que implementa uma célula do tabuleiro e as funções de alteração
module Celula ( Estado(..)
, Celula(..)
, marcaCelula
, desmarcaCelula
, abrirCelula
, modificarCelula
) where

import qualified Data.Char as DC

-- Estrutura que controla o Estado do jogo
data Estado = Fechada | Aberta | Marcada | Explodida deriving(Eq, Show)

-- Estrutura que armazena a célula
data Celula = Vazia | Celula {posicaoX :: Int, posicaoY :: Int, estado :: Estado, flagMina :: Bool} deriving (Show, Eq)

-- Altera o estado da célula para Marcada
marcaCelula :: Celula -> Celula
marcaCelula   (Celula x y e f) = (Celula x y Marcada f)

-- Altera o estado da célula para Fechada
desmarcaCelula :: Celula -> Celula
desmarcaCelula (Celula x y e f) = (Celula x y Fechada f)

-- Abri uma célula e se for uma bomba encerrar o jogo
abrirCelula :: Celula -> Celula
abrirCelula (Celula x y e True)  =  (Celula x y Explodida True)
abrirCelula (Celula x y e False) = (Celula x y Aberta False) 

-- Chama as funções anteriores de acordo com o caracter passado
modificarCelula ::  Char -> Celula -> Celula
modificarCelula '+' celula = marcaCelula celula
modificarCelula '-' celula = desmarcaCelula celula
modificarCelula '_' celula = abrirCelula celula