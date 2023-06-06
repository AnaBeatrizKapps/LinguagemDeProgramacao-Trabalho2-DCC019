-- ****************************************************
-- Ana Beatriz Kapps dos Reis - Matricula: 201835006
-- Rosa M. Ottoni Fernandes  - Matricula: -----
-- ****************************************************

module Celula ( State(..) -- Celula
, Celula(..)
, marcaCelula
, desmarcaCelula
, abrirCelula
, modificarCelula
) where

import qualified Data.Char as DC

data State = Fechada | Aberta | Marcada  deriving( Eq, Show )

data Celula = Vazia | Celula { posX :: Int, posY :: Int , state :: State , mine :: Bool } deriving ( Show, Eq )

marcaCelula :: Celula -> Celula
marcaCelula   ( Celula a b c d )        = ( Celula a b Marcada d )

desmarcaCelula :: Celula -> Celula
desmarcaCelula ( Celula a b c d )        = ( Celula a b Fechada d )

abrirCelula :: Celula -> Celula
abrirCelula ( Celula a b c True )  = error "MINA ATIVADA! VOCÊ PERDEU!"
abrirCelula ( Celula a b c False ) = ( Celula a b Aberta False )

modificarCelula ::  Char -> Celula -> Celula
modificarCelula '+' celula = marcaCelula celula
modificarCelula '-' celula = desmarcaCelula celula
modificarCelula '_' celula = abrirCelula celula