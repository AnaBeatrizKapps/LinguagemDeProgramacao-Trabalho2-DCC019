-- ****************************************************
-- Ana Beatriz Kapps dos Reis - Matricula: 201835006
-- Rosa M. Ottoni Fernandes  - Matricula: 202035506
-- ****************************************************

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

data Tabuleiro = Tabuleiro { matriz :: [[Cl.Celula]] , dimLinha :: Int , dimColuna :: Int }

getCelula :: Tabuleiro -> Int -> Int -> Cl.Celula
getCelula ( Tabuleiro bMatriz tamLinha tamColuna ) x y  
    | ((x-1) >= 0 && (x-1) < tamLinha) && ( (y-1) >= 0 && (y-1) < tamColuna) = (bMatriz!!(x-1))!!(y-1)
    | otherwise    = Cl.Vazia

verificaMina :: Cl.Celula-> Int
verificaMina (Cl.Celula _ _ _ True ) = 1
verificaMina _ = 0

criarTabuleiro :: SR.RandomGen genRand => (Int, Int) -> genRand-> Int -> [[Cl.Celula]]
criarTabuleiro (tamLinha,tamColuna) randomSeed numeroDeMinas = 
    foldr (\row b -> ( auxCriacao (take numeroDeMinas (DL.nub (SR.randomRs (1, tamLinha*tamColuna) randomSeed) ) ) tamColuna row) : b) [] [1..tamLinha]
        where auxCriacao randomList sizeList jj =foldr (\ii a -> Cl.Celula { Cl.posX = ii, Cl.posY = jj, Cl.state = Cl.Fechada , Cl.mine = (elem (ii + (jj-1)*sizeList) randomList) } : a) [] [1..sizeList]

mostrarTabuleiro :: Tabuleiro -> String
mostrarTabuleiro bTabuleiro@( Tabuleiro bMatriz tamLinha tamColuna ) =
    foldr (\row b -> auxShow row bTabuleiro tamColuna (bMatriz!!row) ++ b) 
        ("   " ++ ( DL.intersperse ' '  (take tamColuna (['a'..'z']++['A'..'Z']) ) ) ++ " \n" ) [tamLinha-1,(tamLinha-2)..0] -- ++ map (\x -> ['A', x] ) ['A'..'Z'] ++ map (\x -> ['B', x] ) ['A'..'Z'] ) ) ) 
            where auxShow indexRow bTabuleiro sizeList theLine = [DC.intToDigit ( div (indexRow+1) 10 ), DC.intToDigit ( mod (indexRow+1) 10 ) ] ++ foldr (\col a -> (mostrarCelula bTabuleiro (theLine!!col)) ++ a) " \n" [0..sizeList-1]

modificarLinha :: Char -> [Cl.Celula] -> Int -> [Cl.Celula]
modificarLinha command row indexY = take indexY row ++ [ (Cl.modificarCelula command (row!!indexY) ) ] ++ drop (indexY+1) row

modificarMatriz :: Char -> [[Cl.Celula]] -> Int -> Int -> [[Cl.Celula]]
modificarMatriz command matriz posX posY = take (posX-1) matriz ++ [ (modificarLinha command (matriz!!(posX-1)) posY) ] ++ drop posX matriz

quantidadeMinas :: Tabuleiro -> Cl.Celula -> Int
quantidadeMinas bTabuleiro (Cl.Celula posX posY _ _ ) = verificaMina (getCelula bTabuleiro (posX-1) posY) + verificaMina (getCelula bTabuleiro (posX+1) posY) + verificaMina (getCelula bTabuleiro posX (posY-1) ) + verificaMina (getCelula bTabuleiro posX (posY+1) )

mostrarCelula :: Tabuleiro -> Cl.Celula -> String
mostrarCelula _ (Cl.Celula _ _ Cl.Fechada _ ) = " *"
mostrarCelula bTabuleiro theSquare@(Cl.Celula _ _ Cl.Aberta _ ) = ([' ', (DC.intToDigit (quantidadeMinas bTabuleiro theSquare) )] :: String ) -- Devia ser o numero de vizinhos com bombas
mostrarCelula _ (Cl.Celula _ _ Cl.Marcada _ ) = " B"