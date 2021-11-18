{- |
Module      : Tarefa1_2021li1g021
Description : Validação de um potencial mapa
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}

module Tarefa1_2021li1g021 where

import LI12122
import FuncoesUteis

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa ((p, c) : t)
  | contador (Porta, (0, 0)) ((p, c) : t) == 1 = norepete ((p, c) : t)
  | otherwise = False

contador :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Int
contador _ [] = 0
contador (a, _) (x : xs)
  | a == fst x = 1 + contador (a, (0, 0)) xs
  | otherwise = contador (a, (0, 0)) xs

norepete :: [(Peca,Coordenadas)] -> Bool
norepete [] = True
norepete ((p,c):t) = (thereisnt c t && norepete t) && emptyspace ((p,c):t)
    where thereisnt :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
          thereisnt c1 [] = True
          thereisnt c1 ((p,c):t) = c1 /= c && thereisnt c1 t


emptyspace :: [(Peca, Coordenadas)] -> Bool
emptyspace [] = False
emptyspace ((p,c):t)
    | p == Vazio || emptyspace t || tamanho (maiorCoordenada ((p,c):t)) > length ((p,c):t) = True
    | otherwise = False
    where tamanho :: (Int,Int) -> Int
          tamanho (x,y) = (x+1)*(y+1)

