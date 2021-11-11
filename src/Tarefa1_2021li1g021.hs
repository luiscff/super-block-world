{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- |
Module      : Tarefa1_2021li1g021
Description : Validação de um potencial mapa
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g021 where

import LI12122
import Data.Vector.Primitive (elemIndices)


validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa ((p,c):t)   --esta parte verifica se só há uma porta e chama uma auxiliar
    | contador (Porta,(0,0)) ((p,c):t) == 1 = auxDeclPeca ((p,c):t)
    | otherwise = False

contador :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Int
contador _ [] = 0
contador (a,_) (x:xs)
    | a == fst x = 1 + contador (a,(0,0)) xs 
    | otherwise = contador (a,(0,0)) xs 


auxDeclPeca :: [(Peca, Coordenadas)] ->
--(INCOMPLETO) esta parte verifica se há mais do que uma declaração de peça para a mesma posição

