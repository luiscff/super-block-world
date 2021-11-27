{- |
Module      : Tarefa3_2021li1g021
Description : Representação textual do jogo
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g021 where

import LI12122
import Tarefa2_2021li1g021

instance Show Jogo where
  show (Jogo mapa jog) = mapshow mapa

mapshow :: Mapa -> String
mapshow [] = [] -- Falta ver o \n no final
mapshow ([] : ys) = "\n" ++ mapshow ys
mapshow ((x : xs) : ys) = case x of
  Vazio -> " " ++ mapshow [xs] ++ mapshow ys
  Bloco -> "X" ++ mapshow [xs] ++ mapshow ys
  Caixa -> "C" ++ mapshow [xs] ++ mapshow ys
  Porta -> "P" ++ mapshow [xs] ++ mapshow ys














--colocarjogador (Jogador (x, y) d c) []
--colocarjogador (Jogador (x, y) d c) [x1] ++ xs





















{-
mapshow :: Mapa -> String
mapshow [] = [] -- Falta ver o \n no final
mapshow ([] : ys) = "\n" ++ mapshow ys
mapshow ((x : xs) : ys) = case x of
  Vazio -> " " ++ mapshow [xs] ++ mapshow ys
  Bloco -> "X" ++ mapshow [xs] ++ mapshow ys
  Caixa -> "C" ++ mapshow [xs] ++ mapshow ys
  Porta -> "P" ++ mapshow [xs] ++ mapshow ys

encontrar :: Jogador -> String -> String            -- Colocar 1º o jogador e depois vazer o mapshow
encontrar (Jogador (x, y) d c) [] = colocarjogador (Jogador (x, y) d c) []
encontrar (Jogador (x, y) d c) (x1 : xs)
  | y == 0 = colocarjogador (Jogador (x, y) d c) [x1] ++ xs
  | y > 0 = x1 : encontrar (Jogador (x, y-1) d c) xs
  | otherwise = []

colocarjogador :: Jogador -> String -> String
colocarjogador (Jogador (x, y) d c) [] = if d == Este then "<" else ">"
colocarjogador (Jogador (x1, y) d c) (x : xs)
  | x1 == 0 = if d == Este then "<" ++ xs else ">" ++ xs
  | x1 > 0 = x : colocarjogador (Jogador (x1-1, y) d c) xs
  | otherwise = []

juntar :: Mapa -> Jogador -> String
juntar l (Jogador (a,b) d c)
  | x == y = x : juntaraux xs ys
  | otherwise = y : juntaraux xs ys
 where
   (x:xs) = mapshow l
   (y:ys) = encontrar (Jogador (a,b) d c) (x:xs)

juntaraux :: String -> String -> String
juntaraux [] l = l
juntaraux l [] = l
juntaraux (x:xs) (y:ys)
  | x == y = x : juntaraux xs ys
  | otherwise = y : juntaraux xs ys
-}
