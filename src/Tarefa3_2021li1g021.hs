{- |
Module      : Tarefa3_2021li1g021
Description : Representação textual do jogo
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g021 where

import LI12122

instance Show Jogo where
  show (Jogo mapa jog) = jogo mapa jog

-- | Função: Tranforma dados do tipo Mapa em dados do Tipo Show
-- == Código
-- @
-- jogo :: Mapa -> Jogador -> String
-- jogo [] _ = ""
-- jogo (x1:xs) (Jogador (x,y) d c)
--  | y == 0 = colocarjogador x1 (Jogador (x,y) d c) ++ "\n" ++ mapshow xs
--  | y > 0 = mapshow [x1] ++ jogo xs (Jogador (x,y-1) d c)
--  | otherwise = ""
-- @
jogo ::
  -- | Recebe um Mapa
  Mapa ->
  -- | Recebe um Jogador
  Jogador ->
  -- | Converte estes inputs em uma String
  String
jogo [] _ = ""
jogo (x1 : xs) (Jogador (x, y) d c)
  | y == 0 = colocarjogador x1 (Jogador (x, y) d c) ++ "\n" ++ mapshow xs
  | y > 0 = mapshow [x1] ++ "\n" ++ jogo xs (Jogador (x, y -1) d c)
  | otherwise = ""

-- | Função: Coloca o Jogador na posição Correta
-- == Código
-- @
-- colocarjogador :: [Peca] -> Jogador -> String
-- colocarjogador [] _ = ""
-- colocarjogador (z:zs) (Jogador (x,y) d c)
--  | x == 0 = if d == Este then ">" ++ mapshowaux zs else "<" ++ mapshowaux zs
--  | x > 0 = mapshowaux [z] ++ colocarjogador zs (Jogador (x-1,y) d c)
--
-- @
colocarjogador ::
  -- | Recebe uma Lsta de Peças
  [Peca] ->
  -- | Recebe um Jogador
  Jogador ->
  -- | Coloca o Jogador na Lista já no tipo String
  String
colocarjogador [] _ = ""
colocarjogador (z : zs) (Jogador (x, y) d c)
  | x == 0 = if d == Este then ">" ++ mapshowaux zs else "<" ++ mapshowaux zs
  | x > 0 = mapshowaux [z] ++ colocarjogador zs (Jogador (x -1, y) d c)

-- | Função: Converte dados Mapa em String
-- == Código
-- @
-- mapshow :: Mapa -> String
-- mapshow [] = []
-- mapshow ([] : ys) = "\n" ++ mapshow ys
-- mapshow ((x : xs) : ys) = case x of
--  Vazio -> " " ++ mapshow [xs] ++ mapshow ys
--  Bloco -> "X" ++ mapshow [xs] ++ mapshow ys
--  Caixa -> "C" ++ mapshow [xs] ++ mapshow ys
--  Porta -> "P" ++ mapshow [xs] ++ mapshow ys
-- @
mapshow ::
  -- | Recebe um Mapa
  Mapa ->
  -- | Tranforma em uma String
  String
mapshow [] = []
mapshow ([] : ys) = mapshow ys
mapshow ((x : xs) : ys)
  | x == Vazio && ys /= [] = " " ++ mapshow [xs] ++ "\n" ++ mapshow ys
  | x == Bloco && ys /= [] = "X" ++ mapshow [xs] ++ "\n" ++ mapshow ys
  | x == Caixa && ys /= [] = "C" ++ mapshow [xs] ++ "\n" ++ mapshow ys
  | x == Porta && ys /= [] = "P" ++ mapshow [xs] ++ "\n" ++ mapshow ys
  | x == Vazio && ys == [] = " " ++ mapshow [xs]
  | x == Bloco && ys == [] = "X" ++ mapshow [xs]
  | x == Caixa && ys == [] = "C" ++ mapshow [xs]
  | x == Porta && ys == [] = "P" ++ mapshow [xs]

-- | Função Auxiliar: Converte uma Lista de Peças em String
-- == Código
-- @
-- mapshowaux :: [Peca] -> String
-- mapshowaux [] = []
-- mapshowaux (x:xs)
--  | x == Vazio = " " ++ mapshowaux xs
--  | x == Bloco = "X" ++ mapshowaux xs
--  | x == Caixa = "C" ++ mapshowaux xs
--  | x == Porta = "P" ++ mapshowaux xs
--
-- @
mapshowaux ::
  -- | Recebe uma Lista de Peças
  [Peca] ->
  -- | Converte em uma String
  String
mapshowaux [] = []
mapshowaux (x : xs)
  | x == Vazio = " " ++ mapshowaux xs
  | x == Bloco = "X" ++ mapshowaux xs
  | x == Caixa = "C" ++ mapshowaux xs
  | x == Porta = "P" ++ mapshowaux xs
