{- |
Module      : Tarefa1_2021li1g021
Description : Validação de um potencial mapa
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g021 where

import FuncoesUteis
import LI12122

-- | Tarefa 1: Verificar Mapa através de um conjuntos de funções interligadas
--
-- == Código
-- @
-- validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
-- validaPotencialMapa l = checkporta l && norepete l && emptyspace l && checkcaixas l
-- @
validaPotencialMapa ::
  -- | Recebe a lista a avaliar
  [(Peca, Coordenadas)] ->
  -- | Se a lista for válida o mapa é válido - Retorna True
  Bool
validaPotencialMapa l = checkporta l && norepete l && emptyspace l && checkcaixas l

-- | Função: Verifica a existência de apenas uma Porta
--
-- == Código
-- @
-- checkporta :: [(Peca, Coordenadas)] -> Bool
-- checkporta [] = False
-- checkporta ((p, c) : t)
--  | contador (Porta, (0, 0)) ((p, c) : t) == 1 = norepete ((p, c) : t)
--  | otherwise = False
--  where contador :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Int
--        contador _ [] = 0
--        contador (a, _) (x : xs)
--          | a == fst x = 1 + contador (a, (0, 0)) xs
--          | otherwise = contador (a, (0, 0)) xs
-- @
checkporta ::
  -- | Recebe uma Lista
  [(Peca, Coordenadas)] ->
  -- | Caso tenha apenas uma porta retorna True
  Bool
checkporta [] = False
checkporta ((p, c) : t)
  | contador (Porta, (0, 0)) ((p, c) : t) == 1 = True
  | otherwise = False
  where
    contador ::
      -- | Recebe um par
      (Peca, Coordenadas) ->
      -- | Recebe uma lista de pares
      [(Peca, Coordenadas)] ->
      -- | Dá um valor Inteiro
      Int
    contador _ [] = 0
    contador (a, _) (x : xs)
      | a == fst x = 1 + contador (a, (0, 0)) xs
      | otherwise = contador (a, (0, 0)) xs

-- | Função: Verifica se existem elementos com as mesmas posções
--
-- == Código
-- @
-- norepete :: [(Peca, Coordenadas)] -> Bool
-- norepete [] = True
-- norepete ((p, c) : t) = (thereisnt c t && norepete t) && emptyspace ((p, c) : t)
--  where
--    thereisnt :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
--    thereisnt c1 [] = True
--    thereisnt c1 ((p, c) : t) = c1 /= c && thereisnt c1 t
-- @
norepete ::
  -- | Recebe um par de Coordenadas
  [(Peca, Coordenadas)] ->
  -- | Caso existam elementos repetidos retorna False
  Bool
norepete [] = True
norepete ((p, c) : t)
  | thereisnt c t && norepete t = True
  | otherwise = False
  where
    thereisnt ::
      -- | Recebe uma posição
      Coordenadas ->
      -- | Verifica se existem alguma igual
      [(Peca, Coordenadas)] ->
      -- | Caso exista, retorna False
      Bool
    thereisnt c1 [] = True
    thereisnt c1 ((p, c) : t) = c1 /= c && thereisnt c1 t

-- | Função: Verifica se existe algum espaço vazio
--
-- == Código
-- @
-- emptyspace :: [(Peca, Coordenadas)] -> Bool
-- emptyspace [] = False
-- emptyspace ((p, c) : t)
--  | p == Vazio || emptyspace t || tamanho (maiorCoordenada ((p, c) : t)) > length ((p, c) : t) = True
--  | otherwise = False
--  where
--    tamanho :: (Int, Int) -> Int
--    tamanho (x, y) = (x + 1) * (y + 1)
-- @
emptyspace ::
  -- | Recebe uma Lista
  [(Peca, Coordenadas)] ->
  -- | Se existir algum espaço vazio retorna True
  Bool
emptyspace [] = False
emptyspace l
  | p == Vazio || emptyspace t || tamanho (maiorCoordenada ((p, c) : t)) > length ((p, c) : t) = True
  | otherwise = False
  where
    ((p, c) : t) = l
    tamanho ::
      -- | Rece o maior x e o maior y
      (Int, Int) ->
      -- | Calcula a área do mapa
      Int
    tamanho (x, y) = (x + 1) * (y + 1)

-- | Função: Valida as Caixas - Verifica se cada caixa está pousada num bloco ou numa outra caixa
--
-- == Código
-- @
-- checkcaixas :: [(Peca,Coordenadas)] -> Bool
-- checkcaixas l
--  | (c,d+1) `elem` coordsblocosecaixas l = checkcaixasaux xs (coordsblocosecaixas l)
--  | otherwise = False
--  where ((c,d):xs) = coordscaixas l
--        checkcaixasaux :: [Coordenadas] -> [Coordenadas] -> Bool
--        checkcaixasaux [] l = True
--        checkcaixasaux l [] = False
--        checkcaixasaux ((c,d):xs) b
--          | (c,d+1) `elem` b = checkcaixasaux xs b
--          | otherwise = False
-- @
checkcaixas ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Se todas as caixas foram válidas retorna True
  Bool
checkcaixas [] = False
checkcaixas l
  | (c, d + 1) `elem` coordsblocosecaixas l = checkcaixasaux xs (coordsblocosecaixas l)
  | otherwise = False
  where
    ((c, d) : xs) = coordscaixas l
    checkcaixasaux ::
      -- | Recebe as coordenadas das caixas
      [Coordenadas] ->
      -- | Recebe as coordendas dos blocos e das caixas
      [Coordenadas] ->
      -- | Se todas as caixas forma válidas retorna True
      Bool
    checkcaixasaux [] l = True
    checkcaixasaux l [] = False
    checkcaixasaux ((c, d) : xs) b
      | (c, d + 1) `elem` b = checkcaixasaux xs b
      | otherwise = False

-- | Função Auxiliar Específica deste módulo
--
-- == Código
-- @
-- coordscaixas :: [(Peca, Coordenadas)] -> [Coordenadas] -- Dá as coordenadas das Caixas
-- coordscaixas [] = []
-- coordscaixas ((p, c) : t)
--  | p == Caixa = c : coordscaixas t
--  | otherwise = coordscaixas t
-- @
coordscaixas ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Dá as coordenadas das Caixas
  [Coordenadas]
coordscaixas [] = []
coordscaixas ((p, c) : t)
  | p == Caixa = c : coordscaixas t
  | otherwise = coordscaixas t

-- | Função Auxiliar Específica deste módulo
--
-- == Código
-- @
-- coordsblocosecaixas :: [(Peca, Coordenadas)] -> [Coordenadas] -- Dá uma lista sem as caixas e sem a porta
-- coordsblocosecaixas [] = []
-- coordsblocosecaixas ((p, c) : t)
--  | p == Porta = coordsblocosecaixas t
--  | otherwise = c : coordsblocosecaixas t
-- @
coordsblocosecaixas ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  [Coordenadas] -- Dá uma lista sem as caixas e sem a porta
coordsblocosecaixas [] = []
coordsblocosecaixas ((p, c) : t)
  | p == Porta = coordsblocosecaixas t
  | otherwise = c : coordsblocosecaixas t
