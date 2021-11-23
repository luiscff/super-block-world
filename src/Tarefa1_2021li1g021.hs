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

{- Tarefa 1: Verificar Mapa através de um conjuntos de funções interligadas -}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa = checkporta

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
  | contador (Porta, (0, 0)) ((p, c) : t) == 1 = norepete ((p, c) : t)
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
norepete ((p, c) : t) = (thereisnt c t && norepete t) && emptyspace ((p, c) : t)
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
emptyspace ((p, c) : t)
  | p == Vazio || emptyspace t || tamanho (maiorCoordenada ((p, c) : t)) > length ((p, c) : t) = True
  | otherwise = False
  where
    tamanho ::
      -- | Rece o maior x e o maior y
      (Int, Int) ->
      -- | Calcula a área do mapa
      Int
    tamanho (x, y) = (x + 1) * (y + 1)

caixas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -- Dá uma lista com apenas as Caixas
caixas [] = []
caixas ((p, c) : t)
  | p == Caixa = (p, c) : caixas t
  | otherwise = caixas t

removecaixaseporta :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -- Dá uma lista sem as caixas e sem a porta
removecaixaseporta [] = []
removecaixaseporta ((p, c) : t)
  | p == Caixa = removecaixaseporta t
  | p == Porta = removecaixaseporta t
  | otherwise = (p, c) : removecaixaseporta t

checkcaixas :: [(Peca, Coordenadas)] -> Bool
checkcaixas [] = True
checkcaixas l
  | x == x1 && y == y1 + 1 && checkcaixasaux t t1 = True
  | x == x1 && y /= y1 + 1 = checkcaixasaux [(p, (x, y))] t1
  | x /= x1 = checkcaixasaux [(p, (x, y))] t1
  | otherwise = False
  where
    (p, (x, y)) : t = caixas l
    (p1, (x1, y1)) : t1 = removecaixaseporta l

checkcaixasaux :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
checkcaixasaux _ [] = False
checkcaixasaux a b
  | x == x1 && y == y1 + 1 && checkcaixasaux t t1 = True
  | x /= x1 = checkcaixasaux a t1
  | otherwise = False
  where
    (p, (x, y)) : t = a
    (p1, (x1, y1)) : t1 = b

{-
mapabase :: [(Peca, Coordenadas)] -> Bool
mapabase [] = False
mapabase l = mapabaseaux [menorElemY l] (outroselementos l)
             where outroselementos :: [(Peca,Coordenadas)] -> [(Peca, Coordenadas)]
                   outroselementos [] = []
                   outroselementos ((p,c):t) = if (p,c) == menorElemY ((p,c):t) then t else (p,c) : outroselementos t

mapabaseaux :: [(Peca,Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
mapabaseaux (p,(x,y)) (p1,(x1,y1):t)                                        -- Atenção que o menor elementos pode não ser um Bloco (mas terá de ser um bloco obrigatoriamente)
  | p1 == Bloco && x==x1 && (y1 == y+1 ||

-}