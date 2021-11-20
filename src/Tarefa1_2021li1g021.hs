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

norepete :: [(Peca, Coordenadas)] -> Bool
norepete [] = True -- Ver quando tenho 2 objetos com a mesma coordenada deve dar True
norepete ((p, c) : t) = (thereisnt c t && norepete t) && emptyspace ((p, c) : t)
  where
    thereisnt :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
    thereisnt c1 [] = True
    thereisnt c1 ((p, c) : t) = c1 /= c && thereisnt c1 t

emptyspace :: [(Peca, Coordenadas)] -> Bool
emptyspace [] = False
emptyspace ((p, c) : t)
  | p == Vazio || emptyspace t || tamanho (maiorCoordenada ((p, c) : t)) > length ((p, c) : t) = True
  | otherwise = False
  where
    tamanho :: (Int, Int) -> Int
    tamanho (x, y) = (x + 1) * (y + 1)

caixas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]               -- Dá uma lista com apenas as Caixas
caixas [] =[]
caixas ((p,c):t)
  | p == Caixa = (p,c) : caixas t
  | otherwise = caixas t

removecaixaseporta :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]    -- Dá uma lista sem as caixas e sem a porta
removecaixaseporta [] = []
removecaixaseporta ((p,c):t)
  | p == Caixa = removecaixaseporta t
  | p == Porta = removecaixaseporta t
  | otherwise = (p,c) : removecaixaseporta t


checkcaixas :: [(Peca, Coordenadas)] -> Bool
checkcaixas [] = True
checkcaixas l
  | x == x1 && y == y1+1 && checkcaixasaux t t1 = True
  | x == x1 && y /= y1+1 = checkcaixasaux [(p,(x,y))] t1
  | x /= x1 = checkcaixasaux [(p,(x,y))] t1
  | otherwise = False
  where (p,(x,y)):t = caixas l
        (p1,(x1,y1)):t1 = removecaixaseporta l

checkcaixasaux :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
checkcaixasaux _ [] = False
checkcaixasaux a b
  | x == x1 && y == y1+1 = True
  | x /= x1 = checkcaixasaux a t
  | otherwise = False
  where [(p,(x,y))] = a
        (p1,(x1,y1)):t = b


mapabase :: [(Peca, Coordenadas)] -> Bool
mapabase [] = False
mapabase l = mapabaseaux [menorElemY l] (outroselementos l)
             where outroselementos :: [(Peca,Coordenadas)] -> [(Peca, Coordenadas)]
                   outroselementos [] = []
                   outroselementos ((p,c):t) = if (p,c) == menorElemY ((p,c):t) then t else (p,c) : outroselementos t

mapabaseaux :: [(Peca,Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
mapabaseaux (p,(x,y)) (p1,(x1,y1):t)                                        -- Atenção que o menor elementos pode não ser um Bloco (mas terá de ser um bloco obrigatoriamente)
  | p1 == Bloco && x==x1 && (y1 == y+1 || 