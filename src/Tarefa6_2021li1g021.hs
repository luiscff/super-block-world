{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1gXXX where

import LI12122
import Tarefa2_2021li1g021

--resolveJogo :: Int -> Jogo -> Maybe [Movimento]
--resolveJogo 0 jogo = undefined --testar só se é possível acabar
--resolveJogo i jogo = undefined

{-
resolveJogo :: Jogo -> Maybe [Movimento]
resolveJogo (Jogo ((x:xs):ys) (Jogador (c1,c2) direc caixa))
    | direc == Este && caixa == False = resolveJogoaux (Jogo ((x:xs):ys)) (Jogador (c1,c2) Este False)
    | direc == Este && caixa == True = resolveJogoaux (Jogo ((x:xs):ys)) (Jogador (c1,c2) Este True)
    | direc == Oeste && caixa == False = resolveJogoaux (Jogo ((x:xs):ys)) (Jogador (c1,c2) Oeste False)
    | direc == Oeste && caixa == True = resolveJogoaux (Jogo ((x:xs):ys)) (Jogador (c1,c2) Oeste True)
    | otherwise = error "Incorreta identificação do Jogo"

-}



movimento :: Jogo -> Maybe [Movimento]
movimento (Jogo [] _) = Just []
movimento (Jogo map (Jogador (xj,yj) direc caixa))
    | checkchao (xj, (yj+1)) (xp, (yp+1)) (desconstroiMapa map) == True = contarpassos (xj, yj+1) (xp,yp+1)
    | otherwise = error "Incompleto"
    where (xp,yp) = (porta (desconstroiMapa map)) 


checkchao :: Coordenadas -> Coordenadas -> [(Peca,Coordenadas)] -> Bool
checkchao (xj,yj) (xp,yp) ((pe,(x,y)):xs) 
    | xj > xp = checkchaoaux 0 (xj,yj) (xp,yp) ((pe,(x,y)):xs)
    | xj < xp = checkchaoaux 1 (xj, yj) (xp, yp) ((pe, (x,y)):xs)
    | otherwise = True
    where
        checkchaoaux :: Int -> Coordenadas -> Coordenadas -> [(Peca,Coordenadas)] -> Bool
        checkchaoaux v cj cp [] = False
        checkchaoaux v (xbj,ybj) (xp,yp) l
            | v == 0 && xbj /= xp = elemaux (Bloco, (xbj-1,ybj)) l && checkchaoaux 0 (xbj-1,ybj) (xp,yp) l
            | v == 1 && xbj /= xp = elemaux (Bloco, (xbj+1,ybj)) l && checkchaoaux 1 (xbj+1,ybj) (xp,yp) l
            | v == 0 && xbj == xp || v == 1 && xbj == xp = True


        elemaux :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Bool
        elemaux p [] = False
        elemaux (p,(xbj,ybj)) ((pe,(x1,y1)):xs)
            | p == pe && xbj == x1 && ybj <= y1 = True
            | otherwise = elemaux (p, (xbj,ybj)) xs


porta :: [(Peca,Coordenadas)] -> Coordenadas
porta [] = error "Não existe Porta"
porta ((pe,(x1,y1)):xs)
    | pe == Porta = (x1,y1)
    | otherwise = porta xs


contarpassos :: Coordenadas -> Coordenadas -> Maybe [Movimento]
contarpassos (xj,yj) (xp,yp)
    | xj < xp = andardir (xp-xj)
    | xj > xp = andaresq (xp+xj-1)
    | xj == xj = Just []
    where
        andaresq :: Int -> Maybe [Movimento]
        andaresq 0 = Just []
        andaresq n
            | n /= 0 = Just (AndarEsquerda : andaresqaux (n-1))
         where 
                andaresqaux :: Int -> [Movimento]
                andaresqaux 0 = []
                andaresqaux n
                    | n /= 0 = (AndarEsquerda : andaresqaux (n-1))

        andardir :: Int -> Maybe [Movimento]
        andardir 0 = Just []
        andardir n
            | n /= 0 = Just (AndarDireita :  andardiraux (n-1))
            where 
                andardiraux :: Int -> [Movimento]
                andardiraux 0 = []
                andardiraux n
                    | n /= 0 = (AndarDireita : andardiraux (n-1))
