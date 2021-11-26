{- |
Module      : Tarefa4_2021li1g021
Description : Movimentação do personagem
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g021 where

import LI12122
import FuncoesUteis
import Tarefa2_2021li1g021


moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador (x,y) dir bool)) mov
    |mov == AndarDireita = andarDir mapa (Jogador (x,y) Este bool)
    |mov == AndarEsquerda = andarEsq mapa (Jogador (x,y) Este bool)
--    |mov == trepar -- && o q tiver em cima for Vazio
--    |mov == InterageCaixa -- && o q tiver em cima do jogador e da caixa for Vazio 

andarDir mapa (Jogador (x2,y2) dir bool) --falta a queda
    |fst(maiorCoordenada (desconstroiMapa mapa)) == x2 || elem (Bloco,(x2 + 1,y2)) (desconstroiMapa mapa) || elem (Caixa,(x2 + 1,y2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x2,y2) Este bool) --se estiver na parte mais à direita do mapa ou se tiver algo a bloquear o caminho à direita, apenas vira o jogador para a direita
    |elem (Caixa,(x2 + 1,y2 + 1)) (desconstroiMapa mapa) == False && elem (Bloco,(x2 + 1,y2 + 1)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x2 + 1, (ondeVaiCair (x2 +1) y2 (desconstroiMapa mapa) []) ) Este bool)
    |otherwise = Jogo mapa (Jogador (x2 + 1,y2) Este bool)

andarEsq mapa (Jogador (x2,y2) dir bool)
    |x2 == 0 || elem (Bloco,(x2 -1 ,y2)) (desconstroiMapa mapa) || elem (Caixa,(x2 - 1,y2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x2,y2) Oeste bool) --se estiver na parte mais à direita do mapa ou se tiver algo a bloquear o caminho à direita, apenas vira o jogador para a direita
    |otherwise = Jogo mapa (Jogador (x2 - 1,y2) Oeste bool)

-- trepar mapa (Jogador (x,y) Este bool)

ondeVaiCair _ _ [] ac = snd(snd(menorElemY ac)) --vai me dar o Y só
ondeVaiCair x y ((p,(x2,y2)):t) ac -- ja esta no X certo e justamente antes de cair, tb ja esta confirmado que vai cair
    |x == x2 && y2 < (y + 1) && (p == Bloco || p == Caixa) = ondeVaiCair x y t (ac ++ [(p,(x2,y2))])--sacar o Bloco ou Caixa com menor Y destes (ou seja, o mais acima)(onde o jogador vai aterrar)
    |otherwise = ondeVaiCair x y t ac

moveJogadorAUX jogo mov ac 
    |ac == [] = moveJogador jogo mov
    |otherwise = correrMovimentos (moveJogador jogo mov) ac 

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo (m:ms)
    |(m:ms) == [] = jogo
    |otherwise = moveJogadorAUX jogo m ms