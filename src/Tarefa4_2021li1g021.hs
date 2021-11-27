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
    |mov == AndarDireita = andarDir mapa (Jogador (x,y) dir bool)
    |mov == AndarEsquerda = andarEsq mapa (Jogador (x,y) dir bool)
    |mov == Trepar = trepar mapa (Jogador (x,y) dir bool)
--    |mov == InterageCaixa -- && o q tiver em cima do jogador e da caixa for Vazio 

trepar mapa (Jogador (x,y) dir bool) --faz o jogador trepar --falta fazer as coisas com a caixa na mão
    |dir == Este && elem (Bloco,(x+1,y)) (desconstroiMapa mapa) == False && elem (Caixa,(x+1,y)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x,y) dir bool)  --quando ta virado pa direita e o elem à direita n existe
    |dir == Oeste && elem (Bloco,(x-1,y)) (desconstroiMapa mapa) == False && elem (Caixa,(x-1,y)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x,y) dir bool) --quando ta virado pa esquerda e o elem à esquerda n existe
    |dir == Este && (elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa)) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool) --se tiver 2 blocos ou caixas empilhados à direita
    |dir == Oeste && (elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa)) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)   --se tiver 2 blocos ou caixas empilhados à esquerda
    |dir == Este && bool == True && elem (Bloco,(x+1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool) --se tiver a carregar uma caixa e n houver 2 espaços por cima do bloco q quer trepar à direita
    |dir == Oeste && bool == True && elem (Bloco,(x-1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool) --se tiver a carregar uma caixa e n houver 2 espaços por cima do bloco q quer trepar à esquerda
    |dir == Este = Jogo mapa (Jogador (x+1,y-1) Este bool) --trepa para a direita
    |dir == Oeste = Jogo mapa (Jogador (x-1,y-1) Oeste bool) --trepa para a esquerda

andarDir mapa (Jogador (x2,y2) dir bool) --anda 1 vez para a direita
    |fst(maiorCoordenada (desconstroiMapa mapa)) == x2 || elem (Bloco,(x2 + 1,y2)) (desconstroiMapa mapa) || elem (Caixa,(x2 + 1,y2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x2,y2) Este bool) --se estiver na parte mais à direita do mapa ou se tiver algo a bloquear o caminho à direita, apenas vira o jogador para a direita
    |bool == True && ( elem (Bloco,(x2+1,y2-1)) (desconstroiMapa mapa) == True ) = Jogo mapa (Jogador (x2,y2) Este bool) --se tiver a segurar caixa e houver bloco à direita e acima, não pode andar
    |elem (Caixa,(x2 + 1,y2 + 1)) (desconstroiMapa mapa) == False && elem (Bloco,(x2 + 1,y2 + 1)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x2 + 1, (ondeVaiCair (x2 + 1) y2 (desconstroiMapa mapa) []) - 1) Este bool) --quando o jogador cai
    |otherwise = Jogo mapa (Jogador (x2 + 1,y2) Este bool)

andarEsq mapa (Jogador (x2,y2) dir bool) --anda 1 vez para a esquerda
    |x2 == 0 || elem (Bloco,(x2 -1 ,y2)) (desconstroiMapa mapa) || elem (Caixa,(x2 - 1,y2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x2,y2) Oeste bool) --se estiver na parte mais à esquerda do mapa ou se tiver algo a bloquear o caminho à esquerda, apenas vira o jogador para a esquerda
    |bool == True && ( elem (Bloco,(x2-1,y2-1)) (desconstroiMapa mapa) == True ) = Jogo mapa (Jogador (x2,y2) Oeste bool) --se tiver a segurar caixa e houver bloco à esquerda e acima, não pode andar
    |elem (Caixa,(x2 - 1,y2 + 1)) (desconstroiMapa mapa) == False && elem (Bloco,(x2 - 1,y2 + 1)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x2 - 1, (ondeVaiCair (x2 - 1) y2 (desconstroiMapa mapa) []) - 1) Oeste bool) --quando o jogador cai
    |otherwise = Jogo mapa (Jogador (x2 - 1,y2) Oeste bool)

ondeVaiCair _ _ [] ac = snd(snd(menorElemY ac)) --vai dar o Y do bloco onde o jogador vai aterrar
ondeVaiCair x y ((p,(x2,y2)):t) ac 
    |x == x2 && y2 > (y + 1) && (p == Bloco || p == Caixa) = ondeVaiCair x y t (ac ++ [(p,(x2,y2))])
    |otherwise = ondeVaiCair x y t ac

moveJogadorAUX jogo mov ac 
    |ac == [] = moveJogador jogo mov
    |otherwise = correrMovimentos (moveJogador jogo mov) ac 

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo (m:ms)
    |(m:ms) == [] = jogo
    |otherwise = moveJogadorAUX jogo m ms