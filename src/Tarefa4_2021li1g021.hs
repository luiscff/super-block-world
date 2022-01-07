{- |
Module      : Tarefa4_2021li1g021

Description : Movimentação do personagem

Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.-}
module Tarefa4_2021li1g021 where

import LI12122
import FuncoesUteis
import Tarefa2_2021li1g021
{- | A função 'moveJogador' chama uma função que aplica o movimento ao jogo, seja ele qual for -}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador (x,y) dir bool)) mov
    |mov == AndarDireita = andarDir mapa (Jogador (x,y) dir bool)
    |mov == AndarEsquerda = andarEsq mapa (Jogador (x,y) dir bool)
    |mov == Trepar = trepar mapa (Jogador (x,y) dir bool)
    |mov == InterageCaixa = interageCaixa mapa (Jogador (x,y) dir bool)
{- | A função 'interageCaixa' chama uma função que pega numa caixa ou que pousa uma caixa dependendo da situação-}
interageCaixa :: Mapa -> Jogador -> Jogo
interageCaixa mapa (Jogador (x,y) dir bool)
    |bool == True = pousaCaixa mapa (Jogador (x,y) dir bool)
    |bool == False = pegaCaixa mapa (Jogador (x,y) dir bool)

pegaCaixa :: Mapa -> Jogador -> Jogo
{- | A função 'pegaCaixa' faz o jogador pegar na caixa que está na sua frente, se possível

__não pega quando não tem uma caixa à frente__

@
    |dir == Este && (elem (Caixa,(x+1,y)) (desconstroiMapa mapa) == False) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && (elem (Caixa,(x-1,y)) (desconstroiMapa mapa) == False) = Jogo mapa (Jogador (x,y) dir bool)
@

__não pega quando a caixa tem um obstáculo em cima__

@
    |dir == Este && (elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa)) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && (elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa)) = Jogo mapa (Jogador (x,y) dir bool) 
@

__não pega quando o jogador tem obstáculo em cima__

@
    |elem (Bloco,(x,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
@
-}
pegaCaixa mapa (Jogador (x,y) dir bool)
    |dir == Este && (elem (Caixa,(x+1,y)) (desconstroiMapa mapa) == False) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && (elem (Caixa,(x-1,y)) (desconstroiMapa mapa) == False) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Este && (elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa)) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && (elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa)) = Jogo mapa (Jogador (x,y) dir bool) 
    |elem (Bloco,(x,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Este = Jogo (tiraCaixaMapa (desconstroiMapa mapa) (x+1,y) []) (Jogador (x,y) dir True)
    |dir == Oeste = Jogo (tiraCaixaMapa (desconstroiMapa mapa) (x-1,y) []) (Jogador (x,y) dir True)
{- | a função 'tiraCaixaMapa' retira uma determinada caixa do mapa

== Exemplos de utilização

>>> tiraCaixaMapa [(Porta,(2,5)), (Bloco,(3,2)), (Caixa,(2,3)), (Caixa,(1,2))] (2,3) []

>[[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Vazio,Caixa,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Porta,Vazio]]-}
tiraCaixaMapa :: [(Peca,Coordenadas)] -> Coordenadas -> [(Peca,Coordenadas)] -> Mapa
tiraCaixaMapa ((p,(x,y)):t) (x2,y2) l
    |x == x2 && y == y2 = constroiMapa (l ++ [(Vazio,(x,y))] ++ t)
    |otherwise = tiraCaixaMapa t (x2,y2) (l ++ [(p,(x,y))])
{- | A função 'pousaCaixa' faz o jogador pousar a caixa que carrega, se for possível

__quando não pode pousar a caixa poque não existe chão__

@
    |dir == Este && (elem (Caixa,(x+1,y-1))(desconstroiMapa mapa) || elem (Bloco,(x+1,y-1))(desconstroiMapa mapa))  = Jogo mapa (Jogador (x,y) dir bool) 
    |dir == Oeste && (elem (Caixa,(x-1,y-1))(desconstroiMapa mapa) || elem (Bloco,(x-1,y-1))(desconstroiMapa mapa))  = Jogo mapa (Jogador (x,y) dir bool)
@
@
    |dir == Este && x == (maiorX (desconstroiMapa mapa)) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && x == 0 = Jogo mapa (Jogador (x,y) dir bool)
@
__quando empilha a caixa em cima de outra coisa__

@
    |dir == Este && (elem (Caixa,(x+1,y))(desconstroiMapa mapa) || elem (Bloco,(x+1,y))(desconstroiMapa mapa)) && (elem (Porta,(x+1,y-1)) (desconstroiMapa mapa) == False) = Jogo (empilhaCaixaDir (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |dir == Oeste && (elem (Caixa,(x-1,y))(desconstroiMapa mapa) || elem (Bloco,(x-1,y))(desconstroiMapa mapa)) && (elem (Porta,(x-1,y-1)) (desconstroiMapa mapa) == False) = Jogo (empilhaCaixaEsq (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
@

__quando pousa a caixa (à direita ou à esquerda)__

@
    |dir == Este && (elem (Porta,(x+1,y)) (desconstroiMapa mapa) == False) = Jogo (pousaDir (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |dir == Oeste && (elem (Porta,(x-1,y)) (desconstroiMapa mapa) == False) = Jogo (pousaEsq (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
@
-}
pousaCaixa :: Mapa -> Jogador -> Jogo
pousaCaixa mapa (Jogador (x,y) dir bool)
    |dir == Este && (elem (Caixa,(x+1,y-1))(desconstroiMapa mapa) || elem (Bloco,(x+1,y-1))(desconstroiMapa mapa))  = Jogo mapa (Jogador (x,y) dir bool) 
    |dir == Oeste && (elem (Caixa,(x-1,y-1))(desconstroiMapa mapa) || elem (Bloco,(x-1,y-1))(desconstroiMapa mapa))  = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Este && x == (maiorX (desconstroiMapa mapa)) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && x == 0 = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Este && (elem (Caixa,(x+1,y))(desconstroiMapa mapa) || elem (Bloco,(x+1,y))(desconstroiMapa mapa)) && (elem (Porta,(x+1,y-1)) (desconstroiMapa mapa) == False) = Jogo (empilhaCaixaDir (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |dir == Oeste && (elem (Caixa,(x-1,y))(desconstroiMapa mapa) || elem (Bloco,(x-1,y))(desconstroiMapa mapa)) && (elem (Porta,(x-1,y-1)) (desconstroiMapa mapa) == False) = Jogo (empilhaCaixaEsq (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |dir == Este && (elem (Caixa,(x+1,y+1))(desconstroiMapa mapa) == False) && (elem (Bloco,(x+1,y+1)) (desconstroiMapa mapa) == False) = Jogo (quedaCaixaDir (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |dir == Oeste && (elem (Caixa,(x-1,y+1))(desconstroiMapa mapa) == False) && (elem (Bloco,(x-1,y+1)) (desconstroiMapa mapa) == False) = Jogo (quedaCaixaEsq (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |dir == Este && (elem (Porta,(x+1,y)) (desconstroiMapa mapa) == False) = Jogo (pousaDir (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |dir == Oeste && (elem (Porta,(x-1,y)) (desconstroiMapa mapa) == False) = Jogo (pousaEsq (desconstroiMapa mapa) (x,y)) (Jogador (x,y) dir False)
    |otherwise = Jogo mapa (Jogador (x,y) dir bool)
{- | a função 'pousaDir' acrescenta uma caixa no mapa na coordenada à direita do jogador

== Exemplos de utilização

>>> pousaDir [(Porta,(2,5)), (Bloco,(3,2)), (Caixa,(2,3)), (Caixa,(1,2))] (3,3)

>[[Vazio,Vazio,Vazio,Vazio,Vazio]
>,[Vazio,Vazio,Vazio,Vazio,Vazio]
>,[Vazio,Caixa,Vazio,Bloco,Vazio]
>,[Vazio,Vazio,Caixa,Vazio,Caixa]
>,[Vazio,Vazio,Porta,Vazio,Vazio]]-}
pousaDir :: [(Peca,Coordenadas)] -> Coordenadas -> Mapa
pousaDir l (x,y) = constroiMapa (l ++ [(Caixa,(x+1,y))])
{- | a função 'pousaEsq' acrescenta uma caixa no mapa na coordenada à esquerda do jogador

== Exemplos de utilização

>>> pousaEsq [(Porta,(2,5)), (Bloco,(3,2)), (Caixa,(2,3)), (Caixa,(1,2))] (1,1)

>[[Vazio,Vazio,Vazio,Vazio]
>,[Caixa,Vazio,Vazio,Vazio]
>,[Vazio,Caixa,Vazio,Bloco]
>,[Vazio,Vazio,Caixa,Vazio]
>,[Vazio,Vazio,Porta,Vazio]]-}
pousaEsq :: [(Peca,Coordenadas)] -> Coordenadas -> Mapa
pousaEsq l (x,y) = constroiMapa (l ++ [(Caixa,(x-1,y))])
{- | a função 'quedaCaixaDir' dá o mapa depois do jogador pousar a caixa à sua direita e depois desta cair, já que ocorre sempre que não existe chão à esquerda do jogador-}
quedaCaixaDir :: [(Peca,Coordenadas)] -> Coordenadas -> Mapa
quedaCaixaDir l (x,y) = constroiMapa (l ++ [(Caixa,(x+1, (ondeVaiCair (x+1) y l []) -1 ))])
{- | a função 'quedaCaixaEsq' dá o mapa depois do jogador pousar a caixa à sua esquerda e depois desta cair, já que ocorre sempre que não existe chão à esquerda do jogador-}
quedaCaixaEsq :: [(Peca,Coordenadas)] -> Coordenadas -> Mapa
quedaCaixaEsq l (x,y) = constroiMapa (l ++ [(Caixa,(x-1, (ondeVaiCair (x-1) y l []) -1 ))])
{- | a função 'empilhaCaixaDir' dá o mapa depois do jogador empilhar a caixa em cima de algo à direita -}
empilhaCaixaDir :: [(Peca,Coordenadas)] -> Coordenadas -> Mapa
empilhaCaixaDir l (x,y) = constroiMapa (l ++ [(Caixa,(x+1,y-1))])
{- | a função 'empilhaCaixaEsq' dá o mapa depois do jogador empilhar a caixa em cima de algo à esquerda -}
empilhaCaixaEsq :: [(Peca,Coordenadas)] -> Coordenadas -> Mapa
empilhaCaixaEsq l (x,y) = constroiMapa (l ++ [(Caixa,(x-1,y-1))])
{- | a função 'trepar' retorna o jogo depois do jogador trepar algo(se possível)

__quando o elemento diretamente ao lado do jogador n existe__

@
    |dir == Este && elem (Bloco,(x+1,y)) (desconstroiMapa mapa) == False && elem (Caixa,(x+1,y)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x,y) dir bool)  
    |dir == Oeste && elem (Bloco,(x-1,y)) (desconstroiMapa mapa) == False && elem (Caixa,(x-1,y)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x,y) dir bool)
@

__quando há  2 blocos ou caixas empilhados diretamente ao lado do jogador__

@
    |dir == Este && (elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa)) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && (elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa)) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
@

__quando o jogador estiver a carregar uma caixa e não houver 2 espaços por cima do bloco que o jogador quer trepar__

@
    |dir == Este && bool == True && elem (Bloco,(x+1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && bool == True && elem (Bloco,(x-1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
@

__quando o jogador trepa normalmente__

@
    |dir == Este = Jogo mapa (Jogador (x+1,y-1) Este bool)
    |dir == Oeste = Jogo mapa (Jogador (x-1,y-1) Oeste bool)
@
-}
trepar :: Mapa -> Jogador -> Jogo
trepar mapa (Jogador (x,y) dir bool)
    |dir == Este && elem (Bloco,(x+1,y)) (desconstroiMapa mapa) == False && elem (Caixa,(x+1,y)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && elem (Bloco,(x-1,y)) (desconstroiMapa mapa) == False && elem (Caixa,(x-1,y)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x,y) dir bool) 
    |dir == Este && (elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa)) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && (elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa)) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Este && bool == True && elem (Bloco,(x+1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Oeste && bool == True && elem (Bloco,(x-1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) dir bool)
    |dir == Este = Jogo mapa (Jogador (x+1,y-1) Este bool)
    |dir == Oeste = Jogo mapa (Jogador (x-1,y-1) Oeste bool)
{- | a função 'andarDir' faz o jogador virar-se para a direita e andar uma vez, se for possível-}
andarDir :: Mapa -> Jogador -> Jogo
andarDir mapa (Jogador (x2,y2) dir bool)
    |fst(maiorCoordenada (desconstroiMapa mapa)) == x2 || elem (Bloco,(x2 + 1,y2)) (desconstroiMapa mapa) || elem (Caixa,(x2 + 1,y2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x2,y2) Este bool)
    |bool == True && ( elem (Bloco,(x2+1,y2-1)) (desconstroiMapa mapa) == True ) = Jogo mapa (Jogador (x2,y2) Este bool)
    |elem (Caixa,(x2 + 1,y2 + 1)) (desconstroiMapa mapa) == False && elem (Bloco,(x2 + 1,y2 + 1)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x2 + 1, (ondeVaiCair (x2 + 1) y2 (desconstroiMapa mapa) []) - 1) Este bool)
    |otherwise = Jogo mapa (Jogador (x2 + 1,y2) Este bool)
{- | a função 'andarEsq' faz o jogador virar-se para a esquerda e andar uma vez, se for possível-}
andarEsq :: Mapa -> Jogador -> Jogo
andarEsq mapa (Jogador (x2,y2) dir bool)
    |x2 == 0 || elem (Bloco,(x2 -1 ,y2)) (desconstroiMapa mapa) || elem (Caixa,(x2 - 1,y2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x2,y2) Oeste bool)
    |bool == True && ( elem (Bloco,(x2-1,y2-1)) (desconstroiMapa mapa) == True ) = Jogo mapa (Jogador (x2,y2) Oeste bool)
    |elem (Caixa,(x2 - 1,y2 + 1)) (desconstroiMapa mapa) == False && elem (Bloco,(x2 - 1,y2 + 1)) (desconstroiMapa mapa) == False = Jogo mapa (Jogador (x2 - 1, (ondeVaiCair (x2 - 1) y2 (desconstroiMapa mapa) []) - 1) Oeste bool)
    |otherwise = Jogo mapa (Jogador (x2 - 1,y2) Oeste bool)
{- | a função 'ondeVaiCair' dá o y da coordenada onde algo (jogador ou caixa) vai aterrar-}
ondeVaiCair :: Int -> Int -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> Int
ondeVaiCair _ _ [] ac = snd(snd(menorElemY ac))
ondeVaiCair x y ((p,(x2,y2)):t) ac 
    |x == x2 && y2 > (y + 1) && (p == Bloco || p == Caixa) = ondeVaiCair x y t (ac ++ [(p,(x2,y2))])
    |otherwise = ondeVaiCair x y t ac
{- | a função 'moveJogadorAUX' é uma auxiliar da função 'correrMovimentos' que faz com que um jogador se mova várias vezes (fazendo com que a função 'moveJogador' execute várias vezes)-}
moveJogadorAUX :: Jogo -> Movimento -> [Movimento] -> Jogo
moveJogadorAUX jogo mov ac 
    |ac == [] = moveJogador jogo mov
    |otherwise = correrMovimentos (moveJogador jogo mov) ac 
{- | a função 'correrMovimentos' faz com que sejam executados vários movimentos a um jogo-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo (m:ms)
    |(m:ms) == [] = jogo
    |otherwise = moveJogadorAUX jogo m ms