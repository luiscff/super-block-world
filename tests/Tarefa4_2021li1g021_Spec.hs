module Tarefa4_2021li1g021_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g021
import Tarefa4_2021li1g021
import Fixtures

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=?  moveJogador m1e1 AndarDireita
    , "Tarefa 4 - Teste Move m1e1 Trepar quando não pode trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste Move m1e4 Trepar quando tem parede alta" ~: m1e4 ~=? moveJogador m1e4 Trepar
    , "Tarefa 4 - Teste Move m1e2 Trepar" ~: Jogo m1r (Jogador (4,2) Oeste False) ~=? moveJogador (Jogo m1r (Jogador (5,3) Oeste False)) Trepar
    , "Tarefa 4 - Teste Move m5e1 Trepar com caixa na mão quando não pode trepar" ~: m5e1 ~=? moveJogador m5e1 Trepar
    , "Tarefa 4 - Teste Move m3e1 Trepar com caixa na mão" ~: Jogo m3r (Jogador (4,2) Oeste True) ~=? moveJogador m3e1 Trepar
    , "Tarefa 4 - Teste Move m1e1 InterageCaixa quando não tem caixa para pegar" ~: m1e1 ~=?  moveJogador m1e1 InterageCaixa
    , "Tarefa 4 - Teste Move m5e2 quando não pode pegar numa caixa porque há um obstáculo em cima do jogador" ~: m5e2 ~=? moveJogador m5e2 InterageCaixa
    , "Tarefa 4 - Teste Move m4e1 quando não pode pegar numa caixa porque há um obstáculo em cima da caixa" ~: m4e1 ~=? moveJogador m4e1 InterageCaixa
    , "Tarefa 4 - Teste Move m5e3 quando não pode pousar caixa" ~: m5e3 ~=? moveJogador m5e3 InterageCaixa
    , "Tarefa 4 - Teste Move m1e5 quando não pode pousar caixa porque tem uma porta" ~: m1e5 ~=? moveJogador m1e5 InterageCaixa
    , "Tarefa 4 - Teste Move quando pega numa caixa à esquerda" ~: Jogo
    [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    ] 
    (Jogador (4,3) Oeste True) ~=? moveJogador (Jogo [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (4,3) Oeste False )) InterageCaixa
    , "Tarefa 4 - Teste Move m5e1 InterageCaixa quando empilha uma caixa" ~: Jogo
    [[Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Caixa, Caixa, Vazio, Bloco],
    [Porta, Vazio, Vazio, Bloco, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    ]
    (Jogador (5,3) Oeste False) ~=? moveJogador m5e1 InterageCaixa
    , "Tarefa 4 - Teste Move m1e2 Queda de caixa" ~: Jogo
    [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    ] 
    (Jogador (6,0) Oeste False) ~=? moveJogador m1e3 InterageCaixa
    , "Tarefa 4 - Teste movimentos andar e caminho bloqueado m1e2" ~: Jogo m1r (Jogador (3, 3) Este False) ~=?  correrMovimentos m1e2 [AndarEsquerda, AndarDireita, AndarDireita, AndarDireita]
    , "Tarefa 4 - Teste movimentos andar e cair m1e1" ~: Jogo m1r (Jogador (5,3) Oeste False) ~=? correrMovimentos m1e1 [AndarEsquerda, AndarEsquerda, AndarEsquerda]
    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Tarefa 4 - Teste movimentos m1e2 pegar numa caixa" ~: Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este True) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    , "Tarefa 4 - Teste movimentos m1e2 pegar e pousar uma caixa" ~:
      Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]
    ]