module Tarefa6_2021li1g021_Spec where

import Test.HUnit
import LI12122
import Tarefa6_2021li1g021
import Fixtures

testsT6 =
  test
    [ "Tarefa 6 - Teste Jogador na mesma Coordenada da Porta" ~: Just [] ~=? resolveJogo 0 m1e6
    , "Tarefa 6 - Teste Jogador noum coordenada superior à da Porta" ~: Just [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 10 mie1
    . "Tarefa 6 - Teste Impossível resolver o Mapa com um número baixo de movimentos" ~: Nothing ~=? resolveJogo 4 mie1
    , "Tarefa 6 - Teste para o Jogo m1e2" ~: Just [AndarEsquerda, AndarEsquerda] ~=? resolveJogo 5 m1e2
    , "Tarefa 6 - Teste para o Jogo m2e1" ~: Just [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 10 m2e1
    , "Tarefa 6 - Teste para o Jogo m2e2" ~: Just [AndarEsquerda, AndarEsquerda] ~=? resolveJogo 5 m2e2
    , "Tarefa 6 - Teste para o Jogo m3e1" ~: Just [AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 10 m3e1
    , "Tarefa 6 - Teste para o Jogo m3e2" ~: Just [AndarEsquerda, AndarEsquerda] ~=? resolveJogo 3 m3e2
    ]