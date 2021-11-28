module Tarefa1_2021li1g021_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g021
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: True ~=? validaPotencialMapa m1
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: False ~=? validaPotencialMapa []
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: False  ~=? validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))]
    , " Tarefa 1 - Teste Valida Mapa sem Espaços Vazios" ~: False ~=? validaPotencialMapa [(Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (0,3))]
    , " Tarefa 1 - Teste Valida Mapa com Peca Vazio" ~: True ~=? validaPotencialMapa [(Bloco, (0,1)), (Bloco, (0,2)), (Vazio, (0,3)), (Porta, (1,0))]
    , " Tarefa 1 - Teste Valida Mapa com Caixas" ~: True ~=? validaPotencialMapa [(Porta, (0,0)), (Caixa, (0,1)), (Bloco, (0,2))] 
    , " Tarefa 1 - Teste Valida Mapa com Caixa em cima de outra Caixa " ~: True ~=? validaPotencialMapa [(Porta, (0,0)), (Caixa, (0,1)), (Caixa, (0,2)), (Bloco, (0,3))]
    ]
