module Tarefa3_2021li1g021_Spec where

import Test.HUnit
import Tarefa3_2021li1g021
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m2e1"  ~: "    X <\n    X X\n      X\nP   C X\nXXXXXXX" ~=? show m2e1
    , "Tarefa 3 - Teste Imprime Jogo m2e2" ~: "    X  \n    X X\n      X\nP < C X\nXXXXXXX" ~=? show m2e2
    , "Tarefa 3 - Teste Imprime Jogo m3e3" ~: "    X  \n      X\n      X\nP   C>X\nXXXXXXX" ~=? show m3e3
    ]

 