module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m5r :: Mapa
m5r =
  [ [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Bloco, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m3r :: Mapa
m3r =
  [ [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m4r :: Mapa
m4r =
  [ [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Bloco, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m1e3 :: Jogo
m1e3 = Jogo m1r (Jogador (6, 0) Oeste True)

m1e4 :: Jogo 
m1e4 =  Jogo m1r (Jogador (5,3) Este False)

m1e5 :: Jogo
m1e5 = Jogo m1r (Jogador (1,3) Oeste True)

m5e1 :: Jogo
m5e1 = Jogo m5r (Jogador (5,3) Oeste True)

m5e2 :: Jogo
m5e2 = Jogo m5r (Jogador (4,2) Oeste False)

m5e3 :: Jogo
m5e3 = Jogo m5r (Jogador (2,3) Este True)

m3e1 :: Jogo
m3e1 = Jogo m3r (Jogador (5,3) Oeste True)

m4e1 :: Jogo
m4e1 = Jogo m4r (Jogador (4,2) Oeste False)