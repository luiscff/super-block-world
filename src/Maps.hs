module Maps where 

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 = [ (Porta, (0, 3)),
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
  (Bloco, (6, 1)),
  (Bloco, (12,9))]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste True)

m2e1 = Jogo m2r (Jogador (6, 0) Oeste True)

m2r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
 [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
 [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
 [Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
 [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
 [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]]