{- |
Module      : Tarefa5_2021li1g021
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where
import LI12122
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Maps
import System.Exit (exitSuccess)
import Tarefa2_2021li1g021 (desconstroiMapa)
import Tarefa4_2021li1g021 (moveJogador)

data Menu = MP OpcoesIn | DentroJogo | PassouLvl OpcoesLvl | Gameover

instance Eq Menu where
  (==) (MP Jogar) (MP Jogar) = True
  (==) (MP Sair) (MP Sair) = True
  (==) (DentroJogo) (DentroJogo) = True
  (==) (PassouLvl Proximo) (PassouLvl Proximo) = True
  (==) (PassouLvl Leave) (PassouLvl Leave) = True
  (==) (Gameover) (Gameover) = True
  (==) _ _ = False

data OpcoesLvl = Proximo | Leave
data OpcoesIn = Jogar | Sair

type Estado = ([Picture] , Menu , Jogo)

jogadoresNaMsmPosicao :: Jogo -> Coordenadas -> Bool
jogadoresNaMsmPosicao (Jogo _ (Jogador (x1,y1) _ _)) (x2,y2)
  |x1 == x2 && y1 == y2 = True
  |otherwise = False

jogadorNaPorta :: Jogo -> Bool
jogadorNaPorta (Jogo mapa (Jogador (x,y) dir bool))
  |coordenadaPorta (desconstroiMapa mapa) == (x,y) = True
  |otherwise = False

coordenadaPorta :: [(Peca, Coordenadas)] -> Coordenadas --n há caso de paragem pq é obrigatório ter uma porta
coordenadaPorta ((p,(x,y)):t)
  |p == Porta = (x,y)
  |otherwise = coordenadaPorta t

desenhaEstado :: Estado -> IO Picture
desenhaEstado e@(imagens, menu, jogo)
  |menu == MP Jogar = return $ imagens !! 5
  |menu == MP Sair = return $ imagens !! 6
  |menu == DentroJogo = return $ desenhaJogo e jogo
  |menu == (PassouLvl Proximo) = return $ imagens !! 7
  |menu == (PassouLvl Leave) = return $ imagens !! 8
  |menu == Gameover = return $ imagens !! 9
  |otherwise = return Blank

desenhaJogo :: Estado -> Jogo -> Picture
desenhaJogo e (Jogo mapa@(a:t) jogador) = (Translate x y (Pictures [desenhaMapa e (desconstroiMapa mapa), desenhaJogador e jogador]))
              where
                x = (fromIntegral((length a)*(-50)))/2.0 --50 é o nº de píxeis das imagens
                y = (fromIntegral((length mapa)*(50)))/2.0

desenhaMapa :: Estado -> [(Peca, Coordenadas)] -> Picture
desenhaMapa e [] = Blank
desenhaMapa e@(imagens,menu,jogo) ((p,(x,y)):t)
  |p == Bloco = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 0)), desenhaMapa e t]
  |p == Caixa = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 1)), desenhaMapa e t]
  |p == Porta = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 2)), desenhaMapa e t]
  
desenhaJogador :: Estado -> Jogador -> Picture
desenhaJogador e@(imagens,menu, Jogo mapa jogador) (Jogador (x,y) dir bool)
  |dir == Este && bool == True = Pictures [translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 3), translate (fromIntegral (x*50)) (fromIntegral ((y-1)*(-50))) (imagens !! 1)]
  |dir == Oeste && bool == True = Pictures [translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 4), translate (fromIntegral (x*50)) (fromIntegral ((y-1)*(-50))) (imagens !! 1)]
  |dir == Este  = translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 3)
  |dir == Oeste = translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 4)

inputManager :: Event -> Estado -> IO Estado
inputManager (EventKey (Char 'p') Down _ _ ) (i, DentroJogo, j) --cheat code que passa diretamente po prox nivel desde que o jogador não ande
  |j == jg1 = return (i, DentroJogo, jg2)
  |j == jg2 = return (i, DentroJogo, jg3)
  |j == jg3 = return (i, DentroJogo, jg4)
  |j == jg4 = return (i, DentroJogo, jg5)
  |j == jg5 = return (i, Gameover, j)

inputManager (EventKey (Char 'o') Down _ _ ) (i, DentroJogo, j) --cheat code que passa diretamente po nivel anterior desde que o jogador não ande
  |j == jg1 = return (i, MP Jogar, jg1)
  |j == jg2 = return (i, DentroJogo, jg1)
  |j == jg3 = return (i, DentroJogo, jg2)
  |j == jg4 = return (i, DentroJogo, jg3)
  |j == jg5 = return (i, DentroJogo, jg4)

inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)

inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, PassouLvl Proximo, j) = return (i, PassouLvl Leave, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, PassouLvl Proximo, j) = return (i, PassouLvl Leave, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, PassouLvl Leave, j) = return (i, PassouLvl Proximo, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, PassouLvl Leave, j) = return (i, PassouLvl Proximo, j)

inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Sair, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, PassouLvl Leave, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, Gameover, j) = exitSuccess

inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, PassouLvl Proximo, j)
  |j == jg1 = return (i, DentroJogo, jg2)
  |j == jg2 = return (i, DentroJogo, jg3)
  |j == jg3 = return (i, DentroJogo, jg4)
  |j == jg4 = return (i, DentroJogo, jg5)
  |j == jg5 = return (i, Gameover, j)

inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Jogar, j) = return (i, DentroJogo, j)

inputManager (EventKey (SpecialKey KeyEsc) Down _ _ ) (i, DentroJogo, j) = return (i, MP Jogar, j)

inputManager (EventKey (SpecialKey KeyRight) Down _ _ ) (i, DentroJogo, j)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaMsmPosicao (moveJogador j AndarDireita) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaMsmPosicao (moveJogador j AndarDireita) (jg2End) = return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaMsmPosicao (moveJogador j AndarDireita) (jg3End) = return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaMsmPosicao (moveJogador j AndarDireita) (jg4End) = return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaMsmPosicao (moveJogador j AndarDireita) (jg5End) = return (i, PassouLvl Proximo, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j AndarDireita)

inputManager (EventKey (SpecialKey KeyLeft) Down _ _ ) (i, DentroJogo, j) 
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaMsmPosicao (moveJogador j AndarEsquerda) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaMsmPosicao (moveJogador j AndarEsquerda) (jg2End) = return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaMsmPosicao (moveJogador j AndarEsquerda) (jg3End) = return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaMsmPosicao (moveJogador j AndarEsquerda) (jg4End) = return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaMsmPosicao (moveJogador j AndarEsquerda) (jg5End) = return (i, PassouLvl Proximo, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j AndarEsquerda)

inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, DentroJogo, j) = return (i, DentroJogo, moveJogador j InterageCaixa)

inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, DentroJogo, j)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaMsmPosicao (moveJogador j Trepar) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaMsmPosicao (moveJogador j Trepar) (jg2End)= return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaMsmPosicao (moveJogador j Trepar) (jg3End)= return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaMsmPosicao (moveJogador j Trepar) (jg4End)= return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaMsmPosicao (moveJogador j Trepar) (jg5End)= return (i, PassouLvl Proximo, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j Trepar)
inputManager _ e = return e

time :: Float -> Estado -> IO Estado
time _ e = return e

main :: IO ()
main = do
    bloco <- loadBMP "Imagens/bloco.bmp"
    caixa <- loadBMP "Imagens/caixa.bmp"
    castelo <- loadBMP "Imagens/castelo.bmp"
    mariodireita <- loadBMP "Imagens/mariodir.bmp"
    marioesquerda <- loadBMP "Imagens/marioesq.bmp"
    mpJogar <- loadBMP "Imagens/MPJogar.bmp"
    mpSair <- loadBMP "Imagens/MPSair.bmp"
    passoulvlProx <- loadBMP "Imagens/passouLvlProx.bmp"
    passoulvlSair <- loadBMP "Imagens/passouLvlSair.bmp"
    gameover <- loadBMP "Imagens/gameover.bmp"

    let estadoInicial = ([bloco, caixa, castelo, mariodireita, marioesquerda, mpJogar, mpSair, passoulvlProx, passoulvlSair, gameover], MP Jogar, jg1)

    playIO FullScreen 
        black 
        30 
        estadoInicial 
        desenhaEstado 
        inputManager 
        time