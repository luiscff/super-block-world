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
import FuncoesUteis
import System.Exit
import Tarefa2_2021li1g021 (desconstroiMapa)
import Tarefa4_2021li1g021

data Menu = MP OpcoesIn | DentroJogo 

instance Eq Menu where
  (==) (MP Jogar) (MP Jogar) = True
  (==) (MP Sair) (MP Sair) = True
  (==) (DentroJogo) (DentroJogo) = True
  (==) _ _ = False

data OpcoesIn = Jogar | Sair

type Estado = ([Picture] , Menu , Jogo)

desenhaEstado :: Estado -> IO Picture
desenhaEstado e@(imagens, menu, jogo)
  |menu == MP Jogar = return $ imagens !! 5
  |menu == MP Sair = return $ imagens !! 6
  |menu == DentroJogo = return $ desenhaJogo e jogo

desenhaJogo :: Estado -> Jogo -> Picture
desenhaJogo e (Jogo mapa jogador) = Scale 2.0 2.0 $ Translate (fromIntegral((maiorX (desconstroiMapa mapa))*(-20))) (fromIntegral((maiorY (desconstroiMapa mapa))*(15))) $ Pictures [desenhaMapa e (desconstroiMapa mapa), desenhaJogador e jogador]

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
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Sair, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Jogar, j) = return (i, DentroJogo, j)
inputManager (EventKey (SpecialKey KeyEsc) Down _ _ ) (i, DentroJogo, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyRight) Down _ _ ) (i, DentroJogo, j) = return (i, DentroJogo, moveJogador j AndarDireita)
inputManager (EventKey (SpecialKey KeyLeft) Down _ _ ) (i, DentroJogo, j) = return (i, DentroJogo, moveJogador j AndarEsquerda)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, DentroJogo, j) = return (i, DentroJogo, moveJogador j InterageCaixa)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, DentroJogo, j) = return (i, DentroJogo, moveJogador j Trepar)
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

    let estadoInicial = ([bloco, caixa, castelo, mariodireita, marioesquerda, mpJogar, mpSair], MP Jogar, m1e1)

    playIO FullScreen 
        black 
        30 
        estadoInicial 
        desenhaEstado 
        inputManager 
        time