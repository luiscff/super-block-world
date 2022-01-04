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

type Imagens = [Picture]

data Menu = MP OpcoesIn | DentroJogo 

instance Eq Menu where
  (==) (MP Jogar) (MP Jogar) = True
  (==) (MP Sair) (MP Sair) = True
  (==) (DentroJogo) (DentroJogo) = True
  (==) _ _ = False

data OpcoesIn = Jogar | Sair

type Estado = (Imagens , Menu , Jogo)

desenhaEstado :: Estado -> IO Picture
desenhaEstado e@(imagens, menu, jogo)
    |menu == MP Jogar = return $ imagens !! 5
    |menu == MP Sair = return $ imagens !! 6
    |menu == DentroJogo = return $ desenhaJogo jogo

desenhaJogo :: Jogo -> Picture
desenhaJogo = undefined

inputManager :: Event -> Estado -> IO Estado
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Sair, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Jogar, j) = return (i, DentroJogo, j)
inputManager _ e = return e

time :: Float -> Estado -> IO Estado
time _ e = return e

main :: IO ()
main = do
    bloco <- loadBMP "Imagens/bloco.bmp"
    caixa <- loadBMP "Imagens/caixa.bmp"
    castelo <- loadBMP "Imagens/castelo.bmp"
    mariodireita <- loadBMP "Imagens/mariodireita.bmp"
    marioesquerda <- loadBMP "Imagens/marioesquerda.bmp"
    mpJogar <- loadBMP "Imagens/menu.bmp"
    mpSair <- loadBMP ""
    let estadoInicial = ([bloco, caixa, castelo, mariodireita, marioesquerda, mpJogar, mpSair], MP Jogar, m1e1)

    playIO FullScreen 
        black 
        30 
        estadoInicial 
        desenhaEstado 
        inputManager 
        time