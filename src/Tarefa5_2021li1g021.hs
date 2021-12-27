{- |
Module      : Tarefa5_2021li1g021
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where
import LI12122
import Graphics.Gloss.Interface.Pure.Game

menuInicial = undefined

desenhaJogo :: Jogo -> Picture
desenhaJogo = undefined

inputManager :: Event -> Jogo -> Jogo
inputManager = undefined

time :: Float -> Jogo -> Jogo
time = undefined

main :: IO ()
main = do
    play FullScreen black 20 menuInicial desenhaJogo inputManager time