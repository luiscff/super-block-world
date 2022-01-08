{- |
Module      : Tarefa5_2021li1g021
Description : Aplicação Gráfica
Bibliografia :
              -Site: https://hackage.haskell.org/package/gloss
              -Documentos fornecidos durante as aulas de L.I.1
Dificuldades sentidas : O início foi muito lento e complicado mas com a ajuda do professor e de colegas foi possível adquirir conhecimentos para realizar a tarefa
Observações : Ponderei fazer um sistema de menus mais complexo mas acabei por não o fazer uma vez que ocuparia muito tempo e não demonstraria mais conhecimento, por isso, mantive essa parte muito simples
              Ponderei também fazer um sistema de high scores baseado em melhor tempo mas acabei por não ter tempo para o desenvolver.

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

{- | esta estrutura de dados consiste em 4 Menus: o Menu Principal (MP), o Menu que permite desenhar o jogo em si (DentroJogo), o menu de passagem de nível (PassouLvl) e o Menu final (Gameover) 

@
data Menu = MP OpcoesIn | DentroJogo | PassouLvl OpcoesLvl | Gameover
@

-}
data Menu = MP OpcoesIn | DentroJogo | PassouLvl OpcoesLvl | Gameover

{- | esta parte permite igualar menus, é bastante útil para separar o que cada situação deve fazer (o melhor exemplo é a função desenhaEstado)

@
instance Eq Menu where
  (==) (MP Jogar) (MP Jogar) = True
  (==) (MP Sair) (MP Sair) = True
  (==) (DentroJogo) (DentroJogo) = True
  (==) (PassouLvl Proximo) (PassouLvl Proximo) = True
  (==) (PassouLvl Leave) (PassouLvl Leave) = True
  (==) (Gameover) (Gameover) = True
  (==) _ _ = False
@

-}
instance Eq Menu where
  (==) (MP Jogar) (MP Jogar) = True
  (==) (MP Sair) (MP Sair) = True
  (==) (DentroJogo) (DentroJogo) = True
  (==) (PassouLvl Proximo) (PassouLvl Proximo) = True
  (==) (PassouLvl Leave) (PassouLvl Leave) = True
  (==) (Gameover) (Gameover) = True
  (==) _ _ = False

{- | esta parte serve para declarar as opções selecionadas no menu de passagem de nível e no menu inicial, respetivamente 

@
data OpcoesLvl = Proximo | Leave
@

@
data OpcoesIn = Jogar | Sair
@

-}

data OpcoesLvl = Proximo | Leave
data OpcoesIn = Jogar | Sair

type Estado = ([Picture] , Menu , Jogo)

{- | a função jogadoresNaPosicao verifica se o jogador de um jogo se encontra em determinada coordenada

== Código
@
jogadoresNaPosicao (Jogo _ (Jogador (x1,y1) _ _)) (x2,y2)
  |x1 == x2 && y1 == y2 = True
  |otherwise = False
@

-}
jogadoresNaPosicao :: Jogo -> Coordenadas -> Bool
jogadoresNaPosicao (Jogo _ (Jogador (x1,y1) _ _)) (x2,y2)
  |x1 == x2 && y1 == y2 = True
  |otherwise = False

{- | a função jogadoresNaPosicao verifica se o jogador de um jogo se encontra na mesma coordenada da porta

== Código
@
jogadorNaPorta (Jogo mapa (Jogador (x,y) dir bool))
  |coordenadaPorta (desconstroiMapa mapa) == (x,y) = True
  |otherwise = False
@

-}
jogadorNaPorta :: Jogo -> Bool
jogadorNaPorta (Jogo mapa (Jogador (x,y) dir bool))
  |coordenadaPorta (desconstroiMapa mapa) == (x,y) = True
  |otherwise = False

{- | a função coordenadaPorta recebe uma lista de (Peca,Coordenadas) e dá a coordenada da Porta

== Código
@
coordenadaPorta ((p,(x,y)):t)
  |p == Porta = (x,y)
  |otherwise = coordenadaPorta t
@

-}
coordenadaPorta :: [(Peca, Coordenadas)] -> Coordenadas
coordenadaPorta ((p,(x,y)):t)
  |p == Porta = (x,y)
  |otherwise = coordenadaPorta t

{- | A função 'desenhaEstado' desenha o Estado atual

__quando o menu atual é jogo em si, desenha o jogo recorrendo a uma função auxiliar__

@
  |menu == DentroJogo = return $ desenhaJogo e jogo
@

__quando os menus não são o jogo, desenha imagens que representam os menus com uma opção selecionada__

@
desenhaEstado e@(imagens, menu, jogo)
  |menu == MP Jogar = return $ imagens !! 5
  |menu == MP Sair = return $ imagens !! 6
  |menu == (PassouLvl Proximo) = return $ imagens !! 7
  |menu == (PassouLvl Leave) = return $ imagens !! 8
  |menu == Gameover = return $ imagens !! 9
@

-}
desenhaEstado :: Estado -> IO Picture
desenhaEstado e@(imagens, menu, jogo)
  |menu == MP Jogar = return $ imagens !! 5
  |menu == MP Sair = return $ imagens !! 6
  |menu == DentroJogo = return $ desenhaJogo e jogo
  |menu == (PassouLvl Proximo) = return $ imagens !! 7
  |menu == (PassouLvl Leave) = return $ imagens !! 8
  |menu == Gameover = return $ imagens !! 9

{- | A função 'desenhaJogo' desenha o Jogo atual recorrendo a funções auxiliares para desenhar o Mapa e o Jogador e faz com que o jogo fique no centro do ecrã

== Código
@
desenhaJogo e (Jogo mapa@(a:t) jogador) = (Translate x y (Pictures [desenhaMapa e (desconstroiMapa mapa), desenhaJogador e jogador]))
              where
                x = (fromIntegral((length a)*(-50)))/2.0
                y = (fromIntegral((length mapa)*(50)))/2.0
@

-}
desenhaJogo :: Estado -> Jogo -> Picture
desenhaJogo e (Jogo mapa@(a:t) jogador) = (Translate x y (Pictures [desenhaMapa e (desconstroiMapa mapa), desenhaJogador e jogador]))
              where
                x = (fromIntegral((length a)*(-50)))/2.0
                y = (fromIntegral((length mapa)*(50)))/2.0

{- | A função 'desenhaMapa' desenha o Mapa uma peça de cada vez

== Código
@
desenhaMapa e [] = Blank
desenhaMapa e@(imagens,menu,jogo) ((p,(x,y)):t)
  |p == Bloco = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 0)), desenhaMapa e t]
  |p == Caixa = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 1)), desenhaMapa e t]
  |p == Porta = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 2)), desenhaMapa e t]
@

-}
desenhaMapa :: Estado -> [(Peca, Coordenadas)] -> Picture
desenhaMapa e [] = Blank
desenhaMapa e@(imagens,menu,jogo) ((p,(x,y)):t)
  |p == Bloco = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 0)), desenhaMapa e t]
  |p == Caixa = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 1)), desenhaMapa e t]
  |p == Porta = Pictures [(translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 2)), desenhaMapa e t]
  
{- | A função 'desenhaJogador' desenha o jogador

__quando o jogador carrega uma caixa desenha o jogador e uma caixa em cima __
@
desenhaJogador e@(imagens,menu, Jogo mapa jogador) (Jogador (x,y) dir bool)
  |dir == Este && bool == True = Pictures [translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 3), translate (fromIntegral (x*50)) (fromIntegral ((y-1)*(-50))) (imagens !! 1)]
  |dir == Oeste && bool == True = Pictures [translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 4), translate (fromIntegral (x*50)) (fromIntegral ((y-1)*(-50))) (imagens !! 1)]
@

__quando o jogador não carrega uma caixa desenha só o jogador__ 
@
  |dir == Este  = translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 3)
  |dir == Oeste = translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 4)
@

-}
desenhaJogador :: Estado -> Jogador -> Picture
desenhaJogador e@(imagens,menu, Jogo mapa jogador) (Jogador (x,y) dir bool)
  |dir == Este && bool == True = Pictures [translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 3), translate (fromIntegral (x*50)) (fromIntegral ((y-1)*(-50))) (imagens !! 1)]
  |dir == Oeste && bool == True = Pictures [translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 4), translate (fromIntegral (x*50)) (fromIntegral ((y-1)*(-50))) (imagens !! 1)]
  |dir == Este  = translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 3)
  |dir == Oeste = translate (fromIntegral (x*50)) (fromIntegral ((-y)*50)) (imagens !! 4)

{- | A função 'inputManager' permitir-nos manipular (através do teclado) o personagem

__"cheat code" que passa diretamente para o próximo nível desde que o jogador não ande (existe para facilitar os testes feitos pelos desenvolvedores e a apresentação do jogo) __

@
inputManager (EventKey (Char 'p') Down _ _ ) (i, DentroJogo, j)
  |j == jg1 = return (i, DentroJogo, jg2)
  |j == jg2 = return (i, DentroJogo, jg3)
  |j == jg3 = return (i, DentroJogo, jg4)
  |j == jg4 = return (i, DentroJogo, jg5)
  |j == jg5 = return (i, Gameover, j)
@

-}
inputManager :: Event -> Estado -> IO Estado
inputManager (EventKey (Char 'p') Down _ _ ) (i, DentroJogo, j)
  |j == jg1 = return (i, DentroJogo, jg2)
  |j == jg2 = return (i, DentroJogo, jg3)
  |j == jg3 = return (i, DentroJogo, jg4)
  |j == jg4 = return (i, DentroJogo, jg5)
  |j == jg5 = return (i, Gameover, j)

{- 
__"cheat code" que passa diretamente para o nível anterior desde que o jogador não ande (existe para facilitar os testes feitos pelos desenvolvedores e a apresentação do jogo) __ 

@
inputManager (EventKey (Char 'o') Down _ _ ) (i, DentroJogo, j)
  |j == jg1 = return (i, MP Jogar, jg1)
  |j == jg2 = return (i, DentroJogo, jg1)
  |j == jg3 = return (i, DentroJogo, jg2)
  |j == jg4 = return (i, DentroJogo, jg3)
  |j == jg5 = return (i, DentroJogo, jg4)
@

-}
inputManager (EventKey (Char 'o') Down _ _ ) (i, DentroJogo, j)
  |j == jg1 = return (i, MP Jogar, jg1)
  |j == jg2 = return (i, DentroJogo, jg1)
  |j == jg3 = return (i, DentroJogo, jg2)
  |j == jg4 = return (i, DentroJogo, jg3)
  |j == jg5 = return (i, DentroJogo, jg4)

{-
__esta parte permite alternar entre as opções do menu principal__

@
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
@

-}
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Jogar, j) = return (i, MP Sair, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, MP Sair, j) = return (i, MP Jogar, j)

{-
__esta parte permite alternar entre as opções do menu de passagem de nível__

@
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, PassouLvl Proximo, j) = return (i, PassouLvl Leave, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, PassouLvl Proximo, j) = return (i, PassouLvl Leave, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, PassouLvl Leave, j) = return (i, PassouLvl Proximo, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, PassouLvl Leave, j) = return (i, PassouLvl Proximo, j)
@
-}
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, PassouLvl Proximo, j) = return (i, PassouLvl Leave, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, PassouLvl Proximo, j) = return (i, PassouLvl Leave, j)
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, PassouLvl Leave, j) = return (i, PassouLvl Proximo, j)
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, PassouLvl Leave, j) = return (i, PassouLvl Proximo, j)

{-
__esta parte permite sair do jogo ao clicar Enter na opção "Sair" de cada menu (foi uma sugestão do aluno a100643 Rui Lopes Martins)__

@
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Sair, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, PassouLvl Leave, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, Gameover, j) = exitSuccess
@

-}
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Sair, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, PassouLvl Leave, j) = exitSuccess
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, Gameover, j) = exitSuccess

{-
__esta parte permite ir para o próximo nível ao clicar Enter na opção "Próximo Nível" do menu de passagem de nível ou ir para __

@
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, PassouLvl Proximo, j)
  |j == jg1 = return (i, DentroJogo, jg2)
  |j == jg2 = return (i, DentroJogo, jg3)
  |j == jg3 = return (i, DentroJogo, jg4)
  |j == jg4 = return (i, DentroJogo, jg5)
@

-}
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, PassouLvl Proximo, j)
  |j == jg1 = return (i, DentroJogo, jg2)
  |j == jg2 = return (i, DentroJogo, jg3)
  |j == jg3 = return (i, DentroJogo, jg4)
  |j == jg4 = return (i, DentroJogo, jg5)

{-
__esta parte põe o jogo em pausa no Esc e continua o jogo ao clicar Enter__

@
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Jogar, j) = return (i, DentroJogo, j)
inputManager (EventKey (SpecialKey KeyEsc) Down _ _ ) (i, DentroJogo, j) = return (i, MP Jogar, j)
@
-}
inputManager (EventKey (SpecialKey KeyEnter) Down _ _ ) (i, MP Jogar, j) = return (i, DentroJogo, j)
inputManager (EventKey (SpecialKey KeyEsc) Down _ _ ) (i, DentroJogo, j) = return (i, MP Jogar, j)

{-
__esta parte faz o jogador andar para a direita e garante que o jogo progride quando o jogador chega à porta depois de andar para a direita__

@
inputManager (EventKey (SpecialKey KeyRight) Down _ _ ) (i, DentroJogo, j)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg2End) = return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg3End) = return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg4End) = return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg5End) = return (i, Gameover, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j AndarDireita)
@

-}
inputManager (EventKey (SpecialKey KeyRight) Down _ _ ) (i, DentroJogo, j)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg2End) = return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg3End) = return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg4End) = return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j AndarDireita)) && jogadoresNaPosicao (moveJogador j AndarDireita) (jg5End) = return (i, Gameover, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j AndarDireita)

{-
__esta parte faz o jogador andar para a esquerda e garante que o jogo progride quando o jogador chega à porta depois de andar para a esquerda__

@
inputManager (EventKey (SpecialKey KeyLeft) Down _ _ ) (i, DentroJogo, j) 
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg2End) = return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg3End) = return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg4End) = return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg5End) = return (i, PassouLvl Proximo, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j AndarEsquerda)
@

-}
inputManager (EventKey (SpecialKey KeyLeft) Down _ _ ) (i, DentroJogo, j) 
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg2End) = return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg3End) = return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg4End) = return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j AndarEsquerda)) && jogadoresNaPosicao (moveJogador j AndarEsquerda) (jg5End) = return (i, PassouLvl Proximo, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j AndarEsquerda)

{-
__esta parte faz o jogador interagir com a caixa__

@
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, DentroJogo, j) = return (i, DentroJogo, moveJogador j InterageCaixa)
@

-}
inputManager (EventKey (SpecialKey KeyDown) Down _ _ ) (i, DentroJogo, j) = return (i, DentroJogo, moveJogador j InterageCaixa)

{-
__esta parte faz o jogador trepar e garante que o jogo progride quando o jogador chega à porta depois de trepar__

@
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, DentroJogo, j)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg2End)= return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg3End)= return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg4End)= return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg5End)= return (i, PassouLvl Proximo, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j Trepar)
@

-}
inputManager (EventKey (SpecialKey KeyUp) Down _ _ ) (i, DentroJogo, j)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg1End) = return (i, PassouLvl Proximo, jg1)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg2End)= return (i, PassouLvl Proximo, jg2)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg3End)= return (i, PassouLvl Proximo, jg3)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg4End)= return (i, PassouLvl Proximo, jg4)
  |(jogadorNaPorta (moveJogador j Trepar)) && jogadoresNaPosicao (moveJogador j Trepar) (jg5End)= return (i, PassouLvl Proximo, jg5)
  |otherwise = return (i, DentroJogo, moveJogador j Trepar)

{-
__esta parte garante que tudo o que não esteja explícito em cima não faz nada no jogo__

@
inputManager _ e = return e
@

-}
inputManager _ e = return e

-- | A função time apenas retorna o mesmo estado (não foi implementada nenhuma funcionalidade que a utilize)
time :: Float -> Estado -> IO Estado
time _ e = return e

{- | A função main carrega todas as imagens; cria um estado inicial com estas imagens, o menu principal e o 1º nível e chama a função playIO (que é a função principal da tarefa)

==Código

@
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
        100 
        estadoInicial 
        desenhaEstado 
        inputManager 
        time
@

-}
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
        100
        estadoInicial 
        desenhaEstado 
        inputManager 
        time