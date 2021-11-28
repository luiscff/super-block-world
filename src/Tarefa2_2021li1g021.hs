{- |
Module      : Tarefa2_2021li1g021

Description : Construção/Desconstrução do mapa

Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.-}
module Tarefa2_2021li1g021 where
import FuncoesUteis
import LI12122
{- | A função 'constroiMapa' contrói um mapa

== Exemplos de utilização

>>> constroiMapa [(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]

>[[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
>,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
>,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
>,[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco]
>,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = constroiMapaAUX (ordenaXeY l) (criarMapaVazio (maiorCoordenada l)) 0 0 []
{- | A função 'constroiMapaAUX' é uma auxiliar da função 'constroiMapa' que pega numa lista ordenada e mete os seus elementos nos sítios certos de um mapa de Vazios com a mesma dimensão-}
constroiMapaAUX :: [(Peca,Coordenadas)] -> Mapa -> Int -> Int -> [Peca] -> Mapa
constroiMapaAUX [(p,(x,y))] (l:ls) n nx ac 
    |x == nx = [ac ++ [p] ++ (quantosVaziosFaltam (ac ++ [p]) l)]
    |x /= nx = constroiMapaAUX [(p,(x,y))] (l:ls) n (nx + 1) (ac ++ [Vazio])
constroiMapaAUX ((p,(x,y)):t) (l:ls) n nx ac
    |y == n && y == snd(snd(head t)) && x == nx = constroiMapaAUX t (l:ls) n (nx+1) (ac ++ [p])
    |y == n && y == snd(snd(head t)) && x /= nx = constroiMapaAUX ((p,(x,y)):t) (l:ls) n (nx+1) (ac ++ [Vazio])
    |y == n && y /= snd(snd(head t)) && x == nx = [ac ++ [p] ++ quantosVaziosFaltam (ac ++ [p]) l] ++ constroiMapaAUX t (l:ls) (n+1) 0 []
    |y == n && y /= snd(snd(head t)) && x /= nx = constroiMapaAUX ((p,(x,y)):t) (l:ls) n (nx + 1) (ac ++ [Vazio])
    |y /= n = [l] ++ constroiMapaAUX ((p,(x,y)):t) ls (n+1) 0 []
{- | A função 'quantosVaziosFaltam' dá uma lista de Vazios do tamanho necessário para juntá-la a uma certa lista de modo a que esta fique do mesmo tamanho da 2ª lista que recebe

== Exemplos de utilização

>>> quantosVaziosFaltam [Porta] [Vazio,Vazio,Vazio,Vazio]

>[Vazio,Vazio,Vazio]

>>> [Porta] ++ quantosVaziosFaltam [Porta] [Bloco,Porta,Caixa,Vazio,Bloco]

>[Porta,Vazio,Vazio,Vazio,Vazio]-}
quantosVaziosFaltam :: [Peca] -> [Peca] -> [Peca]
quantosVaziosFaltam l1 l2
    |length l1 == length l2 = []
    |length l1 < length l2 = [Vazio] ++ quantosVaziosFaltam (l1 ++ [Vazio]) l2
{- | A função 'ordenaXeY' ordena uma lista do tipo [(Peca, Coordenadas)] de acordo com as suas coordenadas

== Exemplos de utilização

>>> ordenaXeY [(Porta,(2,5)), (Bloco,(3,2)), (Bloco,(2,3)), (Bloco,(1,2))]

>[(Bloco,(1,2)),(Bloco,(3,2)),(Bloco,(2,3)),(Porta,(2,5))]-}
ordenaXeY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaXeY l = ordenaXeYaux (ordenaY l) []
{- | A função 'ordenaXeYaux' é uma auxiliar da 'ordenaXeY'-}
ordenaXeYaux :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaXeYaux [x] l = ordenaX (l ++ [x])
ordenaXeYaux ((p,(x,y)):t) l
    |y == snd(snd(head t)) = ordenaXeYaux t (l ++ [(p,(x,y))])
    |otherwise = ordenaX (l ++ [(p,(x,y))]) ++ ordenaXeYaux t []
{- | A função 'ordenaY' ordena uma lista relativamente ao y da coordenada 

== Exemplos de utilização

>>> ordenaY [(Porta,(2,5)), (Bloco,(1,2)), (Bloco,(2,3)), (Bloco,(3,2))]

>[(Bloco,(3,2)),(Bloco,(1,2)),(Bloco,(2,3)),(Porta,(2,5))]-}
ordenaY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaY l = auxOrdenaY l []
{- | A função 'auxOrdenaY' é uma auxiliar da função 'ordenaY'-}
auxOrdenaY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
auxOrdenaY [] l = l
auxOrdenaY l1 l2 = auxOrdenaY(removerElem (menorElemY l1) l1) (l2 ++ [menorElemY l1])
{- | A função 'ordenaX' ordena uma lista relativamente ao x da coordenada

== Exemplos de utilização

>>> ordenaX [(Porta,(2,5)), (Bloco,(1,2)), (Bloco,(2,3)), (Bloco,(3,2))]

>[(Bloco,(1,2)),(Bloco,(2,3)),(Porta,(2,5)),(Bloco,(3,2))]-}
ordenaX :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaX l = auxOrdenaX l []
{- | A função 'auxOrdenaX' é uma auxiliar da função 'ordenaX'-}
auxOrdenaX :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
auxOrdenaX [] l = l
auxOrdenaX l1 l2 = auxOrdenaX(removerElem (menorElemX l1) l1) (l2 ++ [menorElemX l1])
{- | A função 'criarMapaVazio' pega numa coordenada e cria um mapa de Vazios com essas dimensões 

== Exemplos de utilização

>>> criarMapaVazio (2,3)

>[[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio]]-}
criarMapaVazio :: Coordenadas -> Mapa
criarMapaVazio (x,y) = criarMapaVazioAUX1 (x,y) (y+1) []
{- | A função 'criarMapaVazioAUX1' é uma auxiliar da função 'criarMapaVazio' que pega nas listas dadas pela função criarMapaVazioAUX2 e cria uma lista dessas listas-}
criarMapaVazioAUX1 :: Coordenadas -> Int -> [Peca] -> [[Peca]]
criarMapaVazioAUX1 (x,y) 0 l = []
criarMapaVazioAUX1 (x,y) n l = criarMapaVazioAUX2 (x,y) (0,0) [] : criarMapaVazioAUX1 (x,y) (n-1) l 
{- | A função 'criarMapaVazioAUX2' é uma auxiliar da função 'criarMapaVazio' que dá uma linha do mapa de cada vez-}
criarMapaVazioAUX2 :: Coordenadas -> Coordenadas -> [Peca] -> [Peca]
criarMapaVazioAUX2 (x,y) (a,b) l
    |a < x = criarMapaVazioAUX2 (x,y) (a+1,b) (l ++ [Vazio])
    |a == x = l ++ [Vazio]
{- | A função 'desconstroiMapa' recebe um mapa e transforma-o na forma [(Peca, Coordenadas)], ocultando os Vazios

== Exemplos de utilização

>>> desconstroiMapa [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

>[(Bloco,(6,1)),(Bloco,(6,2)),(Porta,(0,3)),(Caixa,(4,3)),(Bloco,(6,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Bloco,(5,4)),(Bloco,(6,4))]
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapaAUX mapa (0,0)
{- | A função 'desconstroiMapaAUX' é uma auxiliar da função 'desconstroiMapa' -}
desconstroiMapaAUX :: Mapa -> Coordenadas -> [(Peca, Coordenadas)]
desconstroiMapaAUX ([a]:t') (x,y)
    |a == Vazio = desconstroiMapaAUX t' (0,y+1)
    |otherwise = [(a,(x,y))] ++ desconstroiMapaAUX t' (0,y+1)
desconstroiMapaAUX [] _ = []
desconstroiMapaAUX ((h:t):t') (x,y)
    |h == Vazio = desconstroiMapaAUX (t:t') (x+1, y)
    |h == Porta = [(Porta,(x,y))] ++ desconstroiMapaAUX (t:t') (x+1, y)
    |h == Bloco = [(Bloco,(x,y))] ++ desconstroiMapaAUX (t:t') (x+1, y)
    |h == Caixa = [(Caixa,(x,y))] ++ desconstroiMapaAUX (t:t') (x+1, y)