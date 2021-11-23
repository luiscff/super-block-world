{- |
Module      : Tarefa2_2021li1g021
Description : Construção/Desconstrução do mapa
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g021 where
import FuncoesUteis
import LI12122

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = [[]]
constroiMapa l = constroiMapaAUX (ordenaXeY l) (criarMapaVazio (maiorCoordenada l)) 0 0 [] -- pega numa lista ordenada e mete os seus elementos nos sítios certos de um mapa vazio
constroiMapaAUX :: [(Peca,Coordenadas)] -> Mapa -> Int -> Int -> [Peca] -> Mapa
constroiMapaAUX [(p,(x,y))] l n nx ac --caso de paragem (quando a lista só tem 1 elemento)
    |x == nx = [ac ++ [p]]
    |x /= nx = constroiMapaAUX [(p,(x,y))] l n (nx + 1) (ac ++ [Vazio])
    

constroiMapaAUX ((p,(x,y)):t) (l:ls) n nx ac
    |y == n && y == snd(snd(head t)) && x == nx = constroiMapaAUX t (l:ls) n (nx+1) (ac ++ [p])
    |y == n && y == snd(snd(head t)) && x /= nx = constroiMapaAUX ((p,(x,y)):t) (l:ls) n (nx+1) (ac ++ [Vazio])
    |y == n && y /= snd(snd(head t)) && x == nx = [ac ++ [p] ++ quantosVaziosFaltam (ac ++ [p]) l] ++ constroiMapaAUX t (l:ls) (n+1) 0 []
    |y == n && y /= snd(snd(head t)) && x /= nx = constroiMapaAUX ((p,(x,y)):t) (l:ls) n (nx + 1) (ac ++ [Vazio])
    |y /= n = [l] ++ constroiMapaAUX ((p,(x,y)):t) ls (n+1) 0 []

quantosVaziosFaltam l1 l2 -- enche a lista de Vazios até a mesma ser do tamanho necessário 
    |length l1 == length l2 = []
    |length l1 < length l2 = [Vazio] ++ quantosVaziosFaltam (l1 ++ [Vazio]) l2

ordenaXeY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] --ordena uma lista do tipo [(Peca, Coordenadas)] de acordo com as suas coordenadas
ordenaXeY l = ordenaXeYaux (ordenaY l) []

ordenaXeYaux :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaXeYaux [x] l = ordenaX (l ++ [x])
ordenaXeYaux ((p,(x,y)):t) l
    |y == snd(snd(head t)) = ordenaXeYaux t (l ++ [(p,(x,y))])
    |otherwise = ordenaX (l ++ [(p,(x,y))]) ++ ordenaXeYaux t []

ordenaY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaY l = auxOrdenaY l [] --ordena uma lista relativamente ao y da coordenada 

auxOrdenaY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
auxOrdenaY [] l = l
auxOrdenaY l1 l2 = auxOrdenaY(removerElem (menorElemY l1) l1) (l2 ++ [menorElemY l1])

ordenaX :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaX l = auxOrdenaX l [] --ordena uma lista relativamente ao x da coordenada. Ignora o Y

auxOrdenaX :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
auxOrdenaX [] l = l
auxOrdenaX l1 l2 = auxOrdenaX(removerElem (menorElemX l1) l1) (l2 ++ [menorElemX l1])

criarMapaVazio :: Coordenadas -> Mapa
criarMapaVazio (x,y) = criarMapaVazioAUX1 (x,y) (y+1) [] --pega numa coordenada e cria um mapa vazio com essas dimensões

criarMapaVazioAUX1 :: Coordenadas -> Int -> [Peca] -> [[Peca]] --vai pegar nas listas dadas pela função criarMapaVazioAUX2 e criar uma lista dessas listas
criarMapaVazioAUX1 (x,y) 0 l = []
criarMapaVazioAUX1 (x,y) n l = criarMapaVazioAUX2 (x,y) (0,0) [] : criarMapaVazioAUX1 (x,y) (n-1) l 

criarMapaVazioAUX2 :: Coordenadas -> Coordenadas -> [Peca] -> [Peca] --vai dar uma linha do mapa de cada vez
criarMapaVazioAUX2 (x,y) (a,b) l
    |a < x = criarMapaVazioAUX2 (x,y) (a+1,b) (l ++ [Vazio])
    |a == x = l ++ [Vazio]

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapaAUX mapa (0,0) --pega num mapa e transforma-o na forma [(Peca, Coordenadas)]

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