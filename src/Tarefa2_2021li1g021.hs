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

exemplo1 = [(Porta, (0, 2)),(Bloco, (0, 3)), (Bloco, (4,4)), (Bloco, (1, 3)), (Bloco, (2, 3))]

--para já só constrói um mapa com 1 x (x=0), tudo o resto dá errado (os elementos também só são organizados pelo y da coordenada)
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = [[]]
constroiMapa l = constroiMapaAUX (ordenaY l) (criarMapaVazio (maiorCoordenada l)) 0 --vai dar match das coordenadas dadas com as da função criarMapaVazio e meter lá as peças dadas
--l tem que ser ordenada
constroiMapaAUX [] _ _ = []
constroiMapaAUX ((p,(x,y)):t) (l:ls) n
    | y == n = [[p]] ++ constroiMapaAUX t ls (n+1) -- vai trabalhar e modificar l para q a lista de listas (antigamente vazia) comece a ter elementos (nas posiçoes corretas)
    | y /= n = [l] ++ constroiMapaAUX ((p,(x,y)):t) ls (n+1)

ordenaY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaY l = auxOrdenaY l [] --ordena uma lista relativamente ao y da coordenada 

auxOrdenaY [] l = l
auxOrdenaY l1 l2 = auxOrdenaY(removerElem (menorElemY l1) l1) (l2 ++ [menorElemY l1])

--criarMapaVazio :: ------- -> Mapa
criarMapaVazio (x,y) = criarMapaVazioAUX1 (x,y) (y+1) [] --esta função pega numa coordenada e cria um mapa vazio com essas dimensões

               --vai pegar nas listas dadas pela função criarMapaVazioAUX2 e criar uma lista dessas listas
criarMapaVazioAUX1 (x,y) 0 l = []
criarMapaVazioAUX1 (x,y) n l = criarMapaVazioAUX2 (x,y) (0,0) [] : criarMapaVazioAUX1 (x,y) (n-1) l 

--criarMapaVazioAUX2 :: Coordenadas -> Coordenadas -> [a]                  ERRO
--vai dar uma linha do mapa de cada vez
criarMapaVazioAUX2 (x,y) (a,b) l
    |a < x = criarMapaVazioAUX2 (x,y) (a+1,b) (l ++ [Vazio])
    |a == x = l ++ [Vazio]

coordenadaUltimoTuplo((p,c):t)  --dá a última coordenada do último tuplo
    |t == [] = c          --nao me lembro para que ia servir esta funçao mas pode ser útil no futuro
    |otherwise = coordenadaUltimoTuplo t

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = undefined
