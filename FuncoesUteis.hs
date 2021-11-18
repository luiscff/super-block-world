module FuncoesUteis where
import LI12122

maiorCoordenada :: [(Peca,Coordenadas)] -> Coordenadas
maiorCoordenada l = maiorCoordenadaAUX l (0,0) --esta função dá a maior coordenada de uma lista

maiorCoordenadaAUX :: [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas
maiorCoordenadaAUX [] (a,b) = (a,b)
maiorCoordenadaAUX ((p,(x,y)):t) (a,b) --com a maior coordenada sabemos a dimensão da lista
    |x > a && y > b = maiorCoordenadaAUX t (x,y)
    |x > a = maiorCoordenadaAUX t (x,b)
    |y > b = maiorCoordenadaAUX t (a,y)
    |otherwise = maiorCoordenadaAUX t (a,b)