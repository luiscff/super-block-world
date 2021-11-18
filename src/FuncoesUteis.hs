module FuncoesUteis where
import LI12122
-- Falta colocar o cabeçalho e as legendas
maiorCoordenada :: [(Peca,Coordenadas)] -> Coordenadas
maiorCoordenada l = maiorCoordenadaAUX l (0,0) --esta função dá a maior coordenada de uma lista

maiorCoordenadaAUX :: [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas
maiorCoordenadaAUX [] (a,b) = (a,b)
maiorCoordenadaAUX ((p,(x,y)):t) (a,b) --com a maior coordenada sabemos a dimensão da lista
    |x > a && y > b = maiorCoordenadaAUX t (x,y)
    |x > a = maiorCoordenadaAUX t (x,b)
    |y > b = maiorCoordenadaAUX t (a,y)
    |otherwise = maiorCoordenadaAUX t (a,b)

menorElem :: Ord a => [a] -> a
menorElem [] = error "não existe menorElem de uma lista vazia"
menorElem [x] = x
menorElem (x:xs)
    |x >= head xs = menorElem xs
    |otherwise = menorElem (x: (tail xs))


maiorElem :: Ord a => [a] -> a
maiorElem [] = error "não existe maiorElem de uma lista vazia"
maiorElem [x] = x
maiorElem (x:xs)
    |x <= head xs = maiorElem xs           
    |otherwise = maiorElem (x: (tail xs))
