module FuncoesUteis where
import LI12122
-- Falta colocar o cabeçalho e as legendas
maiorCoordenada :: [(Peca,Coordenadas)] -> Coordenadas
maiorCoordenada l = maiorCoordenadaAUX l (0,0) --dá a maior coordenada de uma lista

maiorCoordenadaAUX :: [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas
maiorCoordenadaAUX [] (a,b) = (a,b)
maiorCoordenadaAUX ((p,(x,y)):t) (a,b) 
    |x > a && y > b = maiorCoordenadaAUX t (x,y)
    |x > a = maiorCoordenadaAUX t (x,b)
    |y > b = maiorCoordenadaAUX t (a,y)
    |otherwise = maiorCoordenadaAUX t (a,b)

menorElemY :: [(Peca,Coordenadas)] -> (Peca,Coordenadas) --retorna um elemento com o menor Y
menorElemY [] = error "não existe menorElemY de uma lista vazia"
menorElemY [x] = x
menorElemY ((p,(x,y)):t)
    |y >= snd(maiorCoordenada [head t]) = menorElemY t
    |otherwise = menorElemY ( [(p,(x,y))] ++ (tail t))

maiorElemY :: [(Peca,Coordenadas)] -> (Peca,Coordenadas) --retorna um elemento com o maior Y
maiorElemY [] = error "não existe maiorElemY de uma lista vazia"
maiorElemY [x] = x
maiorElemY ((p,(x,y)):t)
    |y <= snd(maiorCoordenada [head t]) = maiorElemY t
    |otherwise = maiorElemY ( [(p,(x,y))] ++ (tail t))

removerElem _ [] = [] --remove um elemento de uma lista
removerElem x (y:ys) 
    | x == y    = removerElem x ys
    | otherwise = y : removerElem x ys