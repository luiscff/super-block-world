{- |
Module      : FuncoesUteis
Description : Funções Utilizadas ao longo do Projeto
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

-}

module FuncoesUteis where
import LI12122

{-| Função: Dá a maior coordenada de uma lista

== Código
@
maiorCoordenada :: [(Peca,Coordenadas)] -> Coordenadas
maiorCoordenada l = maiorCoordenadaAUX l (0,0)
    where 
        maiorCoordenadaAUX :: [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas
        maiorCoordenadaAUX [] (a,b) = (a,b)
        maiorCoordenadaAUX ((p,(x,y)):t) (a,b) 
            |x > a && y > b = maiorCoordenadaAUX t (x,y)
            |x > a = maiorCoordenadaAUX t (x,b)
            |y > b = maiorCoordenadaAUX t (a,y)
            |otherwise = maiorCoordenadaAUX t (a,b)
@
-}

maiorCoordenada :: [(Peca,Coordenadas)] -- ^ Recebe uma lista 
                   -> Coordenadas -- ^ Dá o maior x e o maio y existente sendo (x,y)
maiorCoordenada l = maiorCoordenadaAUX l (0,0)
    where 
        maiorCoordenadaAUX :: [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas
        maiorCoordenadaAUX [] (a,b) = (a,b)
        maiorCoordenadaAUX ((p,(x,y)):t) (a,b) 
            |x > a && y > b = maiorCoordenadaAUX t (x,y)
            |x > a = maiorCoordenadaAUX t (x,b)
            |y > b = maiorCoordenadaAUX t (a,y)
            |otherwise = maiorCoordenadaAUX t (a,b)


{-| Função: Retorna o elemento com o menor Y

== Código
@
menorElemY :: [(Peca,Coordenadas)] -> (Peca,Coordenadas) 
menorElemY [] = error "não existe menorElemY de uma lista vazia"
menorElemY [x] = x
menorElemY ((p,(x,y)):t)
    |y >= snd(maiorCoordenada [head t]) = menorElemY t
    |otherwise = menorElemY ( [(p,(x,y))] ++ (tail t))
@
-}

menorElemY :: [(Peca,Coordenadas)] -- ^ Recebe uma lista
              -> (Peca,Coordenadas) -- ^ Retorna a Peça e a sua coordenda com o menor Y
menorElemY [] = error "não existe menorElemY de uma lista vazia"
menorElemY [x] = x
menorElemY ((p,(x,y)):t)
    |y >= snd(maiorCoordenada [head t]) = menorElemY t
    |otherwise = menorElemY ( [(p,(x,y))] ++ (tail t))


{-| Função: Retorna o elemento com o menor X

== Código
@
menorElemX :: [(Peca,Coordenadas)] -> (Peca,Coordenadas)
menorElemX [] = error "não existe menorElemX de uma lista vazia"
menorElemX [x] = x
menorElemX ((p,(x,y)):t)
    |x >= fst(maiorCoordenada [head t]) = menorElemX t
    |otherwise = menorElemX ( [(p,(x,y))] ++ (tail t))
@
-}

menorElemX :: [(Peca,Coordenadas)] -- ^ Recebe uma lista
              -> (Peca,Coordenadas) -- ^ Retorna a Peça e a sua coordenda com o menor X
menorElemX [] = error "não existe menorElemX de uma lista vazia"
menorElemX [x] = x
menorElemX ((p,(x,y)):t)
    |x >= fst(maiorCoordenada [head t]) = menorElemX t
    |otherwise = menorElemX ( [(p,(x,y))] ++ (tail t))

{-| Função: Remove um elemento de uma lista

== Código
@
removerElem :: (Peca,Coordendas) -> [(Peca,Coordenadas)] -> [(Pe,Coordenadas)]
removerElem _ [] = []
removerElem x (y:ys) 
    | x == y    = removerElem x ys
    | otherwise = y : removerElem x ys
@
-}

removerElem :: (Peca,Coordenadas) -- ^ Recebe um Tuplo
               -> [(Peca,Coordenadas)] -- ^ Recebe uma lista
               -> [(Peca,Coordenadas)] -- ^ Remove todos os elementos iguais
removerElem _ [] = []
removerElem x (y:ys) 
    | x == y    = removerElem x ys
    | otherwise = y : removerElem x ys