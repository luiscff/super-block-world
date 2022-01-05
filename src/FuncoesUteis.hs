{- |
Module      : FuncoesUteis
Description : Funções Utilizadas ao longo do Projeto
Copyright   : Flávio David Rodrigues Sousa <a100715@alunos.uminho.pt>;
            : Luís Carlos Fragoso Figueiredo <a100549@alunos.uminho.pt>;

-}
module FuncoesUteis where

import LI12122

-- | Função: Dá a maior coordenada de uma lista
--
-- == Código
-- @
-- maiorCoordenada :: [(Peca,Coordenadas)] -> Coordenadas
-- maiorCoordenada l = maiorCoordenadaAUX l (0,0)
--    where
--        maiorCoordenadaAUX :: [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas
--        maiorCoordenadaAUX [] (a,b) = (a,b)
--        maiorCoordenadaAUX ((p,(x,y)):t) (a,b)
--            |x > a && y > b = maiorCoordenadaAUX t (x,y)
--            |x > a = maiorCoordenadaAUX t (x,b)
--            |y > b = maiorCoordenadaAUX t (a,y)
--            |otherwise = maiorCoordenadaAUX t (a,b)
-- @
maiorCoordenada ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Dá o maior x e o maio y existente sendo (x,y)
  Coordenadas
maiorCoordenada l = maiorCoordenadaAUX l (0, 0)
  where
    maiorCoordenadaAUX :: [(Peca, Coordenadas)] -> Coordenadas -> Coordenadas
    maiorCoordenadaAUX [] (a, b) = (a, b)
    maiorCoordenadaAUX ((p, (x, y)) : t) (a, b)
      | x > a && y > b = maiorCoordenadaAUX t (x, y)
      | x > a = maiorCoordenadaAUX t (x, b)
      | y > b = maiorCoordenadaAUX t (a, y)
      | otherwise = maiorCoordenadaAUX t (a, b)

-- | Função: Retorna o elemento com o menor Y
--
-- == Código
-- @
-- menorElemY :: [(Peca,Coordenadas)] -> (Peca,Coordenadas)
-- menorElemY [] = error "não existe menorElemY de uma lista vazia"
-- menorElemY [x] = x
-- menorElemY ((p,(x,y)):t)
--    |y >= snd(maiorCoordenada [head t]) = menorElemY t
--    |otherwise = menorElemY ( [(p,(x,y))] ++ (tail t))
-- @
menorElemY ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Retorna a Peça e a sua coordenada com o menor Y
  (Peca, Coordenadas)
menorElemY [] = error "não existe menorElemY de uma lista vazia"
menorElemY [x] = x
menorElemY ((p, (x, y)) : t)
  | y >= snd (maiorCoordenada [head t]) = menorElemY t
  | otherwise = menorElemY ([(p, (x, y))] ++ (tail t))

-- | Função: Retorna o maior X
--
-- == Código
-- @
--maiorX [] = error "não existe maiorX de uma lista vazia"
--maiorX [x] = fst(snd(x))
--maiorX ((p, (x, y)) : t)
--  | x <= fst (maiorCoordenada [head t]) = maiorX t
--  | otherwise = x
-- @
maiorX ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Retorna o maior X
  Int
maiorX [] = error "não existe maiorX de uma lista vazia"
maiorX [x] = fst(snd(x))
maiorX ((p, (x, y)) : t)
  | x <= fst (maiorCoordenada [head t]) = maiorX t
  | otherwise = x

-- | Função: Retorna o maior Y
--
-- == Código
-- @
--maiorY [] = error "não existe maiorY de uma lista vazia"
--maiorY [x] = snd(snd(x))
--maiorY ((p, (x, y)) : t)
--  | y <= snd (maiorCoordenada [head t]) = maiorY t
--  | otherwise = y
-- @
maiorY ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Retorna o maior Y
  Int
maiorY [] = error "não existe maiorY de uma lista vazia"
maiorY [x] = snd(snd(x))
maiorY ((p, (x, y)) : t)
  | y <= snd (maiorCoordenada [head t]) = maiorY t
  | otherwise = y

-- | Função: Retorna o elemento com o menor X
--
-- == Código
-- @
-- menorElemX :: [(Peca,Coordenadas)] -> (Peca,Coordenadas)
-- menorElemX [] = error "não existe menorElemX de uma lista vazia"
-- menorElemX [x] = x
-- menorElemX ((p,(x,y)):t)
--    |x >= fst(maiorCoordenada [head t]) = menorElemX t
--    |otherwise = menorElemX ( [(p,(x,y))] ++ (tail t))
-- @
menorElemX ::
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Retorna a Peça e a sua coordenada com o menor X
  (Peca, Coordenadas)
menorElemX [] = error "não existe menorElemX de uma lista vazia"
menorElemX [x] = x
menorElemX ((p, (x, y)) : t)
  | x >= fst (maiorCoordenada [head t]) = menorElemX t
  | otherwise = menorElemX ([(p, (x, y))] ++ (tail t))

-- | Função: Remove um elemento de uma lista
--
-- == Código
-- @
-- removerElem :: (Peca,Coordendas) -> [(Peca,Coordenadas)] -> [(Pe,Coordenadas)]
-- removerElem _ [] = []
-- removerElem x (y:ys)
--    | x == y    = removerElem x ys
--    | otherwise = y : removerElem x ys
-- @
removerElem ::
  -- | Recebe um Tuplo
  (Peca, Coordenadas) ->
  -- | Recebe uma lista
  [(Peca, Coordenadas)] ->
  -- | Remove todos os elementos iguais
  [(Peca, Coordenadas)]
removerElem _ [] = []
removerElem x (y : ys)
  | x == y = removerElem x ys
  | otherwise = y : removerElem x ys
