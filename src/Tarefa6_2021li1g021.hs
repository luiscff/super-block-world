{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1gXXX where

import LI12122
import Tarefa4_2021li1g021
import Data.List


-- | Função: O seu objetivo é tentar resover o jogo através dos 4 movimentos possíveis
--
-- == Código
-- @
-- resolveJogo :: Int -> Jogo ->Maybe [Movimento]
-- resolveJogo i jogo = let possivelmovimento = filter chegouporta (analisa i jogo) in if possivelmovimento == [] then Nothing else Just (criarmovimento (head (sortOn length possivelmovimento))) 
-- @
resolveJogo ::
    -- | Recebe um Valo Máximo de Movimentos permitidos  
    Int ->
    -- | Recebe o Jogo para o qual vai resolver 
    Jogo ->
    -- | Devolve uma possibilidade de movimentos 
    Maybe [Movimento]
resolveJogo i jogo = let possivelmovimento = filter chegouporta (analisa i jogo) in if possivelmovimento == [] then Nothing else Just (criarmovimento (head (sortOn length possivelmovimento))) 

-- | Função : Recebe uma lista de Jogo e converte para uma lista de Movimento
--
-- == Código
-- @
-- movimento ::[Jogo] ->[Movimento]
-- movimento [] = []
-- movimento [_] = []
-- movimento (x:y:t) = movimentoaux x y : movimento (y:t) 
--     where
--         movimentoaux :: Jogo -> Jogo -> Movimento
--         movimentoaux (Jogo _ (Jogador (_,y1) _ ca0)) (Jogo _ (Jogador (_,y2) dir ca1))
--             | ca0 /= ca1 = InterageCaixa
--             | y1 > y2 = Trepar
--             | dir == Este = AndarDireita
--             | dir == Oeste = AndarEsquerda
--             | otherwise = error "Algum tipo de dado está incorreto"
-- @

movimento ::
-- | Recebe uma Lista de Jogo
    [Jogo] ->
-- | Devolve uma lista de Movimentos
    [Movimento]
movimento [] = []
movimento [_] = []
movimento (x:y:t) = movimentoaux x y : movimento (y:t) 
    where
        movimentoaux ::
            -- | Recebe um tipo Jogo
            Jogo ->
            -- | Rece outro tipo Jogo
            Jogo ->
            -- | Analisa a diferença existente e converte para Movimento
            Movimento
        movimentoaux (Jogo _ (Jogador (_,y1) _ ca0)) (Jogo _ (Jogador (_,y2) dir ca1))
            | ca0 /= ca1 = InterageCaixa
            | y1 > y2 = Trepar
            | dir == Este = AndarDireita
            | dir == Oeste = AndarEsquerda
            | otherwise = error "Algum tipo de dado está incorreto"


-- | Função: Esta função limita-se a determinar se o Jogador terminou nas mesmas coordenadas das coordenadas da Porta
--
-- == Código
-- @
-- porta :: [Jogo] -> Bool
-- porta jogo = (mapa !! yj) !! xj == Porta 
--     where
--         Jogo mapa (Jogador (xj,yj) _ _) = last jogo
-- @
porta ::
    -- | Recebe uma Lista de Jogos 
    [Jogo] -> 
    -- | Retorna True no caso de o Jogador terminar nas mesmas coordenadas da Porta
    Bool
porta jogo = (mapa !! yj) !! xj == Porta 
    where
        Jogo mapa (Jogador (xj,yj) _ _) = last jogo

-- | Função: Calcula todas as possibilidades de Jogo permitidas através do Jogo inicial
--
-- == Código
-- @
-- analisa :: Int ->Jogo -> [[Jogo]]
-- analisa i jogo = if i <= 0 then [[jogo]] else [jogo] : map (jogo :) (concatMap (analisa (i - 1)) (criajogos jogo))
-- @
analisa ::
    -- | Recebe o valor máximo de possibilidades de Movimento
    Int ->
    -- | Recebe um tipo Jogo (Jogos originados a partir do Jogo inicial) 
    Jogo -> 
    -- | Devolve uma Lista com todos os Jogos possíveis
    [[Jogo]]
analisa i jogo = if i <= 0 then [[jogo]] else [jogo] : map (jogo :) (concatMap (analisa (i - 1)) (criajogos jogo))

-- | Função: Responsável por criar uma lista do tipo Jogo resultante da combinação das 4 movimentações permitidas no jogo
--
-- == Código
-- @
-- criajogos:: Jogo -> [Jogo]
-- criajogos = criajogosaux [AndarEsquerda, AndarDireita, Trepar, InterageCaixa] 
--     where
--     criajogosaux :: [Movimento] -> Jogo -> [Jogo]
--     criajogosaux [] _ = []
--     criajogosaux (h:t) jogo0 = let jogo1 = moveJogador jogo0 h in if jogo1 /= jogo0 then jogo1 : criajogosaux t jogo0 else criajogosaux t jogo0
-- @
criajogos ::
    -- | Recebe um tipo Jogo inicial
    Jogo ->
    -- | Devolve uma lista de Jogos resultante das possibilidades de movimentação
    [Jogo]
criajogos = criajogosaux [AndarEsquerda, AndarDireita, Trepar, InterageCaixa] 
    where
    criajogosaux :: [Movimento] -> Jogo -> [Jogo]
    criajogosaux [] _ = []
    criajogosaux (h:t) jogo0 = let jogo1 = moveJogador jogo0 h in if jogo1 /= jogo0 then jogo1 : criajogosaux t jogo0 else criajogosaux t jogo0