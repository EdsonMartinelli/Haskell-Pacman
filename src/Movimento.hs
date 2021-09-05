module Movimento where

import Blocos
import Graphics.Gloss
import Utilidades

velocidadePadrao = 1 :: Float

blocoNulo = Bloco {posBloco = (0, 0), posBlocoLista = (0, 0), tipo = "Nulo", spriteBloco = blank} :: Bloco

blocosAdjacentes :: Bloco -> [[Bloco]] -> [Bloco]
blocosAdjacentes x mapaAtual =
  [ verificaFinal mapaAtual (posYBloco + 1) posXBloco
  , verificaFinal mapaAtual posYBloco (posXBloco - 1)
  , verificaFinal mapaAtual posYBloco (posXBloco + 1)
  , verificaFinal mapaAtual (posYBloco - 1) posXBloco
  ]
  where
    posXBloco = (fst $ posBlocoLista x)
    posYBloco = (snd $ posBlocoLista x)

{-
   [bloco 1, bloco 2, bloco 3, bloco 4]
                +---+
                | 4 |
            +---+---+---+
            | 2 | x | 3 |
            +---+---+---+
                | 1 |
                +---+

    O y esta invertido pois o gloss não usa o sistema cartesiano comum.
-}
verificaFinal :: [[Bloco]] -> Int -> Int -> Bloco
verificaFinal mapaAtual y x
  | x > maxX - 1 = mapaAtual !! y !! 0
  | y > maxY - 1 = mapaAtual !! 0 !! x
  | x < 0 = mapaAtual !! y !! (maxX - 1)
  | y < 0 = mapaAtual !! (maxY - 1) !! x
  | otherwise = mapaAtual !! y !! x
  where
    maxX = (length (head mapaAtual))
    maxY = (length mapaAtual)

{-
    Verifica se os bloco não está na beirada para conseguir utilizar
    a função anterior em um Warp.
-}
elegeBlocos :: [Bloco] -> [Bloco]
elegeBlocos [] = []
elegeBlocos (x:blocos)
  | tipo x == "Parede" || tipo x == "Portao" = [blocoNulo] ++ elegeBlocos blocos
  | otherwise = [x] ++ elegeBlocos blocos

{-
    Define, dado os blocos adjacentes, os blocos aptos se tornarem
    blocos conectados ao objeto (pacman ou ao fantasma), colocando
    um bloco nulo no lugar dos não aptos para facilitar a localização
    pela posição na lista.
-}
conectaBloco :: [[Bloco]] -> (Float, Float) -> (Float, Float) -> Bloco -> [Bloco] -> Bloco
conectaBloco _ _ _ blocoAtual [] = blocoAtual
conectaBloco mapaAtual pos vel blocoAtual (x:blocos)
  | (posBloco x == posFuturaNormal && tipo x /= "Nulo") ||
      (posBloco x == posFuturaXmax && tipo x /= "Nulo") ||
      (posBloco x == posFuturaXmin && tipo x /= "Nulo") ||
      (posBloco x == posFuturaYmax && tipo x /= "Nulo") || (posBloco x == posFuturaYmin && tipo x /= "Nulo") = x
  | otherwise = conectaBloco mapaAtual pos vel blocoAtual blocos
  where
    posFuturaNormal = ((fst $ pos) + (fst $ vel) * 2, (snd $ pos) + (snd $ vel) * 2)
    posFuturaXmax = (posXmax + (fst $ vel) * 2, (snd $ pos) + (snd $ vel) * 2)
    posFuturaXmin = (posXmin + (fst $ vel) * 2, (snd $ pos) + (snd $ vel) * 2)
    posFuturaYmax = ((fst $ pos) + (fst $ vel) * 2, posYmin + (snd $ vel) * 2)
    posFuturaYmin = ((fst $ pos) + (fst $ vel) * 2, posYmax + (snd $ vel) * 2)
    posXmax = fst $ posMax mapaAtual
    posYmax = snd $ posMax mapaAtual
    posXmin = fst $ posMin mapaAtual
    posYmin = snd $ posMin mapaAtual

{-
    Conecta o bloco escolhido ao objeto um loop antes que esse
    chegue no centro do bloco, levando em conta a direção que
    é pega a partir da velocidade, desclassificando os blocos
    nulos.
-}
paraEmParede :: (Float, Float) -> Bloco -> [[Bloco]] -> (Float, Float)
paraEmParede vel blocoAtual mapaAtual
  | (fst $ vel) > 0 && tipo (blocoProcurado blocoAtual mapaAtual 2) == "Nulo" = (0, 0)
  | (fst $ vel) < 0 && tipo (blocoProcurado blocoAtual mapaAtual 1) == "Nulo" = (0, 0)
  | (snd $ vel) < 0 && tipo (blocoProcurado blocoAtual mapaAtual 0) == "Nulo" = (0, 0)
  | (snd $ vel) > 0 && tipo (blocoProcurado blocoAtual mapaAtual 3) == "Nulo" = (0, 0)
  | otherwise = vel

{-
    Verifica a colisão com paredes e caso ocorra zera a velocidade.
-}
blocoProcurado :: Bloco -> [[Bloco]] -> Int -> Bloco
blocoProcurado blocoAtual mapaAtual id = (elegeBlocos $ blocosAdjacentes blocoAtual mapaAtual) !! id

{-
    Retorna o bloco escolhido dado a posição na lista.
-}
atualizaVelocidade :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bloco -> [[Bloco]] -> (Float, Float)
atualizaVelocidade pos vel veld blocoAtual mapaAtual
  | veld == vel = paraEmParede vel blocoAtual mapaAtual
  | podeVirar pos vel veld blocoAtual mapaAtual == True = veld
  | otherwise = paraEmParede vel blocoAtual mapaAtual

{-
    Atualiza a velocidade com base no estado do objeto.
-}
podeVirar :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bloco -> [[Bloco]] -> Bool
podeVirar pos vel veld blocoAtual mapaAtual
  | not estaNoMeio && not mesmoEixo = False
  | (fst $ veld) > 0 && tipo (blocoProcurado blocoAtual mapaAtual 2) == "Nulo" = False
  | (fst $ veld) < 0 && tipo (blocoProcurado blocoAtual mapaAtual 1) == "Nulo" = False
  | (snd $ veld) < 0 && tipo (blocoProcurado blocoAtual mapaAtual 0) == "Nulo" = False
  | (snd $ veld) > 0 && tipo (blocoProcurado blocoAtual mapaAtual 3) == "Nulo" = False
  | otherwise = True
  where
    blocoEleitos = (elegeBlocos $ blocosAdjacentes blocoAtual mapaAtual)
    estaNoMeio = posBloco (conectaBloco mapaAtual pos vel blocoAtual blocoEleitos) == pos
    mesmoEixo = ((fst $ vel) /= 0 && (fst $ veld) /= 0) || ((snd $ vel) /= 0 && (snd $ veld) /= 0)

{-
    Verifica a cada loop se o objeto pode utilizar a velocidade
    desejada para ir para a direção, funciona como uma maneira
    de guardar o comando do usuario ou do proprio jogo, para que
    não seja preciso acertar o meio do bloco no exato frame para se
    movimentar.
-}
atualizaPos :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bloco -> [[Bloco]] -> (Float, Float)
atualizaPos pos vel veld blocoAtual mapaAtual
  | paraEmParede vel blocoAtual mapaAtual == (0, 0) = ((fst $ pos) + (fst $ vel), (snd $ pos) + (snd $ vel))
  | otherwise = atualizaVel
  where
    atualizaVel = verificaWarps pos vel veld blocoAtual mapaAtual

verificaWarps :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bloco -> [[Bloco]] -> (Float, Float)
verificaWarps pos vel veld blocoAtual mapaAtual
  | (fst $ pos) > posXmax + (velocidadePadrao * 2) = (posXmin - (velocidadePadrao * 2), (snd $ pos))
  | (fst $ pos) < posXmin - (velocidadePadrao * 2) = (posXmax + (velocidadePadrao * 2), (snd $ pos))
  | (snd $ pos) < posYmax + (velocidadePadrao * 2) = ((fst $ pos), posYmin - (velocidadePadrao * 2))
  | (snd $ pos) > posYmin - (velocidadePadrao * 2) = ((fst $ pos), posYmax + (velocidadePadrao * 2))
  | otherwise = ((fst $ pos) + (fst $ atualizaVel), (snd $ pos) + (snd $ atualizaVel))
  where
    atualizaVel = atualizaVelocidade pos vel veld blocoAtual mapaAtual
    posXmax = fst $ posMax mapaAtual
    posYmax = snd $ posMax mapaAtual
    posXmin = fst $ posMin mapaAtual
    posYmin = snd $ posMin mapaAtual

posMax :: [[Bloco]] -> (Float, Float)
posMax mapaAtual = (posXmax, posYmax)
  where
    tamaxx = fromIntegral (length (head mapaAtual))
    tamaxy = fromIntegral (length mapaAtual)
    posXmax = ((-(tamaxx * tamanho) + tamanho) / 2) + ((tamaxx - 1) * tamanho)
    posYmax = (((tamaxy * tamanho) - tamanho) / 2) - ((tamaxy - 1) * tamanho) + (tamanhoPlacar / 2)

{-
    Maior posição do labirinto.
-}
posMin :: [[Bloco]] -> (Float, Float)
posMin mapaAtual = (posXmin, posYmin)
  where
    tamaxx = fromIntegral (length (head mapaAtual))
    tamaxy = fromIntegral (length mapaAtual)
    posXmin = ((-(tamaxx * tamanho) + tamanho) / 2)
    posYmin = (((tamaxy * tamanho) - tamanho) / 2) + (tamanhoPlacar / 2)
{-
    Menor posição do labirinto.
-}
