module Fantasma where

import Blocos
import GeradorSprite
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Movimento
import Utilidades

data Fantasma =
  Fantasma
    { idFantasma :: Int
    , posInicial :: (Float, Float)
    , posFantasma :: (Float, Float)
    , velFantasma :: (Float, Float)
    , velFantasmaDesejada :: (Float, Float)
    , spriteFantasma :: [[Picture]]
    , mapa :: [[Bloco]]
    , blocoFantasma :: Bloco
    , tempoSaida :: Int
    }

criaFantasma :: Char -> [String] -> [[Bloco]] -> Float -> Float -> BitmapData -> Fantasma
criaFantasma letra linhas blocos tamanhox tamanhoy sprite =
  acha
    [ [ Fantasma
      { idFantasma = geraId letra
      , posInicial = (posX x, posY y)
      , posFantasma = (posX x, posY y)
      , velFantasma = (0, velocidadePadrao)
      , velFantasmaDesejada = (0, velocidadePadrao)
      , spriteFantasma = geraSpriteFantasma letra sprite
      , mapa = blocos
      , blocoFantasma = blocos !! round y !! round x
      , tempoSaida = decideTempo letra
      }
    | x <- [0 .. (tamanhox - 1)]
    , linhas !! round y !! round x == letra
    ]
    | y <- [0 .. (tamanhoy - 1)]
    ]
  where
    posX x = ((-(tamanhox * tamanho) + tamanho) / 2) + (x * tamanho)
    posY y = (((tamanhoy * tamanho) - tamanho) / 2) - (y * tamanho) + (tamanhoPlacar / 2)

desenhaFantasma :: Int -> Int -> Fantasma -> Picture
desenhaFantasma status tempo fantasma
  | status == -1 || tempo <= 360 =
    Translate (fst $ posFantasma fantasma) (snd $ posFantasma fantasma) ((spriteFantasma fantasma) !! 0 !! 0)
  | status == 1 = blank
  | velFantasma fantasma == (0, velocidadePadrao) =
    Translate
      (fst $ posFantasma fantasma)
      (snd $ posFantasma fantasma)
      ((spriteFantasma fantasma) !! 0 !! (mod (tempo `div` 8) (length ((spriteFantasma fantasma) !! 0))))
  | velFantasma fantasma == (-velocidadePadrao, 0) =
    Translate
      (fst $ posFantasma fantasma)
      (snd $ posFantasma fantasma)
      ((spriteFantasma fantasma) !! 1 !! (mod (tempo `div` 8) (length ((spriteFantasma fantasma) !! 1))))
  | velFantasma fantasma == (velocidadePadrao, 0) =
    Translate
      (fst $ posFantasma fantasma)
      (snd $ posFantasma fantasma)
      ((spriteFantasma fantasma) !! 2 !! (mod (tempo `div` 8) (length ((spriteFantasma fantasma) !! 2))))
  | velFantasma fantasma == (0, -velocidadePadrao) =
    Translate
      (fst $ posFantasma fantasma)
      (snd $ posFantasma fantasma)
      ((spriteFantasma fantasma) !! 3 !! (mod (tempo `div` 8) (length ((spriteFantasma fantasma) !! 3))))
  | otherwise =
    Translate (fst $ posFantasma fantasma) (snd $ posFantasma fantasma) ((spriteFantasma fantasma) !! 3 !! 0)

updateFantasma :: [Int] -> Int -> [[Bloco]] -> Fantasma -> Fantasma
updateFantasma listaRandoms tempo mapaAtual fantasma =
  Fantasma
    { idFantasma = idFantasma fantasma
    , posInicial = posInicial fantasma
    , posFantasma = posNova
    , velFantasma = velNova
    , velFantasmaDesejada =
        decideMovimento
          (tempoSaida fantasma)
          (idFantasma fantasma)
          tempo
          listaRandoms
          (velFantasma fantasma)
          velNova
          (posNova)
          blocoNovo
          mapaAtual
    , spriteFantasma = spriteFantasma fantasma
    , mapa = mapaAtual
    , blocoFantasma = blocoNovo
    , tempoSaida = tempoSaida fantasma
    }
  where
    posNova =
      atualizaPos
        (posFantasma fantasma)
        (velFantasma fantasma)
        (velFantasmaDesejada fantasma)
        (blocoFantasma fantasma)
        (mapa fantasma)
    velNova
      | tipo (blocosElegidos !! 3) == "Portao" && tempo >= (tempoSaida fantasma) = (0, velocidadePadrao)
      | otherwise =
        atualizaVelocidade
          (posFantasma fantasma)
          (velFantasma fantasma)
          (velFantasmaDesejada fantasma)
          (blocoFantasma fantasma)
          (mapa fantasma)
    blocoNovo = mapaAtual !! (snd $ posBlocoLista blocoEscolhido) !! (fst $ posBlocoLista blocoEscolhido)
    blocosElegidos = elegeBlocosFantasma $ blocosAdjacentes (blocoFantasma fantasma) (mapa fantasma)
    blocoEscolhido =
      conectaBloco
        (mapa fantasma)
        (posFantasma fantasma)
        (velFantasma fantasma)
        (blocoFantasma fantasma)
        (blocosElegidos)

decideMovimento ::
     Int
  -> Int
  -> Int
  -> [Int]
  -> (Float, Float)
  -> (Float, Float)
  -> (Float, Float)
  -> Bloco
  -> [[Bloco]]
  -> (Float, Float)
decideMovimento tempoFantasma id tempo listaRandoms (velXAntiga, velYAntiga) (velXNova, velYNova) pos blocoAtual mapaAtual
  | tempo < tempoFantasma = iniciaFantasma velAntiga velNova blocoAtual mapaAtual
  | not estaNoMeio = velNova
  | velocidadeRandom == (-velXAntiga, -velYAntiga) =
    decideMovimento tempoFantasma id tempo (tail listaRandoms) velAntiga velNova pos blocoAtual mapaAtual
  | velNova == (0, 0) || estaEmInterseccao = velocidadeRandom
  | otherwise = velNova
  where
    velAntiga = (velXAntiga, velYAntiga)
    velNova = (velXNova, velYNova)
    blocosEleitos = (elegeBlocosFantasma $ blocosAdjacentes blocoAtual mapaAtual)
    estaNoMeio = posBloco (conectaBloco mapaAtual pos velNova blocoAtual blocosEleitos) == pos
    estaEmInterseccao = sum [1 | bloco <- blocosEleitos, (tipo bloco) /= "Nulo"] >= 3
    velocidadeRandom = getRandomVelocidade id listaRandoms

getRandomVelocidade :: Int -> [Int] -> (Float, Float)
getRandomVelocidade id listaRandoms
  | random `mod` 2 == 0 =
    if random `mod` 4 == 0
      then (velocidadePadrao, 0)
      else (-velocidadePadrao, 0)
  | otherwise =
    if (random + 1) `mod` 4 == 0
      then (0, velocidadePadrao)
      else (0, -velocidadePadrao)
  where
    random = listaRandoms !! id

elegeBlocosFantasma :: [Bloco] -> [Bloco]
elegeBlocosFantasma [] = []
elegeBlocosFantasma (x:blocos)
  | tipo x == "Parede" = [blocoNulo] ++ elegeBlocosFantasma blocos
  | otherwise = [x] ++ elegeBlocosFantasma blocos

iniciaFantasma :: (Float, Float) -> (Float, Float) -> Bloco -> [[Bloco]] -> (Float, Float)
iniciaFantasma vel veld blocoAtual mapaAtual
  | paraEmParede vel blocoAtual mapaAtual == (0, 0) && vel == (0, -velocidadePadrao) = (0, velocidadePadrao)
  | paraEmParede vel blocoAtual mapaAtual == (0, 0) && vel == (0, velocidadePadrao) = (0, -velocidadePadrao)
  | otherwise = vel

geraId :: Char -> Int
geraId letra
  | letra == 'C' = 0
  | letra == 'B' = 1
  | letra == 'R' = 2
  | letra == 'I' = 3

decideTempo :: Char -> Int
decideTempo letra
  | letra == 'C' = tempoClyde * fps
  | letra == 'B' = tempoBlinky * fps
  | letra == 'R' = tempoPinky * fps
  | letra == 'I' = tempoInky * fps
