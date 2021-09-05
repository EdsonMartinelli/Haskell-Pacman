module Pacman where

import Blocos
import GeradorSprite
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Movimento
import Utilidades

data Pacman =
  Pacman
    { posInicial :: (Float, Float)
    , posPacman :: (Float, Float)
    , velPacman :: (Float, Float)
    , velPacmanDesejada :: (Float, Float)
    , spritePacman :: [[Picture]]
    , mapa :: [[Bloco]]
    , bloco :: Bloco
    }

criaPacman :: [String] -> [[Bloco]] -> Float -> Float -> BitmapData -> Pacman
criaPacman linhas blocos tamanhox tamanhoy sprite =
  acha
    [ [ Pacman
      { posInicial = (posX x, posY y)
      , posPacman = (posX x, posY y)
      , velPacman = (velocidadePadrao, 0)
      , velPacmanDesejada = (velocidadePadrao, 0)
      , spritePacman = geraSpritePacman sprite
      , mapa = blocos
      , bloco = blocos !! round y !! round x
      }
    | x <- [0 .. (tamanhox - 1)]
    , linhas !! round y !! round x == 'P'
    ]
    | y <- [0 .. (tamanhoy - 1)]
    ]
  where
    posX x = ((-(tamanhox * tamanho) + tamanho) / 2) + (x * tamanho)
    posY y = (((tamanhoy * tamanho) - tamanho) / 2) - (y * tamanho) + (tamanhoPlacar / 2)

desenhaPacman :: Int -> Int -> Pacman -> Picture
desenhaPacman status tempo pacman
  | status == 1 || status == -1 || tempo <= (tempoInicio * fps) =
    Translate (fst $ posPacman pacman) (snd $ posPacman pacman) (Color (yellow) (circleSolid 12))
  | velPacman pacman == (0, velocidadePadrao) =
    Translate
      (fst $ posPacman pacman)
      (snd $ posPacman pacman)
      ((spritePacman pacman) !! 0 !! (mod (tempo `div` 8) (length ((spritePacman pacman) !! 0))))
  | velPacman pacman == (-velocidadePadrao, 0) =
    Translate
      (fst $ posPacman pacman)
      (snd $ posPacman pacman)
      ((spritePacman pacman) !! 1 !! (mod (tempo `div` 8) (length ((spritePacman pacman) !! 1))))
  | velPacman pacman == (velocidadePadrao, 0) =
    Translate
      (fst $ posPacman pacman)
      (snd $ posPacman pacman)
      ((spritePacman pacman) !! 2 !! (mod (tempo `div` 8) (length ((spritePacman pacman) !! 2))))
  | velPacman pacman == (0, -velocidadePadrao) =
    Translate
      (fst $ posPacman pacman)
      (snd $ posPacman pacman)
      ((spritePacman pacman) !! 3 !! (mod (tempo `div` 8) (length ((spritePacman pacman) !! 3))))
  | otherwise = Translate (fst $ posPacman pacman) (snd $ posPacman pacman) (Color (yellow) (circleSolid 12))

updatePacman :: Pacman -> [[Bloco]] -> Pacman
updatePacman pacman mapaAtual =
  Pacman
    { posInicial = posInicial pacman
    , posPacman =
        atualizaPos (posPacman pacman) (velPacman pacman) (velPacmanDesejada pacman) (bloco pacman) (mapa pacman)
    , velPacman = velNova
    , velPacmanDesejada = velPacmanDesejada pacman
    , spritePacman = spritePacman pacman
    , mapa = mapaAtual
    , bloco = mapaAtual !! (snd $ posBlocoLista blocoEscolhido) !! (fst $ posBlocoLista blocoEscolhido)
    }
  where
    velNova =
      atualizaVelocidade (posPacman pacman) (velPacman pacman) (velPacmanDesejada pacman) (bloco pacman) (mapa pacman)
    blocosElegidos = elegeBlocos $ blocosAdjacentes (bloco pacman) (mapa pacman)
    blocoEscolhido = conectaBloco (mapa pacman) (posPacman pacman) (velPacman pacman) (bloco pacman) (blocosElegidos)

teclasDeControle :: Event -> Pacman -> Pacman
teclasDeControle (EventKey (SpecialKey KeyLeft) Down _ _) pacman@(Pacman {velPacmanDesejada = x}) =
  pacman {velPacmanDesejada = (-velocidadePadrao, 0)}
teclasDeControle (EventKey (SpecialKey KeyRight) Down _ _) pacman@(Pacman {velPacmanDesejada = x}) =
  pacman {velPacmanDesejada = (velocidadePadrao, 0)}
teclasDeControle (EventKey (SpecialKey KeyUp) Down _ _) pacman@(Pacman {velPacmanDesejada = x}) =
  pacman {velPacmanDesejada = (0, velocidadePadrao)}
teclasDeControle (EventKey (SpecialKey KeyDown) Down _ _) pacman@(Pacman {velPacmanDesejada = x}) =
  pacman {velPacmanDesejada = (0, -velocidadePadrao)}
teclasDeControle _ pacman = pacman
