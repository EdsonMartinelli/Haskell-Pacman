module Main where

import Blocos
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Jogo
import Pacman
import System.Random
import Utilidades

import Test.QuickCheck

main :: IO ()
main = do
  mapa <- readFile "mapas/mapa.txt"
  let linhas = processaLinhas (lines mapa)
  Bitmap sprite <- loadBMP "recursos/mapaSprites/mapa.bmp"
  Bitmap spriteTexto <- loadBMP "recursos/textoSprites/texto.bmp"
  Bitmap spritePacman <- loadBMP "recursos/pacmanSprites/pacman.bmp"
  Bitmap spriteFantasma <- loadBMP "recursos/fantasmaSprites/fantasmas.bmp"
  let tamanhox = (calculaMapa linhas) !! 0
  let tamanhoy = length $ calculaMapa linhas
  semente <- getStdGen
  let listaRandoms = (randoms semente :: [Int])
  play
    (InWindow "Pacman" ((tamanhox * (round tamanho)), ((tamanhoy * (round tamanho))) + (round tamanhoPlacar)) (0, 0))
    black
    fps
    (criaJogo linhas (fromIntegral tamanhox) (fromIntegral tamanhoy) sprite listaRandoms spriteFantasma spritePacman spriteTexto)
    desenhar
    handleKeys
    update
  where
    processaLinhas :: [String] -> [String]
    processaLinhas a = [[y | y <- x, y /= '\r'] | x <- a]
    calculaMapa :: [String] -> [Int]
    calculaMapa a = [length x | x <- a]
