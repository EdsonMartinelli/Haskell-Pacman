module Utilidades where

tamanho = 16 :: Float -- Tamanho do bloco do mapa

tamanhoPlacar = 60 :: Float

tamanhoFantasma = 24 :: Float

tamanhoPacman = 24 :: Float

fps = 120 :: Int

tempoInicio = 3 :: Int

tempoBlinky = 3 :: Int

tempoClyde = 4 :: Int

tempoInky = 5 :: Int

tempoPinky = 6 :: Int

tamanhoTexto = 32 :: Float

acha :: [[a]] -> a
acha (x:xs) =
  if null x
    then acha xs
    else head x
