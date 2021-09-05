module Blocos where

import Data.Tuple
import GeradorSprite
import Graphics.Gloss
import Utilidades

data Bloco =
  Bloco
    { posBloco :: (Float, Float)
    , posBlocoLista :: (Int, Int)
    , tipo :: String
    , spriteBloco :: Picture
    }

criaBlocos :: [String] -> Float -> Float -> BitmapData -> [[Bloco]]
criaBlocos linhas tamanhox tamanhoy spriteBloco =
  [ [ Bloco
    { posBloco = (posX x, posY y)
    , posBlocoLista = (round x, round y)
    , tipo = escolheTipo (linhas !! round y !! round x)
    , spriteBloco = geraSprite (geraCodigo x y) spriteBloco
    }
  | x <- [0 .. (tamanhox - 1)]
  ]
  | y <- [0 .. (tamanhoy - 1)]
  ]
  where
    posX x = ((-(tamanhox * tamanho) + tamanho) / 2) + (x * tamanho)
    posY y = (((tamanhoy * tamanho) - tamanho) / 2) - (y * tamanho) + (tamanhoPlacar / 2)
    geraCodigo x y = criaCodigo linhas (round x) (round y) (round (tamanhox - 1)) (round (tamanhoy - 1))

criaCodigo :: [String] -> Int -> Int -> Int -> Int -> [Int]
criaCodigo linhas x y tamanhox tamanhoy
  | linhas !! y !! x == 'X' =
    [ (verificaCodigo linhas (x - 1) (y - 1) tamanhox tamanhoy)
    , (verificaCodigo linhas (x) (y - 1) tamanhox tamanhoy)
    , (verificaCodigo linhas (x + 1) (y - 1) tamanhox tamanhoy)
    , (verificaCodigo linhas (x - 1) (y) tamanhox tamanhoy)
    , (verificaCodigo linhas (x + 1) (y) tamanhox tamanhoy)
    , (verificaCodigo linhas (x - 1) (y + 1) tamanhox tamanhoy)
    , (verificaCodigo linhas (x) (y + 1) tamanhox tamanhoy)
    , (verificaCodigo linhas (x + 1) (y + 1) tamanhox tamanhoy)
    ]
  | linhas !! y !! x == '.' = [1]
  | linhas !! y !! x == 'G' = [2]
  | otherwise = [0]

verificaCodigo :: [String] -> Int -> Int -> Int -> Int -> Int
verificaCodigo linhas x y tamanhox tamanhoy
  | x < 0 || y < 0 || x > tamanhox || y > tamanhoy = -1
  | otherwise =
    if (linhas !! y !! x == 'V' ||
        linhas !! y !! x == 'C' || linhas !! y !! x == 'B' || linhas !! y !! x == 'R' || linhas !! y !! x == 'I')
      then -1
      else if linhas !! y !! x == 'X'
             then 0
             else if linhas !! y !! x == 'G'
                    then 2
                    else 1

{-
   V é a definição de dentro do programa e do texto para as bordas que estourariam o
   vetor, X é definido dentro do arquivo de texto, onde esse é a parede, além de
   outras marcações que podem ser pontos, o pacman, fantasmas ou caminhos.
-}
escolheTipo :: Char -> String
escolheTipo caracter
  | caracter == 'X' = "Parede"
  | caracter == '.' = "Ponto"
  | caracter == 'G' = "Portao"
  | otherwise = "Caminho"

updateBloco :: [[Bloco]] -> (Float, Float) -> (Float, Float) -> [[Bloco]]
updateBloco blocoss pos vel =
  [ [ if posFutura == posBloco bloco
    then atualizaBloco (bloco)
    else bloco
  | bloco <- blocos
  ]
  | blocos <- blocoss
  ]
  where
    posFutura = ((fst $ pos) + (fst $ vel) * 2, (snd $ pos) + (snd $ vel) * 2)

{-
   É preciso da velocidade e posição do pacman devido ao loop do gloss que funciona
   baseado na posição antiga no pacman, assim é preciso prever a nova posição.
-}
atualizaBloco :: Bloco -> Bloco
atualizaBloco x@(Bloco {tipo = t, spriteBloco = sprite})
  | tipo x == "Ponto" = x {tipo = "Caminho", spriteBloco = (Color black (rectangleSolid 16 16))}
  | otherwise = x

desenhaBlocos :: [[Bloco]] -> Picture
desenhaBlocos blocoss =
  Pictures
    [ Pictures [Translate (fst $ posBloco bloco) (snd $ posBloco bloco) (spriteBloco bloco) | bloco <- blocos]
    | blocos <- blocoss
    ]
