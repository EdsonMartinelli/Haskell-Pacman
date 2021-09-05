module GeradorSprite where

import Graphics.Gloss
import Utilidades

----- Mapa
geraSprite :: [Int] -> BitmapData -> Picture
geraSprite codigo sprite
  | codigo == [1] = bitmapSection (defineCorteMapa (3, 9)) sprite
  | codigo == [0] = (Color black (rectangleSolid tamanho tamanho))
  | codigo == [2] = bitmapSection (defineCorteMapa (3, 8)) sprite
  | codigo == [-1, -1, -1, -1, 0, -1, 0, 1] =
    bitmapSection (defineCorteMapa (0, 1)) sprite
  | codigo == [1, 1, 1, 0, 1, 0, 0, 1] ||
      codigo == [1, 1, 1, 0, 1, -1, 0, 1] ||
      codigo == [-1, 1, 1, -1, 1, -1, 0, 1] =
    bitmapSection (defineCorteMapa (1, 8)) sprite
  | codigo == [-1, -1, -1, 0, 0, 0, 1, 1] ||
      codigo == [-1, -1, -1, 0, 0, 1, 1, 1] ||
      codigo == [-1, -1, -1, 0, 0, 1, 1, 0] ||
      codigo == [-1, -1, -1, -1, 0, -1, 1, 1] ||
      codigo == [-1, -1, 0, 0, 0, 1, 1, 1] ||
      codigo == [0, -1, -1, 0, 0, 1, 1, 1] ||
      codigo == [-1, -1, -1, 0, -1, 1, 1, -1] ||
      codigo == [-1, -1, 0, 0, 0, 0, 1, 1] ||
      codigo == [0, -1, -1, 0, 0, 1, 1, 0] =
    bitmapSection (defineCorteMapa (1, 0)) sprite
  | codigo == [-1, -1, -1, 0, 0, 1, 0, 0] =
    bitmapSection (defineCorteMapa (3, 5)) sprite
  | codigo == [-1, -1, -1, 0, 0, 0, 0, 1] =
    bitmapSection (defineCorteMapa (3, 4)) sprite
  | codigo == [-1, -1, -1, 0, -1, 1, 0, -1] =
    bitmapSection (defineCorteMapa (0, 0)) sprite
  | codigo == [-1, 0, 0, -1, 1, -1, 0, 1] ||
      codigo == [-1, 0, 1, -1, 1, -1, 0, 1] ||
      codigo == [-1, 0, 1, -1, 1, -1, 0, 0] ||
      codigo == [0, 0, 1, -1, 1, -1, 0, 1] ||
      codigo == [-1, 0, 1, -1, 1, 0, 0, 1] ||
      codigo == [0, 0, 1, -1, 1, 0, 0, 1] =
    bitmapSection (defineCorteMapa (0, 3)) sprite
  | codigo == [0, 0, 0, 1, 0, 1, 0, 0] ||
      codigo == [1, 0, 0, 1, 0, 1, 0, 0] || codigo == [1, 0, 0, 1, 0, 0, 0, 0] =
    bitmapSection (defineCorteMapa (2, 1)) sprite
  | codigo == [0, 0, 0, 0, 1, 0, 0, 1] ||
      codigo == [0, 0, 1, 0, 1, 0, 0, 1] || codigo == [0, 0, 1, 0, 1, 0, 0, 0] =
    bitmapSection (defineCorteMapa (2, 0)) sprite
  | codigo == [0, 0, -1, 1, -1, 1, 0, -1] ||
      codigo == [1, 0, -1, 1, -1, 1, 0, -1] ||
      codigo == [1, 0, -1, 1, -1, 0, 0, -1] ||
      codigo == [1, 0, 0, 1, -1, 1, 0, -1] ||
      codigo == [1, 0, -1, 1, -1, 1, 0, 0] ||
      codigo == [1, 0, 0, 1, -1, 1, 0, 0] =
    bitmapSection (defineCorteMapa (0, 2)) sprite
  | codigo == [1, 1, 1, 1, 0, 1, 0, 0] ||
      codigo == [1, 1, 1, 1, 0, 1, 0, -1] ||
      codigo == [1, 1, -1, 1, -1, 1, 0, -1] =
    bitmapSection (defineCorteMapa (1, 9)) sprite
  | codigo == [1, 1, 1, 0, 0, 0, 0, 0] ||
      codigo == [0, 1, 1, 0, 0, 0, 0, 0] ||
      codigo == [1, 1, 0, 0, 0, 0, 0, 0] ||
      codigo == [-1, 1, 1, -1, 0, -1, 0, 0] ||
      codigo == [1, 1, -1, 0, -1, 0, 0, -1] =
    bitmapSection (defineCorteMapa (1, 2)) sprite
  | codigo == [0, 0, 0, 0, 0, 0, 0, 0] = (Color black (rectangleSolid 16 16))
  | codigo == [1, 0, 0, 1, 0, 1, 1, 1] || codigo == [1, 0, -1, 1, 0, 1, 1, 1] =
    bitmapSection (defineCorteMapa (2, 3)) sprite
  | codigo == [0, 0, 0, 0, 0, 1, 1, 1] ||
      codigo == [0, 0, 0, 0, 0, 1, 1, 0] ||
      codigo == [0, 0, 0, 0, 0, 0, 1, 1] ||
      codigo == [-1, 0, 0, -1, 0, -1, 1, 1] =
    bitmapSection (defineCorteMapa (1, 7)) sprite
  | codigo == [0, 0, 1, 0, 1, 1, 1, 1] || codigo == [-1, 0, 1, 0, 1, 1, 1, 1] =
    bitmapSection (defineCorteMapa (2, 2)) sprite
  | codigo == [0, 0, 0, 0, 0, 1, 0, 0] =
    bitmapSection (defineCorteMapa (3, 1)) sprite
  | codigo == [0, 0, 0, 0, 0, 0, 0, 1] =
    bitmapSection (defineCorteMapa (3, 0)) sprite
  | codigo == [-1, 0, 1, -1, 0, -1, -1, -1] =
    bitmapSection (defineCorteMapa (0, 5)) sprite
  | codigo == [0, 1, 1, 0, 0, 1, 1, 1] ||
      codigo == [0, 1, 1, 0, 0, -1, -1, -1] ||
      codigo == [1, 1, 1, 0, 0, -1, -1, 0] ||
      codigo == [1, 1, 0, 0, 0, 0, -1, -1] ||
      codigo == [1, 1, 1, 0, 0, 0, -1, -1] ||
      codigo == [1, 1, 0, 0, 0, -1, -1, -1] ||
      codigo == [-1, 1, 1, -1, 0, -1, -1, -1] ||
      codigo == [1, 1, -1, 0, -1, -1, -1, -1] ||
      codigo == [0, 1, 1, 0, 0, -1, -1, 0] ||
      codigo == [1, 1, 1, 0, 0, -1, -1, -1] =
    bitmapSection (defineCorteMapa (1, 1)) sprite
  | codigo == [0, 0, 1, 0, 0, 0, 0, 0] =
    bitmapSection (defineCorteMapa (3, 2)) sprite
  | codigo == [1, 0, 0, 0, 0, 0, 0, 0] =
    bitmapSection (defineCorteMapa (3, 3)) sprite
  | codigo == [1, 0, -1, 0, -1, 1, 1, -1] ||
      codigo == [1, 0, -1, 0, -1, -1, -1, -1] =
    bitmapSection (defineCorteMapa (0, 4)) sprite
  | codigo == [-1, 0, 1, -1, 0, -1, 0, 0] =
    bitmapSection (defineCorteMapa (0, 7)) sprite
  | codigo == [1, 0, -1, 0, -1, 0, 0, -1] =
    bitmapSection (defineCorteMapa (0, 6)) sprite
  | codigo == [-1, 0, 0, -1, 0, -1, 0, 1] =
    bitmapSection (defineCorteMapa (0, 9)) sprite
  | codigo == [0, 0, -1, 0, -1, 1, 0, -1] =
    bitmapSection (defineCorteMapa (0, 8)) sprite
  | codigo == [1, 0, 0, 0, 0, -1, -1, -1] =
    bitmapSection (defineCorteMapa (3, 6)) sprite
  | codigo == [0, 0, 1, 0, 0, -1, -1, -1] =
    bitmapSection (defineCorteMapa (3, 7)) sprite
  | codigo == [1, 1, 1, 0, 2, -1, -1, -1] =
    bitmapSection (defineCorteMapa (2, 9)) sprite
  | codigo == [1, 1, 1, 2, 0, -1, -1, -1] =
    bitmapSection (defineCorteMapa (2, 8)) sprite
  | otherwise = (Color red (rectangleSolid tamanho tamanho))

defineCorteMapa :: (Int, Int) -> Rectangle
defineCorteMapa (a, b)
  | a == 3 =
    Rectangle
      (b * (round tamanho), 0 * (round tamanho))
      ((round tamanho), (round tamanho))
  | a == 2 =
    Rectangle
      (b * (round tamanho), 1 * (round tamanho))
      ((round tamanho), (round tamanho))
  | a == 1 =
    Rectangle
      (b * (round tamanho), 2 * (round tamanho))
      ((round tamanho), (round tamanho))
  | a == 0 =
    Rectangle
      (b * (round tamanho), 3 * (round tamanho))
      ((round tamanho), (round tamanho))
  | otherwise =
    Rectangle
      (b * (round tamanho), a * (round tamanho))
      ((round tamanho), (round tamanho))

----- Fantasmas
geraSpriteFantasma :: Char -> BitmapData -> [[Picture]]
geraSpriteFantasma letra sprite
  | letra == 'B' = escolheSprite 0 sprite
  | letra == 'R' = escolheSprite 3 sprite
  | letra == 'C' = escolheSprite 1 sprite
  | letra == 'I' = escolheSprite 2 sprite

escolheSprite :: Int -> BitmapData -> [[Picture]]
escolheSprite id sprite =
  [ [ bitmapSection (defineCorteObjeto (id, 0)) sprite
    , bitmapSection (defineCorteObjeto (id, 1)) sprite
    ]
  , [ bitmapSection (defineCorteObjeto (id, 2)) sprite
    , bitmapSection (defineCorteObjeto (id, 3)) sprite
    ]
  , [ bitmapSection (defineCorteObjeto (id, 4)) sprite
    , bitmapSection (defineCorteObjeto (id, 5)) sprite
    ]
  , [ bitmapSection (defineCorteObjeto (id, 6)) sprite
    , bitmapSection (defineCorteObjeto (id, 7)) sprite
    ]
  ]

geraSpritePacman :: BitmapData -> [[Picture]]
geraSpritePacman sprite =
  [ [ bitmapSection (defineCorteObjeto (0, 0)) sprite
    , bitmapSection (defineCorteObjeto (0, 1)) sprite
    , bitmapSection (defineCorteObjeto (0, 2)) sprite
    ]
  , [ bitmapSection (defineCorteObjeto (0, 3)) sprite
    , bitmapSection (defineCorteObjeto (0, 4)) sprite
    , bitmapSection (defineCorteObjeto (0, 5)) sprite
    ]
  , [ bitmapSection (defineCorteObjeto (0, 6)) sprite
    , bitmapSection (defineCorteObjeto (0, 7)) sprite
    , bitmapSection (defineCorteObjeto (0, 8)) sprite
    ]
  , [ bitmapSection (defineCorteObjeto (0, 9)) sprite
    , bitmapSection (defineCorteObjeto (0, 10)) sprite
    , bitmapSection (defineCorteObjeto (0, 11)) sprite
    ]
  ]

defineCorteObjeto :: (Int, Int) -> Rectangle
defineCorteObjeto (a, b) =
  Rectangle
    (b * (round tamanhoFantasma), a * (round tamanhoFantasma))
    ((round tamanhoFantasma), (round tamanhoFantasma))

geraSpriteTexto :: BitmapData -> [Picture]
geraSpriteTexto sprite =
  [ Scale 0.6 0.6 (bitmapSection (defineCorteTexto (3, 0)) sprite)
  , Scale 0.6 0.6 (bitmapSection (defineCorteTexto (2, 0)) sprite)
  , Scale 0.6 0.6 (bitmapSection (defineCorteTexto (1, 0)) sprite)
  , Scale 0.6 0.6 (bitmapSection (defineCorteTexto (0, 0)) sprite)
  ]

defineCorteTexto :: (Int, Int) -> Rectangle
defineCorteTexto (a, b) = Rectangle (b, a * (round tamanhoTexto)) (288, 32)
