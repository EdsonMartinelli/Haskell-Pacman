module Colisao where

import Fantasma
import Pacman
import Utilidades

colidiu :: Pacman -> [Fantasma] -> Bool
colidiu pacman fantasmas =
  or
    [ colisaoRet (posPacman pacman) (posFantasma fantasma)
    | fantasma <- fantasmas
    ]

colisaoRet :: (Float, Float) -> (Float, Float) -> Bool
colisaoRet (x1, y1) (x2, y2)
  | x1 + (tamanhoPacman / 2) >= x2 &&
      x1 <= x2 + (tamanhoPacman / 2) &&
      y1 + (tamanhoPacman / 2) >= y2 && y1 <= y2 + (tamanhoPacman / 2) = True
  | otherwise = False
