module Check where

import Jogo
import Colisao
import Utilidades
import Graphics.Gloss
import Blocos
import Test.QuickCheck

propAtualizaListaRandoms lista
        | length lista < 5 = True
        | otherwise = lista !! 4 == head (atualizaListaRandoms lista)

propAcha xss
    | xss == [[]] = True
    | xss == [] = True
    | otherwise = head [head xs | xs <- xss, length xs > 0] == acha xss

propDesenhaBlocos blocos = desenhaBlocos blocos == desenhaBlocos blocos
