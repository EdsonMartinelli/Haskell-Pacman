import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Colisao
import Data.List
import Data.Ord
import Graphics.Gloss
import Jogo
import Test.QuickCheck
import Utilidades

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "Testando atualizaListaRandoms" $ propAtualizaListaRandoms
    , QC.testProperty "Testando acha" $ (propAcha :: [[Int]] -> Bool)
    ]

propAtualizaListaRandoms lista
  | length lista < 5 = True
  | otherwise = lista !! 4 == head (atualizaListaRandoms lista)

propAcha xss
  | listaVazia = True
  | otherwise = head [head xs | xs <- xss, length xs > 0] == acha xss
  where
    listaVazia = null [xs | xs <- xss, length xs > 0]
