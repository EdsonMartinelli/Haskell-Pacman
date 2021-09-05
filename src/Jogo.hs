module Jogo where

import Blocos
import Colisao
import Fantasma
import GeradorSprite
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Movimento
import Pacman
import Utilidades

data Jogo =
  Jogo
    { mapaJogo :: [[Bloco]]
    , pacmanJogo :: Pacman
    , fantasmas :: [Fantasma]
    , tempo :: Int
    , pontos :: Int
    , listaRandoms :: [Int]
    , pontuacaoMaxima :: Int
    , status :: Int
    , textoStatus :: [Picture]
    }

criaJogo ::
     [String]
  -> Float
  -> Float
  -> BitmapData
  -> [Int]
  -> BitmapData
  -> BitmapData
  -> BitmapData
  -> Jogo
criaJogo linhas tamanhox tamanhoy sprite listaRandoms spriteF spriteP spriteT =
  Jogo
    { mapaJogo = listaBlocos
    , pacmanJogo = criaPacman linhas listaBlocos tamanhox tamanhoy spriteP
    , fantasmas = geraFantasmas linhas tamanhox tamanhoy listaBlocos spriteF
    , pontos = 0
    , tempo = 0
    , listaRandoms = listaRandoms
    , pontuacaoMaxima = getPontuacaoMaxima listaBlocos
    , status = 0 --Normal
    , textoStatus = geraSpriteTexto spriteT
    }
  where
    listaBlocos = criaBlocos linhas tamanhox tamanhoy sprite

update :: Float -> Jogo -> Jogo
update seconds game
  | colidiu (pacmanJogo game) (fantasmas game) =
    Jogo
      { mapaJogo = mapaJogo game
      , pacmanJogo = pacmanJogo game
      , fantasmas = fantasmas game
      , pontos = pontos game
      , tempo = (tempo game) + 1
      , listaRandoms = atualizaListaRandoms (listaRandoms game)
      , pontuacaoMaxima = pontuacaoMaxima game
      , status = -1 -- Perdeu
      , textoStatus = textoStatus game
      }
  | pontos game == pontuacaoMaxima game =
    Jogo
      { mapaJogo = mapaJogo game
      , pacmanJogo = pacmanJogo game
      , fantasmas = fantasmas game
      , pontos = pontos game
      , tempo = (tempo game) + 1
      , listaRandoms = atualizaListaRandoms (listaRandoms game)
      , pontuacaoMaxima = pontuacaoMaxima game
      , status = 1 -- Ganhou
      , textoStatus = textoStatus game
      }
  | tempo game >= (tempoInicio * fps) =
    Jogo
      { mapaJogo = atualizaMapa
      , pacmanJogo = updatePacman (pacmanJogo game) (mapaJogo game)
      , fantasmas = atualizaFantasmas
      , pontos = somaPontos (tipo (bloco $ pacmanJogo game)) (pontos game)
      , tempo = (tempo game) + 1
      , listaRandoms = atualizaListaRandoms (listaRandoms game)
      , pontuacaoMaxima = pontuacaoMaxima game
      , status = 0
      , textoStatus = textoStatus game
      }
  | otherwise =
    Jogo
      { mapaJogo = mapaJogo game
      , pacmanJogo = pacmanJogo game
      , fantasmas = fantasmas game
      , pontos = pontos game
      , tempo = (tempo game) + 1
      , listaRandoms = atualizaListaRandoms (listaRandoms game)
      , pontuacaoMaxima = pontuacaoMaxima game
      , status = 0
      , textoStatus = textoStatus game
      }
  where
    atualizaMapa =
      updateBloco
        (mapaJogo game)
        (posPacman (pacmanJogo game))
        (velPacman (pacmanJogo game))
    atualizaFantasma =
      updateFantasma (listaRandoms game) (tempo game) (mapaJogo game)
    atualizaFantasmas = map atualizaFantasma (fantasmas game)

handleKeys :: Event -> Jogo -> Jogo
handleKeys evento game =
  Jogo
    { mapaJogo = mapaJogo game
    , pacmanJogo = teclasDeControle evento (pacmanJogo game)
    , fantasmas = fantasmas game
    , pontos = pontos game
    , tempo = tempo game
    , listaRandoms = listaRandoms game
    , pontuacaoMaxima = pontuacaoMaxima game
    , status = status game
    , textoStatus = textoStatus game
    }

desenhar :: Jogo -> Picture
desenhar game =
  Pictures
    [ desenhaBlocos (mapaJogo game)
    , teste
    , teste3
    , Pictures $
      map (desenhaFantasma (status game) (tempo game)) (fantasmas game)
    , desenhaPacman (status game) (tempo game) (pacmanJogo game)
    ]
  where
    teste =
      Translate
        (-37)
        (tamanhoY * 1.25)
        (Scale
           0.12
           0.12
           (Color yellow (Text ("Points: " ++ (show $ pontos game)))))
    teste3
      | (status game) == -1 =
        Translate 0 (tamanhoY * 1.12) ((textoStatus game) !! 3)
      | (status game) == 1 =
        Translate 0 (tamanhoY * 1.12) ((textoStatus game) !! 2)
      | (tempo game) <= (3 * fps) =
        Translate 0 (tamanhoY * 1.12) ((textoStatus game) !! 0)
      | otherwise = Translate 0 (tamanhoY * 1.12) ((textoStatus game) !! 1)
    (tamanhoX, tamanhoY) = posMax (mapaJogo game)

somaPontos :: String -> Int -> Int
somaPontos tipo pontos
  | tipo == "Ponto" = (pontos + 1)
  | otherwise = pontos

atualizaListaRandoms :: [Int] -> [Int]
atualizaListaRandoms listaRandoms = drop 4 listaRandoms

geraFantasmas ::
     [String] -> Float -> Float -> [[Bloco]] -> BitmapData -> [Fantasma]
geraFantasmas linhas tamanhox tamanhoy listaBlocos spriteF =
  [ criaFantasma 'B' linhas listaBlocos tamanhox tamanhoy spriteF
  , criaFantasma 'C' linhas listaBlocos tamanhox tamanhoy spriteF
  , criaFantasma 'R' linhas listaBlocos tamanhox tamanhoy spriteF
  , criaFantasma 'I' linhas listaBlocos tamanhox tamanhoy spriteF
  ]

getPontuacaoMaxima :: [[Bloco]] -> Int
getPontuacaoMaxima listaBlocos =
  sum [sum [1 | y <- x, tipo y == "Ponto"] | x <- listaBlocos]
