# Haskell-Pacman
![Shield showing the year of creation (2019)](https://img.shields.io/badge/Year-2019-red)
![Shield showing the language of project(Haskell)](https://img.shields.io/badge/Language-Haskell-blueviolet)
![Shield showing the library used in project(Floss)](https://img.shields.io/badge/Library-Gloss-orange)
![Shield showing the course of project(Programming Paradigm)](https://img.shields.io/badge/Course-Programming%20Paradigm-blue)


# What is this project?
This project is a simplified Pacman made in Haskell, it does not have fruits or power up and the ghosts doesn't have the original AI. The game has a map maker to creater you own map, specifying pacman spawn point, ghost spawn point, points and walls, also is possible create warps and others Pacman structures.

## How to use
Firstly you'll need install [Stack](https://docs.haskellstack.org/en/stable/), a cross-platform package and project manager for Haskell, and clone this repo.

After that, you will be able to setup, build and execute the project, just go to the main folder and use the follow lines in terminal: 

```bash
  $ stack setup
  $ stack build
  $ stack exec projeto
```

## Windows

If you use Windows, maybe be need to add glut32.dll in the built project, it happens because of the [gloss library](http://gloss.ouroborus.net/). So, download the dll and put it in the folder .stack-work\install\e910781a\bin (or similar) after build and run the project normaly with `stack exec projeto`.

## Snapshots

![Image of starting stage of pacman](https://i.imgur.com/OLT27i6.png).
![Getting points in the go stage](https://i.imgur.com/pMdN6p6.png).
![Lose screen with frozen ghosts and pacman](https://i.imgur.com/QCCTCLc.png).

## Video

Link: [https://www.youtube.com/watch?v=I0CFMF5AEZU](https://www.youtube.com/watch?v=I0CFMF5AEZU)
