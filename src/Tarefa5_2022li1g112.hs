{- |
Module      : Tarefa3_2022li1g112
Description : Movimentação do personagem e obstáculos
Copyright   : Vasco João Timóteo Gonçalves <a104527@alunos.uminho.pt>
              Orlando Daniel Venda da Costa <a104255@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g112 where

import LI12223
import Tarefa2_2022li1g112
import System.Random

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) (Mapa l t)) = (Jogo (Jogador (x,y-98)) (estendeMapa (Mapa l (init t)) seed) )

