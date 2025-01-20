{- |
Module      : Tarefa2_2022li1g112
Description : Geração contínua de um mapa
Copyright   : Vasco João Timóteo Gonçalves <a104527@alunos.uminho.pt>
              Orlando Daniel Venda da Costa <a104255@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g112 where

import LI12223
import System.Random

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l t)  i = Mapa l ((proxTerreno (Mapa l t) i , proxObstaculo (proxTerreno (Mapa l t) i) [] l i  ) : t)
proxTerreno :: Mapa -> Int -> Terreno
proxTerreno (Mapa l t) i 
  | p == Rio 0 = Rio (mod i (div l 2) + 2)
  | p == Estrada 0 = Estrada (mod i (div l 1) + 1)
  | p == Relva = Relva 
  where p = proximosTerrenosValidos (Mapa l t) !! mod (i + length t) (length (proximosTerrenosValidos (Mapa l t)))

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa l ((Rio v,x): (Rio v1,x1) : (Rio v2,x2) : (Rio v3,x3) : t))= [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l ((Rio v,x):t)) = [Rio 0, Estrada 0,Relva]
proximosTerrenosValidos (Mapa l ((Estrada v,x):(Estrada v1,x1):(Estrada v2,x2):(Estrada v3,x3):(Estrada v4,x4):t)) = [Rio 0,Relva] 
proximosTerrenosValidos (Mapa l ((Estrada v,x):t)) = [Rio 0, Estrada 0,Relva] 
proximosTerrenosValidos (Mapa l ((Relva,x):(Relva,x1):(Relva,x2):(Relva,x3):(Relva,x4):t)) = [Rio 0, Estrada 0] 
proximosTerrenosValidos (Mapa l ((Relva,x):t)) = [Rio 0,Estrada 0,Relva]
proxObstaculo :: Terreno -> [Obstaculo] -> Int -> Int -> [Obstaculo]
proxObstaculo Relva o l i 
  | null o = proxObstaculo Relva [[Nenhum,Arvore] !! mod i 2] l i
  | length o == l = o
  | otherwise =  proxObstaculo Relva p l i
  where p = (proximosObstaculosValidos l (Relva, o) !! mod (i + length o) (length (proximosObstaculosValidos l (Relva, o)))) : o
proxObstaculo (Estrada v) o l i 
  | null o = proxObstaculo (Estrada v) [[Nenhum,Carro] !! mod i 2] l i
  | length o == l = o
  | otherwise =  proxObstaculo (Estrada v) p l i
  where p = proximosObstaculosValidos l (Estrada v, o) !! mod (i + length o) (length (proximosObstaculosValidos l (Estrada v, o))) : o
proxObstaculo (Rio v) o l i 
  | null o = proxObstaculo (Rio v) [[Nenhum,Tronco] !! mod i 2] l i
  | length o == l = o
  | otherwise =  proxObstaculo (Rio v) p l i
  where p = proximosObstaculosValidos l (Rio v, o) !! mod (i + length o) (length (proximosObstaculosValidos l (Rio v, o))) : o

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (p,x) 
  |length x == n = []
  |length x == (n-1) = proximosObstaculosValidosAux2 n (p,x)
  |otherwise = proximosObstaculosValidosAux n (p,x)

proximosObstaculosValidosAux :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidosAux n (Rio v, []) = [Nenhum, Tronco]
proximosObstaculosValidosAux n (Estrada v, []) = [Nenhum, Carro]
proximosObstaculosValidosAux n (Relva, []) = [Nenhum, Arvore]
proximosObstaculosValidosAux n (Rio v,Tronco:Tronco:Tronco:Tronco:Tronco:t) = [Nenhum]
proximosObstaculosValidosAux n (Rio v,x)= [Nenhum,Tronco]
proximosObstaculosValidosAux n (Estrada v,Carro:Carro:Carro:t) = [Nenhum]
proximosObstaculosValidosAux n (Estrada v,x)= [Nenhum,Carro]
proximosObstaculosValidosAux n (Relva,x) = [Nenhum,Arvore]
proximosObstaculosValidosAux2 :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidosAux2  n (Rio v,x)
   | notElem Nenhum x = [Nenhum]
   | length x > 5 && head x == Tronco && last x == Tronco && (contaTroncosIniciais x + contaTroncosFinais x) > 4 = [Nenhum]
   | otherwise = proximosObstaculosValidosAux n (Rio v,x)
proximosObstaculosValidosAux2  n (Estrada v,x)
   | notElem Nenhum x  = [Nenhum]
   | length x > 3 && head x == Carro && last x == Carro && (contaCarrosIniciais x + contaCarrosFinais x) > 2 = [Nenhum]
   | otherwise = proximosObstaculosValidosAux n (Estrada v,x)
proximosObstaculosValidosAux2  n (Relva,x)
   | notElem Nenhum x  = [Nenhum]
   | otherwise = proximosObstaculosValidosAux n (Relva,x)

contaTroncosIniciais :: [Obstaculo] -> Int 
contaTroncosIniciais (h:t) 
  | h == Tronco = 1 + contaTroncosIniciais t
  | otherwise = 0

contaTroncosFinais :: [Obstaculo] -> Int 
contaTroncosFinais t 
  | last t == Tronco = 1 + contaTroncosFinais (init t)
  | otherwise = 0

contaCarrosIniciais :: [Obstaculo] -> Int 
contaCarrosIniciais (h:t) 
  | h == Carro = 1 + contaCarrosIniciais t
  | otherwise = 0

contaCarrosFinais :: [Obstaculo] -> Int 
contaCarrosFinais t 
  | last t == Carro = 1 + contaCarrosFinais (init t)
  | otherwise = 0

listaaocalha :: Int -> Int -> [Int]
listaaocalha n seed = take n (randomRs (0,9) (mkStdGen seed))

numeroaocalha :: Int -> Int
numeroaocalha seed = head $ listaaocalha 1 seed