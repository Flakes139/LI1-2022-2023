{- |
Module      : Tarefa4_2022li1g112
Description : Determinar se o jogo terminou
Copyright   : Vasco João Timóteo Gonçalves <a104527@alunos.uminho.pt>
              Orlando Daniel Venda da Costa <a104255@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g112 where

import LI12223

jogoTerminou :: Jogo -> Bool
jogoTerminou jg@(Jogo (Jogador (a,b)) (Mapa w l)) = jogFora (a,b) (Mapa w l) || atropelado (a,b) (Mapa w l) || afogado (a,b) (Mapa w l)



jogFora :: Coordenadas -> Mapa -> Bool
jogFora (a,b) (Mapa w l) =  a < -882 || a > 882 || b < -502 || b > 478


atropelado :: Coordenadas -> Mapa -> Bool
atropelado (x,y) (Mapa l []) = False
atropelado (x,y) (Mapa l ((Estrada v,o):t))
  | elem (x,y) (posCarro (Mapa l ((Estrada v,o):t)) (-882) (478)) = True
  | otherwise = False
atropelado (x,y) (Mapa l ((Rio v,o):t)) = atropelado (x,y+98) (Mapa l t)
atropelado (x,y) (Mapa l ((Relva,o):t)) = atropelado (x,y+98) (Mapa l t)

posCarro :: Mapa -> Int -> Int -> [Coordenadas]
posCarro (Mapa l []) x y = []
posCarro (Mapa l ((Estrada v,o):t)) x y  = posCarroAux (Estrada v,o) x y ++ posCarro (Mapa l t) x (y-98)
posCarro (Mapa l ((Rio v,o):t)) x y = posCarro (Mapa l t) x (y-98)
posCarro (Mapa l ((Relva,o):t)) x y = posCarro (Mapa l t) x (y-98)

posCarroAux :: (Terreno,[Obstaculo]) -> Int -> Int -> [Coordenadas]
posCarroAux (Estrada v,[]) x y = []
posCarroAux (Estrada v,(h:t1)) x y
  |h == Carro = (x,y) : posCarroAux (Estrada v,t1) (x+98) y  
  |otherwise = posCarroAux (Estrada v,t1) (x+98) y


afogado :: Coordenadas -> Mapa -> Bool
afogado (x,y) (Mapa l []) = False
afogado (x,y) (Mapa l ((Rio v,o):t)) 
  | elem (x,y) (posRio (Mapa l ((Rio v,o):t)) (-882) (478)) = True 
  | otherwise = False
afogado (x,y) (Mapa l ((Estrada v,o):t)) = afogado (x,y+98) (Mapa l t)
afogado (x,y) (Mapa l ((Relva,o):t)) = afogado (x,y+98) (Mapa l t)


posRio :: Mapa -> Int -> Int -> [Coordenadas]
posRio (Mapa l []) x y = []
posRio (Mapa l ((Rio v,o):t)) x y  = posRioAux (Rio v,o) x y ++ posRio (Mapa l t) x (y-98)
posRio (Mapa l ((Estrada v,o):t)) x y = posRio (Mapa l t) x (y-98)
posRio (Mapa l ((Relva,o):t)) x y = posRio (Mapa l t) x (y-98)

posRioAux :: (Terreno,[Obstaculo]) -> Int -> Int -> [Coordenadas]
posRioAux (Rio v,[]) x y = []
posRioAux (Rio v,(h:t1)) x y
  |h == Nenhum = (x,y) : posRioAux (Rio v,t1) (x+98) y  
  |otherwise = posRioAux (Rio v,t1) (x+98) y 



{-| A função indica se o jogador perdeu o jogo,
sendo que "True" significa que perdeu,enquanto que "False"
indica que não perdeu. Para tal, utiliza funções auxiliares,
que testam se o jogador está fora do mapa, debaixo de água
ou na mesma posição que um carro -}
