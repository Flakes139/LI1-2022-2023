{- |
Module      : Tarefa3_2022li1g112
Description : Movimentação do personagem e obstáculos
Copyright   : Vasco João Timóteo Gonçalves <a104527@alunos.uminho.pt>
              Orlando Daniel Venda da Costa <a104255@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g112 where

import LI12223
{-|A funçao animaJogo é a união de todas as funções criadas para resolução desta tarefa -}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo x y) p = (Jogo (movimento x y p)(obstaculo y))
{-|A função obstaculo tem o objetivo de meter os obstaculos a moverem se em certa velocidade para uma direção determinada.
@
obstaculo :: Mapa -> Mapa 
obstaculo (Mapa l t) = (Mapa l (aux t))

aux :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]478
aux [] = []
aux ((Relva,h):t)= (Relva,h): aux t
aux ((Estrada v,h):t) |v>= 0 = (Estrada v,((drop ((length h) - v) h) ++ (take ((length h) - v) h))) : aux t
                      |otherwise = (Estrada v, ((drop (-v) h) ++ (take (-v) h))) : aux t
aux ((Rio v,h):t) |v>= 0 = (Rio v,((drop ((length h) - v) h) ++ (take ((length h) - v) h))) : aux t
                  |otherwise = (Rio v, ((drop (-v) h) ++ (take (-v) h))) : aux t
@
A auxiliar aux foi criada com o intuito de ver como reage o mapa quando a velocidade é negativa ou positiva, ou seja , quando se move para diferentes direções.
A auxiliar mostra que a estrada vais se movendo, assim como o rio.
Porém quando o terreno é Relva, esta fila não se mexe.-}
obstaculo :: Mapa -> Mapa 
obstaculo (Mapa l t) = (Mapa l (aux t))

aux :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
aux [] = []
aux ((Relva,h):t)= (Relva,h): aux t
aux ((Estrada v,h):t) |v>= 0 = (Estrada v,((drop ((length h) - v) h) ++ (take ((length h) - v) h))) : aux t
                      |otherwise = (Estrada v, ((drop (-v) h) ++ (take (-v) h))) : aux t
aux ((Rio v,h):t) |v>= 0 = (Rio v,((drop ((length h) - v) h) ++ (take ((length h) - v) h))) : aux t
                  |otherwise = (Rio v, ((drop (-v) h) ++ (take (-v) h))) : aux t

{-|Esta grande função movimento que tem várias auxiliares que determina o movimento do jogador quando tem instruçoes de movimento.
@
movimento :: Jogador -> Mapa -> Jogada -> Jogador
movimento (Jogador (x,y)) (Mapa l t) (Parado) |elem (x,y) (troncos( Mapa l t)0 0) = Jogador (proxCoorTronco (x,y) (Mapa l t))
                                              |otherwise = Jogador (x,y)

movimento (Jogador (x,y)) (Mapa l t) (Move Cima) |elem (x-1,y) (arvores (Mapa l t)0 0) = Jogador (x,y)
                                                 |x == 0 = Jogador (x,y)
                                                 |otherwise = Jogador (x-1,y)

movimento (Jogador (x,y)) (Mapa l t) (Move Baixo) |elem (x+1,y) (arvores (Mapa l t)0 0) = Jogador (x,y) 
                                                  |x == l-1 = Jogador (x,y)
                                                  |otherwise = Jogador (x+1,y)

movimento (Jogador (x,y)) (Mapa l t) (Move Esquerda) |elem (x,y-1) (arvores (Mapa l t)0 0) = Jogador (x,y)
                                                     |y == 0 = Jogador (x,y)
                                                     |otherwise = Jogador (x,y-1)

movimento (Jogador (x,y)) (Mapa l t) (Move Direita) |elem (x,y+1) (arvores (Mapa l t)0 0) = Jogador (x,y)
                                                    |y == l-1 = Jogador (x,y)
                                                    |otherwise = Jogador (x+1,y) 
@
Inicialmente, quando a instrução é para o jogador não se mecher, este mantém se na mesma posição menos quando está em cima de um tronco, que acompanha o movimento do mesmo.
Quando o Jogador tens intruções de ir para cima, baixo ou para os lados e quando vai contra uma árvore este não se mexe.
Quando o jogador vai para cima as cordenadas diminuem um y, quando se meche para baixo, as coordenadas aumentas um y.
Quando o jogador se mexe lateralmente para a esquerda o x dimunui e quando se mexe para a direita o x aumenta.
Nesta função, também não se mexe quando tenta ir em direção de algum dos limites. -}

movimento :: Jogador -> Mapa -> Jogada -> Jogador
movimento (Jogador (x,y)) (Mapa l t) (Parado) |elem (x,y) (troncos( Mapa l t)(-882) 478) = Jogador (proxCoor (x,y) (Mapa l t))
                                              |otherwise = Jogador (x,y)

movimento (Jogador (x,y)) (Mapa l t) (Move Cima) |elem (x,y+98) (arvores (Mapa l t)(-882) 478) = Jogador (x,y)
                                                 |y == 478 = Jogador (x,y)
                                                 |otherwise = Jogador (x,y+98)

movimento (Jogador (x,y)) (Mapa l t) (Move Baixo) |elem (x,y-98) (arvores (Mapa l t)(-882) 478) = Jogador (x,y) 
                                                  |y == -502 = Jogador (x,y)
                                                  |otherwise = Jogador (x,y-98)

movimento (Jogador (x,y)) (Mapa l t) (Move Esquerda) |elem (x-98,y) (arvores (Mapa l t)(-882) 478) = Jogador (x,y)
                                                     |x == -882 = Jogador (x,y)
                                                     |otherwise = Jogador (x-98,y)

movimento (Jogador (x,y)) (Mapa l t) (Move Direita) |elem (x+98,y) (arvores (Mapa l t)(-882) 478) = Jogador (x,y)
                                                    |x == 882 = Jogador (x,y)
                                                    |otherwise = Jogador (x+98,y)   

{-| As funções arvores servem para ver as coordenadas onde podem estar arvores para em cima ver que quando alguem se tenta mover para lá não e mexe.
@
arvores :: Mapa -> Int -> Int -> [Coordenadas]
arvores (Mapa l []) _ _ = []
arvores (Mapa l ((Relva ,h):t)) x y = auxArvores (Relva,h) x y ++ arvores (Mapa l t) (x+1) y
arvores (Mapa l ((Estrada v,h):t)) x y = arvores (Mapa l t) (x+1) y
arvores (Mapa l ((Rio v,h):t)) x y = arvores (Mapa l t) (x+1) y  


auxArvores :: (Terreno,[Obstaculo])-> Int -> Int -> [Coordenadas]
auxArvores (Relva, []) x y = []
auxArvores (Relva, (h:t)) x y |h == Arvore = (x,y) : auxArvores (Relva,t) x (y+1)
                              |otherwise = auxArvores (Relva,t) x (y+1)
@
As funçoes dão a recursiva quando o terreno não é relva e quando é relva dão as coordenadas das arvores ou arvore que existe em cada linha, aumentando sempre x+1 para ver a linha seguinte, dado que esta já tinha sido analisada na auxiliar.-}
arvores :: Mapa -> Int -> Int -> [Coordenadas]
arvores (Mapa l []) _ _ = []
arvores (Mapa l ((Relva ,h):t)) x y = auxArvores (Relva,h) x y ++ arvores (Mapa l t) x (y-98)
arvores (Mapa l ((Estrada v,h):t)) x y = arvores (Mapa l t) x (y-98)
arvores (Mapa l ((Rio v,h):t)) x y = arvores (Mapa l t) x (y-98)  


auxArvores :: (Terreno,[Obstaculo])-> Int -> Int -> [Coordenadas]
auxArvores (Relva, []) x y = []
auxArvores (Relva, (h:t)) x y |h == Arvore = (x,y) : auxArvores (Relva,t) (x+98) y
                              |otherwise = auxArvores (Relva,t) (x+98) y                                                                                       



{-|
As primeiras 2 funções troncos são exatamente iguais ás das arvores, porém com troncos.
@
troncos :: Mapa -> Int -> Int -> [Coordenadas]
troncos (Mapa l []) _ _ = []
troncos (Mapa l ((Rio v ,h):t)) x y = auxTroncos (Rio v,h) x y ++ troncos (Mapa l t) (x+1) y
troncos (Mapa l ((Estrada v,h):t)) x y = troncos (Mapa l t) (x+1) y
troncos (Mapa l ((Relva ,h):t)) x y = troncos (Mapa l t) (x+1) y 




auxTroncos :: (Terreno,[Obstaculo])-> Int -> Int -> [Coordenadas]
auxTroncos (Rio v, []) x y = []
auxTroncos (Rio v, (h:t)) x y |h == Tronco = (x,y) : auxTroncos (Rio v,t) x (y+1)
                              |otherwise = auxTroncos (Rio v,t) x (y+1) 
@ -}
troncos :: Mapa -> Int -> Int -> [Coordenadas]
troncos (Mapa l []) _ _ = []
troncos (Mapa l ((Rio v ,h):t)) x y = auxTroncos (Rio v,h) x y ++ troncos (Mapa l t) x (y-98)
troncos (Mapa l ((Estrada v,h):t)) x y = troncos (Mapa l t) x (y-98)
troncos (Mapa l ((Relva ,h):t)) x y = troncos (Mapa l t) x (y-98) 




auxTroncos :: (Terreno,[Obstaculo])-> Int -> Int -> [Coordenadas]
auxTroncos (Rio v, []) x y = []
auxTroncos (Rio v, (h:t)) x y |h == Tronco = (x,y) : auxTroncos (Rio v,t) (x+98) y
                              |otherwise = auxTroncos (Rio v,t) (x+98) y 

{-| Estas ultimas funçoes tem o objetivo de ver as coordenadas dos troncos quando estes se encontram em movimento 
@
proxCoor :: Coordenadas -> Mapa -> Coordenadas
proxCoor (x,y) (Mapa l t) = (x,proxCoorAux y (Mapa l (drop x t)))

proxCoorAux :: Int -> Mapa -> Int 
proxCoorAux y (Mapa l ((Rio v,o):t)) = (v+y)

proxCoorTronco :: Coordenadas -> Mapa -> Coordenadas
proxCoorTronco (x,y) (Mapa l t) |elem (x,y) (troncos (Mapa l t) 0 0) = proxCoor (x,y) (Mapa l t) 
                                |otherwise = (x,y) 
@
A funçao proxCoor pega na coordenada x e depois na auxiliar proxCoorAux que vê a posição da fila em que estará o tronco dependendo da velocidade.
A proxCoor acaba por pegar no mapa e retirar os x que não é agua para ter a coordenada certa. 
A funçap proxCoorTronco é a recursiva que vê sempre o movimento dos troncos a partir das coordenadas dos troncos já encontradas nas auxiliares baseando se na posição inicial 0 0.-}
proxCoor :: Coordenadas -> Mapa -> Coordenadas
proxCoor (x,y) (Mapa l t) = (proxCoorAux x (posCoorAux1 (x,y) (Mapa l t)),y)

proxCoorAux :: Int -> Mapa -> Int 
proxCoorAux x (Mapa l ((Rio v,o):t)) = ((v*98)+x)


posCoorAux1 :: Coordenadas -> Mapa -> Mapa
posCoorAux1 (x,y) (Mapa l ((t,o):t1)) 
  | y == (478) = (Mapa l ((t,o):t1))
  | otherwise = posCoorAux1 (x,y+98) (Mapa l t1)

