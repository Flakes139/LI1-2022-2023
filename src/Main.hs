module Main where

import LI12223
import Tarefa1_2022li1g112
import Tarefa2_2022li1g112
import Tarefa3_2022li1g112
import Tarefa4_2022li1g112
import Tarefa5_2022li1g112
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import System.Exit
import System.Directory
import System.Random

data Opcao = Jogar
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | VenceuJogo

type World = (Menu, Jogo, Images, Time, AoCalha)

type AoCalha = Int

type Time = Float

type Images = [Picture]

window :: Display
window = FullScreen --InWindow "Crossy Road" (700, 700) (0,0)

fr :: Int
fr = 1



estadoInicial :: Images -> AoCalha -> World
estadoInicial images seed = (Opcoes Jogar, Jogo (Jogador (0,-404)) mapajogo, images, 0,seed)


drawState :: World -> IO Picture
drawState (VenceuJogo, jogo, images, n,a) = return $ Translate (-200) 0 $ Color red $ scale 0.2 0.2 $ Text  ("Ganhou em "++show (round n) ++ " segundos")
drawState (Opcoes Jogar, jogo, images,n,a) = return $ Pictures $  Translate 0 0  ((!!) images 7) : [Color blue $ drawOption "Jogar", Translate 0 (-70) $ drawOption "Sair"]
drawState (Opcoes Sair, jogo, images,n,a) = return $ Pictures  $  Translate 0 0  ((!!) images 7) : [drawOption "Jogar", Color blue $ Translate 0 (-90) $ drawOption "Sair"]
drawState (ModoJogo, Jogo (Jogador(x,y)) mapajogo, images,n,a) = return $ Pictures $ funcao1 (ModoJogo, Jogo (Jogador(x,y)) mapajogo, images,n,a) (-882) (478) ++ funcao3 (ModoJogo, Jogo (Jogador(x, y)) mapajogo, images,n,a) (-900) (478) ++ [Translate i j  player] 
  where
      i = fromIntegral x
      j = fromIntegral y
      player= head images


drawOption option = Translate (-50) 0 $ Scale 0.5 0.5 $ Text option


time :: Float -> World ->  IO World 
time t (ModoJogo, j, i, n,a) = if jogoTerminou (animaJogo j (Parado)) == True then return $ (VenceuJogo, j, i, n,a) else return $ (ModoJogo, deslizaJogo (numeroaocalha a+1)(animaJogo j (Parado)), i, n,a)
time t (VenceuJogo, j, i, n,a) = return $ (VenceuJogo, j, i, n,a)
time t (o, j, i, n,a) = return $ (o, j, i, n,a)


event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, i,n,a) =
   return $ (ModoJogo, jogo, i, n,a)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, i, n,a) =
   return $ (Opcoes Sair, jogo, i, n,a)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, i, n,a) =
   return $ (Opcoes Sair, jogo, i, n,a)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, i, n,a) =
   return $ (Opcoes Jogar, jogo, i, n,a)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, i, n,a) =
    return $(Opcoes Jogar, jogo, i, n,a)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, i,n,a) =
   return $ error "Fecha Jogo"

-- Venceu Jogo
-- continuar a jogar depois de vencer
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo, jogo, i, n,a) =
   return $ estadoInicial i a
-- identificar que venceu Jogo
event _ (ModoJogo, Jogo (Jogador(x, y)) mapajogo, i,20,a) = return $ (VenceuJogo, Jogo (Jogador(x, y)) mapajogo, i, 20,a)
-- Modo Jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo , (Jogo (Jogador (x,y)) mapajogo), i, n,a) |jogoTerminou (Jogo (movimento (Jogador(x,y)) mapajogo (Move Cima)) mapajogo)  = return $ (VenceuJogo , (Jogo (Jogador(x,y)) mapajogo), i, n,a)
                                                                                               |otherwise = return $ (ModoJogo , Jogo (movimento (Jogador (x,y)) mapajogo (Move Cima)) mapajogo ,i ,n,a)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo , (Jogo (Jogador (x,y)) mapajogo), i, n,a) |jogoTerminou (Jogo (movimento (Jogador(x,y)) mapajogo (Move Baixo)) mapajogo) = return $ (VenceuJogo , (Jogo (Jogador(x,y)) mapajogo), i, n,a) 
                                                                                                 |otherwise = return $ (ModoJogo , Jogo (movimento (Jogador (x,y)) mapajogo (Move Baixo)) mapajogo ,i ,n,a)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo , (Jogo (Jogador (x,y)) mapajogo), i, n,a) |jogoTerminou (Jogo (movimento (Jogador(x,y)) mapajogo (Move Esquerda)) mapajogo)= return $ (VenceuJogo , (Jogo (Jogador(x,y)) mapajogo), i, n,a) 
                                                                                                 |otherwise = return $ (ModoJogo , Jogo (movimento (Jogador (x,y)) mapajogo (Move Esquerda)) mapajogo ,i ,n,a)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo , (Jogo (Jogador (x,y)) mapajogo), i, n,a) |jogoTerminou (Jogo (movimento (Jogador(x,y)) mapajogo (Move Direita)) mapajogo)= return $ (VenceuJogo , (Jogo (Jogador(x,y)) mapajogo), i, n,a) 
                                                                                                  |otherwise = return $ (ModoJogo , Jogo (movimento (Jogador (x,y)) mapajogo(Move Direita)) mapajogo ,i ,n,a)                                                                                             
-- não fazer nada em outros casos    
event _ w = return $ w



funcao1 :: World -> Float -> Float -> [Picture]
funcao1 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l []), images,n,c) a b = [circle 1] 
funcao1 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l ((Rio v,t):t1)), images,n,c) a b = Translate a b ((!!) images 2): 
                                                                                 Translate (a-100) b  ((!!) images 2):
                                                                                 Translate (a+100) b ((!!) images 2):
                                                                                 Translate (a+200) b  ((!!) images 2):
                                                                                 Translate (a+300) b  ((!!) images 2):
                                                                                 Translate (a+400) b  ((!!) images 2):
                                                                                 Translate (a+500) b  ((!!) images 2):
                                                                                 Translate (a+600) b  ((!!) images 2):
                                                                                 Translate (a+700) b  ((!!) images 2):
                                                                                 Translate (a+800) b  ((!!) images 2):
                                                                                 Translate (a+900) b  ((!!) images 2):
                                                                                 Translate (a+1000) b ((!!) images 2):
                                                                                 Translate (a+1100) b ((!!) images 2):
                                                                                 Translate (a+1200) b ((!!) images 2):
                                                                                 Translate (a+1300) b ((!!) images 2):
                                                                                 Translate (a+1400) b ((!!) images 2):
                                                                                 Translate (a+1500) b ((!!) images 2):
                                                                                 Translate (a+1600) b ((!!) images 2):
                                                                                 Translate (a+1700) b ((!!) images 2):
                                                                                 Translate (a+1800) b ((!!) images 2):
                                                                                 Translate (a+1900) b ((!!) images 2):
                                                                                 Translate (a+2000) b ((!!) images 2):
                                                                                 Translate (a+2100) b ((!!) images 2):
                                                                                 Translate (a+2200) b ((!!) images 2):
                                                funcao1 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l t1), images,n,c) a (b- 98)  
funcao1 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l ((Estrada v,t):t1)), images,n,c) a b = Translate a b ((!!) images 3):
                                                                                 Translate (a+140) b  ((!!) images 3):
                                                                                 Translate (a+280) b  ((!!) images 3):
                                                                                 Translate (a+420) b  ((!!) images 3):
                                                                                 Translate (a+560) b  ((!!) images 3):
                                                                                 Translate (a+700) b  ((!!) images 3):
                                                                                 Translate (a+840) b  ((!!) images 3):
                                                                                 Translate (a+980) b  ((!!) images 3):
                                                                                 Translate (a+1120) b ((!!) images 3):
                                                                                 Translate (a+1260) b ((!!) images 3):
                                                                                 Translate (a+1400) b ((!!) images 3):
                                                                                 Translate (a+1540) b ((!!) images 3):
                                                                                 Translate (a+1680) b ((!!) images 3):
                                                                                 Translate (a+1820) b ((!!) images 3):
                                                                                 Translate (a+1960) b ((!!) images 3):
                                                funcao1 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l t1), images,n,c) a (b - 98) 
funcao1 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l ((Relva,t):t1)), images,n,c) a b = Translate a b ((!!) images 1):
                                                                                 Translate (a+172) b  ((!!) images 1):
                                                                                 Translate (a+344) b  ((!!) images 1):
                                                                                 Translate (a+516) b  ((!!) images 1):
                                                                                 Translate (a+688) b  ((!!) images 1):
                                                                                 Translate (a+860) b  ((!!) images 1):
                                                                                 Translate (a+1032) b ((!!) images 1):
                                                                                 Translate (a+1204) b ((!!) images 1):
                                                                                 Translate (a+1376) b ((!!) images 1):
                                                                                 Translate (a+1548) b ((!!) images 1):
                                                                                 Translate (a+1720) b ((!!) images 1):
                                                                                 Translate (a+1892) b ((!!) images 1):
                                                                                 Translate (a+2064) b ((!!) images 1):
                                                                                 Translate (a+2236) b ((!!) images 1):
                                                                                 Translate (a+2408) b ((!!) images 1):
                                                                                 funcao1 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l t1), images,n,c) a (b - 98)

funcao2 :: World -> Float -> Float -> [Picture]
funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l ((terreno , []):t1)), images,n,c) a b = [circle 1] 
funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l ((Relva,t:tail1):t1)), images,n,c) a b |t == Arvore = Translate (a+20) b  ((!!) images 4):funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l((Relva,tail1):t1)), images,n,c) (a+98) b 
                                                                                       |otherwise = funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l((Relva,tail1):t1)), images,n,c) (a+98) b 

funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l ((Rio v,t:tail1):t1)), images,n,c) a b | t == Tronco = Translate a   b    ((!!) images 5):funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l((Rio v,tail1):t1)), images,n,c) (a+98) b 
                                                                                       |otherwise = funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l((Rio v,tail1):t1)), images,n,c) (a+98) b
                
funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l ((Estrada v,t:tail1):t1)), images,n,c) a b | t == Carro = Translate (a+20) b ((!!) images 6):funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l((Estrada v,tail1):t1)), images,n,c) (a+98) b 
                                                                                       |otherwise = funcao2 (ModoJogo, Jogo (Jogador(x, y)) (Mapa l((Estrada v,tail1):t1)), images,n,c) (a+98) b


funcao3 :: World -> Float -> Float -> [Picture]
funcao3 (ModoJogo, Jogo (Jogador (x,y)) (Mapa l []), images,n,c) a b = [circle 1]
funcao3 (ModoJogo, Jogo (Jogador (x,y)) (Mapa l (t:t1)), images,n,c) a b = funcao2 (ModoJogo, Jogo (Jogador (x,y)) (Mapa l (t:t1)), images,n,c) a b ++ funcao3 (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t1), images,n,c) a (b-98)





mapajogo :: Mapa 
mapajogo = Mapa 10[(Estrada 1,[Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),
                    (Relva ,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),
                    (Rio 1 ,[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),
                    (Estrada 1 ,[Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro]),
                    (Relva ,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),
                    (Relva ,[Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore]),
                    (Relva ,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),
                    (Relva ,[Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum]),
                    (Relva ,[Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum]),
                    (Relva ,[Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum]),
                    (Relva ,[Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum])]

{-listaaocalha :: Int -> Int -> [Int]
listaaocalha n seed = take n (randomRs (0,9) (mkStdGen seed))

numeroaocalha :: Int -> Int
numeroaocalha seed = head $ listaaocalha 1 seed-}



main :: IO ()
main = do
 player <- loadBMP "gafanhoto.bmp"
 deserto <- loadBMP "areia.bmp"
 agua <- loadBMP "agua.bmp"
 estrada <- loadBMP "estrada.bmp"
 seed <- randomRIO  (0,100)
 cacto <- loadBMP "cacto.bmp"
 tronco <- loadBMP "tronco.bmp"
 elefante <- loadBMP "elefante.bmp"
 fundo <- loadBMP "fundo.bmp"
 let images = [scale 0.15 0.15 player, scale 0.09 0.09 deserto, scale 0.05 0.05 agua, scale 0.23 0.235 estrada, scale 0.1 0.1 cacto, scale 0.05 0.05 tronco, scale 0.5 0.5 elefante, scale 0.5 0.5 fundo]
 playIO window (greyN 0.5) fr (estadoInicial images seed) drawState event time