module Tarefa3_2022li1g112_Spec where

import LI12223
import Tarefa3_2022li1g112
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1 - Teste com velocidade 2 nos terrenos e o jogador consegue mover-se" ~: Jogo (Jogador (2,0)) (Mapa 5 [(Rio 2,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Nenhum,Nenhum,Arvore])]) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Rio 2,[Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Nenhum,Nenhum,Arvore])])) (Move Baixo),]
                                              "Teste 2 - Teste com o jogador no limite superior do inferior" ~: Jogo (Jogador (1,0)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Estrada 0, [Nenhum,Carro,Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Estrada 0, [Nenhum,Carro,Carro,Nenhum,Nenhum])])) (Move Baixo),
                                              "Teste 3 - Teste com jogador em cima de um tronco, num rio com velocidade não nula" ~: Jogo (Jogador (0,3)) (Mapa 5 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,2)) (Mapa 5 [(Rio 1,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum])])) (Parado)]
