module Tarefa4_2022li1g112_Spec where

import LI12223
import Tarefa4_2022li1g112
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [
                                             "Teste 1" ~: True ~=? jogoTerminou jogo1                 
                                             "Teste 2" ~: True ~=? jogoTerminou jogo2
                                             "Teste 3" ~: False ~=? jogoTerminou jogo3
                                             
jogo1 = (Jogo (Jogador (0,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Tronco,Nenhum,Nenhum]),(Estrada(-1),[Carro,Nenhum,Nenhum])]))
jogo2 = (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Tronco,Nenhum,Nenhum]),(Estrada(-1),[Carro,Nenhum,Nenhum])]))
jogo3 = (Jogo (Jogador (1,0)) (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio 1,[Tronco,Nenhum,Nenhum]),(Estrada(-1),[Carro,Nenhum,Nenhum])]))
