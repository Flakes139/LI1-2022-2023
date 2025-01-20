module Tarefa1_2022li1g112_Spec where

import LI12223
import Tarefa1_2022li1g112
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1 - Invalido com obstaculos em terrenos impróprios" ~:False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada -1, [Nenhum, Tronco, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
                                             ,"Teste 2 - Valido com 2 rios contínguos e direções opostas" ~:True ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Rio -1, [Nenhum, Tronco, Nenhum, Tronco, Tronco]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])
                                             ,"Teste 3 - Invalido com carro com carro com mais que 3 unidades de comprimento" ~:False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada -1, [Nenhum, Carro, Carro, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
                                             ,"Teste 4 - Valido com menos de 5 troncos seguidos" ~:False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Rio -2, [Nenhum, Tronco, Tronco, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
                                             ,"Teste 5 - Valido com pelo menos um nenhum em cada linha" ~:True ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Rio -1, [Nenhum, Tronco, Nenhum, Tronco, Nenhum]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
                                             ,"Teste 6 - Invalido com largura diferente do comprimento" ~:False ~=? mapaValido (Mapa 6 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada -1, [Nenhum, Carro, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])]


