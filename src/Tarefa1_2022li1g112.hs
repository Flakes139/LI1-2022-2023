{- |
Module      : Tarefa1_2022li1g112
Description : Validação de um mapa
Copyright   : Vasco João Timóteo Gonçalves <a104527@alunos.uminho.pt>
              Orlando Daniel Venda da Costa <a104255@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g112 where

import LI12223

{-|A funçao mapa válido é a união de todas as funções auxiliares criadas para resolução desta tarefa -}
mapaValido :: Mapa -> Bool
mapaValido (Mapa l ((tr, (x:y)):t)) = mapaValido1 (Mapa l ((tr, (x:y)):t)) && mapaValido2 (Mapa l ((tr, (x:y)):t)) && mapaValido3 (Mapa l ((tr, (x:y)):t))&& mapaValido4 (Mapa l ((tr, (x:y)):t))&& mapaValido5 (Mapa l ((tr, (x:y)):t))&& mapaValido6 (Mapa l ((tr, (x:y)):t))&& mapaValido7 (Mapa l ((tr, (x:y)):t))
{-|A função mapavalido1 é criada com o intuito de garantir que não existem obstáculos em terrenos impróprios, começando por apresentar o caso de paragem.
@
mapaValido1 :: Mapa -> Bool
mapaValido1 (Mapa l []) = True 
mapaValido1 (Mapa l ((Rio v,(h:hs)):t)) |(elem Carro (h:hs) || elem Arvore (h:hs)) = False
                                       |otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Relva, (h:hs)):t)) |(elem Carro (h:hs) || elem Tronco (h:hs)) = False
                                        |otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Estrada v, (h:hs)):t)) |(elem Arvore (h:hs) || elem Tronco (h:hs)) = False
                                            |otherwise = mapaValido1 (Mapa l t)
@
De seguida são apresentadas as hipóteses para os diferentes terrenos, no rio não podem aparecer nem carros nem árvores, na relva não podem aparecer nem carros nem troncos e na estrada não podem aparecer troncos nem árvores.
Caso contrário a função é valida e ajuda a formar a função inicial.-}
mapaValido1 :: Mapa -> Bool
mapaValido1 (Mapa l []) = True 
mapaValido1 (Mapa l ((Rio v,(h:hs)):t)) |(elem Carro (h:hs) || elem Arvore (h:hs)) = False
                                       |otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Relva, (h:hs)):t)) |(elem Carro (h:hs) || elem Tronco (h:hs)) = False
                                        |otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Estrada v, (h:hs)):t)) |(elem Arvore (h:hs) || elem Tronco (h:hs)) = False
                                            |otherwise = mapaValido1 (Mapa l t)
{-|A função mapavalido2 é utilizada com o objetivo de garantir que 2 rios contínguos têm direções opostas.
@
mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa l []) = True
mapaValido2 (Mapa l ((Relva, (h:hs)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Estrada v, (h:hs)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v,(h:hs)): (Estrada v1,(h1:hs1)):t)) = mapaValido2 (Mapa l ((Estrada v1,(h1:hs1)):t))
mapaValido2 (Mapa l ((Rio v,(h:hs)): (Relva ,(h1:hs1)):t)) = mapaValido2 (Mapa l ((Relva ,(h1:hs1)):t))
mapaValido2 (Mapa l ((Rio v,(h:hs)):[])) = True
mapaValido2 (Mapa l ((Rio v,(h:hs)): (Rio v1,(h1:hs1)):t)) | v>0 && v1>0 = False
                                                           | v<0 && v1<0 = False
                                                           | otherwise = mapaValido2 (Mapa l ((Rio v1,(h1:hs1)):t))
@
A funçao começa por apresentar o caso de paragem, de seguida os casos onde começa com o terreno estrada e relva, que dão a função recursida desta.
Depois temos os casos onde começa em rio mas depois temos estrada ou relva que tal como os casos de cima devolve a recursiva.
Por último a funçao apresenta os casos em que aparecem rios seguidos, obrigando a que se a velocidade de um for positiva, a do outro tem de ser negativa e vice-versa, ou seja, os rios seguidos tem direções opostas.
Devolvendo a recursiva até ao caso de paragem, ajudando assim a formar a funçao inicial.  -}
mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa l []) = True
mapaValido2 (Mapa l ((Relva, (h:hs)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Estrada v, (h:hs)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v,(h:hs)): (Estrada v1,(h1:hs1)):t)) = mapaValido2 (Mapa l ((Estrada v1,(h1:hs1)):t))
mapaValido2 (Mapa l ((Rio v,(h:hs)): (Relva ,(h1:hs1)):t)) = mapaValido2 (Mapa l ((Relva ,(h1:hs1)):t))
mapaValido2 (Mapa l ((Rio v,(h:hs)):[])) = True
mapaValido2 (Mapa l ((Rio v,(h:hs)): (Rio v1,(h1:hs1)):t)) | v>0 && v1>0 = False
                                                           | v<0 && v1<0 = False
                                                           | otherwise = mapaValido2 (Mapa l ((Rio v1,(h1:hs1)):t))
{-|A mapavalido3 é criada para ter a certeza que um ronco tem no máximo 5 unidades de comprimento.
@
mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa l []) = True
mapaValido3 (Mapa l ((Relva, (h:hs)):t)) = mapaValido3 (Mapa l t)
mapaValido3 (Mapa l ((Estrada v, (h:hs)):t)) = mapaValido3 (Mapa l t)
mapaValido3 (Mapa l ((Rio v, (h:hs)):t)) | aux (group(h:hs)) > 5 = False
                                         | otherwise = mapaValido3 (Mapa l t) 


group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = insere h (group t)


insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)


aux :: [[Obstaculo]] -> Int
aux [a] = length a 
aux [] = 0
aux ((h:hs):t) | (elem Tronco (h:hs)) && length (h:hs) >= (aux t) = length (h:hs)
               | otherwise = aux t
@
Para isso tive de criar uma auxiliar group, que tem uma auxiliar insere dentro dela, a group serve para unir todos os elementos seguidos que sejam iguas.
A auxiliar aux foi criada para ver em uma lista de listas qual a length da maior lista cujos elementos sao troncos.
Passando assim para a funçao principal que começa por apresentar o caso de paragem como as restantes.
De seguida apresenta os casos que é Relva e Estrada, que devolvem a recursiva.
Concluindo apresenta os casos onde é rio e a aux, a length da maior lista, da group cujos elementos são troncos.-}
mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa l []) = True
mapaValido3 (Mapa l ((Relva, (h:hs)):t)) = mapaValido3 (Mapa l t)
mapaValido3 (Mapa l ((Estrada v, (h:hs)):t)) = mapaValido3 (Mapa l t)
mapaValido3 (Mapa l ((Rio v, (h:hs)):t)) | aux (group(h:hs)) > 5 = False
                                         | otherwise = mapaValido3 (Mapa l t) 


group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = insere h (group t)


insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)


aux :: [[Obstaculo]] -> Int
aux [a] = length a 
aux [] = 0
aux ((h:hs):t) | (elem Tronco (h:hs)) && length (h:hs) >= (aux t) = length (h:hs)
               | otherwise = aux t


{-|Muito semelhantemente com a de cima, a mapavalido4 utiliza as mesmas auxiliares que a mapavalido3, alterando apenas a auxiliar aux2 que vê a length da maior lista cujos membros são carros
@
mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa l []) = True
mapaValido4 (Mapa l ((Rio v, (h:hs)):t)) = mapaValido4 (Mapa l t)
mapaValido4 (Mapa l ((Relva, (h:hs)):t)) = mapaValido4 (Mapa l t)
mapaValido4 (Mapa l ((Estrada v, (h:hs)):t)) | aux2 (group(h:hs)) > 3 = False
                                         | otherwise = mapaValido4 (Mapa l t) 
aux2 :: [[Obstaculo]] -> Int
aux2 [a] = length a 
aux2 [] = 0
aux2 ((h:hs):t) | (elem Carro (h:hs)) && length (h:hs) >= (aux2 t) = length (h:hs)
                | otherwise = aux2 t
@
De seguida tem o caso de paragem, os casos quando é rio e relva.
Por fim os casos onde é estrada e onde a aux2 da group onde membros da maior lista são carros tem de ser menor ou igual a 3. -}
mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa l []) = True
mapaValido4 (Mapa l ((Rio v, (h:hs)):t)) = mapaValido4 (Mapa l t)
mapaValido4 (Mapa l ((Relva, (h:hs)):t)) = mapaValido4 (Mapa l t)
mapaValido4 (Mapa l ((Estrada v, (h:hs)):t)) | aux2 (group(h:hs)) > 3 = False
                                         | otherwise = mapaValido4 (Mapa l t) 
aux2 :: [[Obstaculo]] -> Int
aux2 [a] = length a 
aux2 [] = 0
aux2 ((h:hs):t) | (elem Carro (h:hs)) && length (h:hs) >= (aux2 t) = length (h:hs)
                | otherwise = aux2 t


{-|A função mapavalido5 é criada com o objetivo de garantir que em qualquer linha existe pleo menos um nenhum.Ou seja, uma linha nao pode ser composta exclusivamente por obstaculos, precisando de haver pelo menos um espa¸co livre.
@
mapaValido5 :: Mapa -> Bool
mapaValido5 (Mapa l []) = True
mapaValido5 (Mapa l ((Relva, (h:hs)):t)) |elem Nenhum (h:hs) = mapaValido5 (Mapa l t)
                                        |otherwise = False
mapaValido5 (Mapa l ((Rio v, (h:hs)):t)) |elem Nenhum (h:hs) = mapaValido5 (Mapa l t)
                                        |otherwise = False
mapaValido5 (Mapa l ((Estrada v, (h:hs)):t)) |elem Nenhum (h:hs) = mapaValido5 (Mapa l t)
                                            |otherwise = False
@
A função começa por apresentar o caso de paragem, de seguida quando o terreno é relva, a função vê se existe algum nenhum, caso contrãrio é falso.
Depois repete o mesmo para quando o terreno é Rio ou Estrada. -}


mapaValido5 :: Mapa -> Bool
mapaValido5 (Mapa l []) = True
mapaValido5 (Mapa l ((Relva, (h:hs)):t)) |elem Nenhum (h:hs) = mapaValido5 (Mapa l t)
                                        |otherwise = False
mapaValido5 (Mapa l ((Rio v, (h:hs)):t)) |elem Nenhum (h:hs) = mapaValido5 (Mapa l t)
                                        |otherwise = False
mapaValido5 (Mapa l ((Estrada v, (h:hs)):t)) |elem Nenhum (h:hs) = mapaValido5 (Mapa l t)
                                            |otherwise = False

{-|A Função mapavalido6 tem o objetivo de confirmar que o comprimento da lista fica igual á largura.
@
mapaValido6 :: Mapa -> Bool
mapaValido6 (Mapa l []) = True
mapaValido6 (Mapa l ((Relva, (h:hs)):t)) |l== length (h:hs) = True
                                        |otherwise = False
mapaValido6 (Mapa l ((Rio v, (h:hs)):t)) |l== length (h:hs) = True
                                        |otherwise = False
mapaValido6 (Mapa l ((Estrada v, (h:hs)):t)) |l==length (h:hs) = True
@
A função começa por apresentar o caso de paragem, e de seguida vê a length da lista e se esta for igual á largura, então a função é verdadeira caso contrário é falsa.
Depois faz o mesmo processo para quando o terreno é rio e estrada.-}
mapaValido6 :: Mapa -> Bool
mapaValido6 (Mapa l []) = True
mapaValido6 (Mapa l ((Relva, (h:hs)):t)) |l== length (h:hs) = True
                                        |otherwise = False
mapaValido6 (Mapa l ((Rio v, (h:hs)):t)) |l== length (h:hs) = True
                                        |otherwise = False
mapaValido6 (Mapa l ((Estrada v, (h:hs)):t)) |l==length (h:hs) = True
                                         |otherwise = False                                       
{-|A função mapavalido7 tem o objetivo de não deicar existir mais do que 4 rios nem 5 estradas ou relvas.
@
mapaValido7 :: Mapa -> Bool
mapaValido7 (Mapa l []) = True
mapaValido7 (Mapa l ((Rio v,(h:hs)):(Rio v1,(h1:hs1)):(Rio v2,(h2:hs2)):(Rio v3,(h3:hs3)):(Rio v4,(h4:hs4)):t)) = False
mapaValido7 (Mapa l ((Relva,(h:hs)): (Relva,(h1:hs1)):(Relva,(h2:hs2)):(Relva,(h3:hs3)):(Relva,(h4:hs4)):(Relva,(h5:hs5)):t)) = False
mapaValido7 (Mapa l ((Estrada v,(h:hs)): (Estrada v1,(h1:hs1)):(Estrada v2,(h2:hs2)):(Estrada v3,(h3:hs3)):(Estrada v4,(h4:hs4)):(Estrada v5,(h5:hs5)):t)) = False
mapaValido7 (Mapa l ((terreno ,(h:hs)):t)) = mapaValido7 (Mapa l t)
@
A função começa por apresentar o caso de paragem, depois apresenta o caso onde aparecem 5 rios seguidos mostrando que dá falso a partir daí.
O mesmo para quado é estrada e relva. No ultimo, temos a recursividade para chegar a um booleano como resultado.
Concluindo estão defenidas as 7 funções que definem a função inicial.-}  
mapaValido7 :: Mapa -> Bool
mapaValido7 (Mapa l []) = True
mapaValido7 (Mapa l ((Rio v,(h:hs)):(Rio v1,(h1:hs1)):(Rio v2,(h2:hs2)):(Rio v3,(h3:hs3)):(Rio v4,(h4:hs4)):t)) = False
mapaValido7 (Mapa l ((Relva,(h:hs)): (Relva,(h1:hs1)):(Relva,(h2:hs2)):(Relva,(h3:hs3)):(Relva,(h4:hs4)):(Relva,(h5:hs5)):t)) = False
mapaValido7 (Mapa l ((Estrada v,(h:hs)): (Estrada v1,(h1:hs1)):(Estrada v2,(h2:hs2)):(Estrada v3,(h3:hs3)):(Estrada v4,(h4:hs4)):(Estrada v5,(h5:hs5)):t)) = False
mapaValido7 (Mapa l ((terreno ,(h:hs)):t)) = mapaValido7 (Mapa l t)
