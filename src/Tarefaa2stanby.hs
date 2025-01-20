
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa lar t) seed = Mapa 

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa 1 []) = [Rio 0,Estrada 0,Relva]
proximosTerrenosValidos (Mapa 1 ((Rio a,b) : (Rio a1,b1) : (Rio a2,b2) : (Rio a3,b3) : t)) = [Estrada 0,Relva]
proximosTerrenosValidos (Mapa 1 ((Rio a,b):t)) = [Rio 0,Estrada 0,Relva]
proximosTerrenosValidos (Mapa 1 ((Estrada a,b) : (Estrada a1,b1):(Estrada a2,b2):(Estrada a3,b3):(Estrada a4,b4):t)) = [Rio 0,Relva]
proximosTerrenosValidos (Mapa 1 ((Estrada a,b):t)) = [Rio 0,Estrada 0,Relva]
proximosTerrenosValidos (Mapa 1 ((Relva,b):(Relva,b1):(Relva,b2):(Relva,b3):(Relva,b4):t)) = [Rio 0,Estrada 0]
proximosTerrenosValidos (Mapa 1 ((Relva,b):t) = [Rio 0,Estrada 0,Relva]


proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (x,y)
  |length y == n = []
  |elem Nenhum x == False && length y == (n-1) = [Nenhum]
  |otherwise = ObsAux n(x,y)
ObsAux n (Estrada a,[]) = [Nenhum,Carro]
ObsAux n (Rio a,[]) = [Nenhum,Tronco]     
ObsAux n (Relva a,[]) = [Nenhum,Arvore] 
ObsAux n (Rio a,(Tronco:Tronco:Tronco:Tronco:Tronco:t)) = [Nenhum]
ObsAux n (Rio a,b) = [Nenhum,Tronco]
ObsAux n (Estrada a,(Carro:Carro:Carro:t)) = [Nenhum]
ObsAux n (Estrada a,b) = [Nenhum,Carro]
ObsAux n (Relva,b) = [Nenhum,Arvore]

{-| O objetivo principal da tarefa 2 é implementar a função estendeMapa.
Para o fazer, é necessário aplicar duas outras funções auxiliares, a função
proximosTerrenosValidos e a função proximosObstaculosValidos (para esta segunda
usei outra função auxiliar como complemento, a função obsAux.)
A função proximosTerrenosValidos cria uma lista de terrenos que podem ser usados 
numa nova linha no topo do mapa dado, sendo que para esta função o parâmetro velocidade
é ignorado, ou seja, assume-se o valor 0. Quando o mapa dado é vazio, todos os terrenos 
são válidos, no entanto, se o topo do mapa já tiver 4 rios, não pode haver outra linha de
rio. 
    Outra função auxiliar usada é a proximosObstaculosValidos. Esta cria uma lista de obs-
táculos que podem ser usados para dar seguimento a uma determinada linha do mapa. O primeiro
parâmetro (Int) é correspondente à largura do mapa, devendo-se ter em atenção os tipos de
obstáculos respetivos ao terreno e também o comprimento dos obstáculos. Além disso, o parâmetro 
Int condiciona a função, no sentido em que a partir do momento que o comprimento da lista de obs-
táculos atinge a largura do mapa, não é permitido adicionar mais obstáculos.
   Por fim, a função estendeMapa deve adicionar uma nova linha ao topo de um dado mapa. O parâ-
metro Int é um inteiro contido no intervalo [0,100], sendo que os terrenos criados têm de ter ve-
locidades definidas -}  