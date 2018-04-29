g= 9.7833 #gravidade
h= 0.12   #altura da esfera ao primeiro sensor

#Medidas = read.table("tabela_medidas2.csv", head=T)

#Medidas


#Velocidade inicial = Vo

# criando funcao veloc. inicial
Vo <- function (g,h){
  Vo <- sqrt(2*g*h)
  return (Vo)
}


 Epot <- function(g,m,h){
   Ep <- m*g*h
   Ep
 }
 
Ecine <- function(m,v){
   Ec <- m*v*v/2
   Ec
}

 Emec <- function(g,m,h,v){
   Em <- Epot(m,g,h) + Ecine(m,v)
   Em
 }
 
 E.elastica<- function(k,x){ #ENERGIA ELASTICA
   E <- k*x*x/2
   E
 }

 QuedaLivre.ac <- function(Vf, V0, t){ # Acelera??o
   Ac <- (Vf - V0) / t
   Ac
 }
 
# importando bibliotecas para gerar gr?ficos 
library(geoR)
library(grid)
library(gridExtra)

# lendo arquivo csv do excel 
View(QuedaL)
QuedaL <- read.csv("QuedaL.csv", sep=";")#LENDO BASE DE QUEDA LIVRE

# acessando coluna pertencente ao titulo "v0" da tabela e chamando cada valor na fun??o
QuedaL$v0 <- Vo(g,QuedaL$H) #CALCULANDO VELOCIDADE INICIAL
#setando 3 digitos de casa decimal em cada valor de QuedaL$v0
QuedaL$v0 <- round(QuedaL$v0, digits=3)

QuedaL$vf <- Vo(g,QuedaL$Xexp+QuedaL$H) #CALCULANDO VELOCIDADE FINAL
QuedaL$vf <- round(QuedaL$vf, digits=3)

QuedaL$dv <- QuedaL$vf-QuedaL$v0 #CALCULANDO VARIACAO DE VELOCIDADE


#colnames(QuedaL) <- c("Medidas", "H(m)", "T1(s)", "T2(s)", "T(s)","Xexp(m)",
    #                  "V0(m/s)", "Vf(m/s)", "Var.V(m/s)")
#grid.table(QuedaL)


QuedaL$m <- 0.028

eixoY <- c(QuedaL$Xexp)
eixoY <- eixoY+0.12
eixoY <- 0.565-eixoY

QuedaL$Ep <- Epot(g,QuedaL$m,eixoY) #CALCULANDO ENERGIA POTENCIAL
QuedaL$Ep <- round(QuedaL$Ep, digits=3)

QuedaL$Ec <- Ecine(QuedaL$m,QuedaL$vf) #CALCULANDO ENERGIA CINETICA
QuedaL$Ec <- round(QuedaL$Ec, digits=3)

QuedaL$Em <- Emec(g,QuedaL$m,QuedaL$H,QuedaL$vf) #CALCULANDO ENERGIA MECANICA
QuedaL$Em <- round(QuedaL$Em, digits=3)

QuedaL$ac <- QuedaLivre.ac(QuedaL$vf, QuedaL$v0, QuedaL$T.s.) #Acelera??o
QuedaL$ac <- round(QuedaL$ac, digits=3)

#vetor com o nome de colunas para printar na tabela
colnames(QuedaL) <- c("Medidas", "H(m)", "T1(s)", "T2(s)", "T(s)","Xexp(m)",
                      "V0(m/s)", "Vf(m/s)", "Var.V(m/s)","Massa(kg)", "Epot(J)", "Ecin(J)", "Emec(J)",
                      "A(m/s^2)")

#criando tabela
grid.table(QuedaL)

#criando grafico de QuedaL$T.s(tempo) por QuedaL$exp(energia exponencial)
plot(QuedaL$T.s., QuedaL$Xexp, type="o", col="blue", ann=FALSE, ylim=c(0, 0.54)) #eixo y vai de 0 a 0.54
title(main="Grafico - Posi??o x Tempo", col.main="black", xlab="Tempo (s)", ylab="Posi??o (m)") # xlab e ylab s?o as labels, main o titulo do grafico
text(QuedaL$T.s.,QuedaL$Xexp, round(QuedaL$Xexp, digits=3),pos=1,cex=0.6)
grid(col="black")


plot(QuedaL$T.s., QuedaL$vf, type="o", col="blue", ann=FALSE, ylim=c(1.5, 3.5))
title(main="Gr?fico - Velocidade Final x Tempo", col.main="black", xlab="Tempo (s)", ylab="Velocidade Final (m/s)")
text(x=QuedaL$T.s.,QuedaL$vf, round(QuedaL$vf, digits=3),pos=1,cex=0.6)
grid(col="black")

#esse
plot(QuedaL$T.s., QuedaL$dv , type="o", col="blue", ann=FALSE, ylim=c(0.2, 2.0))
title(main="Gr?fico - Varia??o da Velocidade x Tempo", col.main="black", xlab="Tempo (s)", ylab="Varia??o da Velocidade (m/s)")
text(x=QuedaL$T.s.,y=QuedaL$dv, round(QuedaL$dv, digits=3),pos=1,cex=0.6)
grid(col="black")

plot(QuedaL$T.s., QuedaL$ac, type="o", col="blue", ann=FALSE, ylim=c(9.6,10.1))
title(main="Gr?fico - Acelera??o x Tempo", col.main="black", xlab="Tempo (s)", ylab="Acelera??o (m/s?)")
text(x=QuedaL$T.s.,y=QuedaL$ac, round(QuedaL$ac, digits=3),pos=1,cex=0.6)
grid(col="black")
m=0.028

#Potencial
eixoX <- c(QuedaL$T.s.)
eixoY <- c(QuedaL$H)
eixoY <- 0.565-eixoY
Ep <- Epot(g,m,eixoY) 
#Ep <- Energia.Pot(QuedaL$m, g, eixoY)
plot(eixoX,QuedaL$Ep, col="blue", type="o", ann=FALSE, ylim=c(-0.05,0.125))
title(main="Gr?fico - Energia Potencial x Tempo", xlab="Tempo (s)", ylab="Energia Potencial (J)", col.main="black")
text(x=eixoX,y=QuedaL$Ep,round(QuedaL$Ep, digits=3),pos=1,cex=0.6)
grid(col="black")


#Cin?tica
eixoX <- c(QuedaL$T.s.)
eixoY <- c(QuedaL$vf)
Ec <- Ecine(m, eixoY)
Ec <- round(Ec, digits=3)
plot(eixoX, Ec, col="blue", type="o", ann=FALSE, ylim=c(0,0.17))
title(main="Gr?fico - Energia Cin?tica x Tempo",xlab="Tempo (s)",ylab="Energia Cin?tica (J)", col.main="black")
text(x=eixoX,y=Ec,round(Ec, digits=3),pos=1,cex=0.6)
grid(col="black")





#Mec?nica
eixoX <- c(QuedaL$T.s.)
oh <- c(QuedaL$Xexp)
oh <- oh+0.12
oh <- 0.565-oh
Ep <- Epot(g,m,oh)
eixoY <- c(QuedaL$vf)
Ec <- Ecine(m, eixoY)
Em <- round(Ep+Ec, digits=3)
plot(x=eixoX,y=Em,main="Gr?fico - Energia Mec?nica x Tempo",xlab="Tempo (s)",ylab="Energia (J)",ylim=c(0,0.2))
lines(x=eixoX,y=Em,type="l", col="red")
text(x=eixoX,y=Em,Em,pos=1,cex=0.6)
lines(x=eixoX,y=Ep,type="l",col="green")
text(x=eixoX[1],y=Ep[1],"Energia potencial",pos=4,cex=0.7)
lines(x=eixoX,y=Ec,type="l",col="blue")
text(x=eixoX[1],y=Ec[1],"Energia cin?tica",pos=4,cex=0.7)
grid(col="black")
