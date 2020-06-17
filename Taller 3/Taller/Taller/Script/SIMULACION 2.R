### EJERCICIO 2:
### CONTRASTE DE HIPOTESIS
### CUANDO NO SE CUMPLE LA HIPOTESIS NULA
### EN UNA DISTRIBUCION NORMAL CON VARIANZA 36
### H0: Mu=20
### Ha: Mu>20

# SE GENERA LA MUESTRA PARA DIFERENTES TAMANOS DE MUESTRA
# PARA REALIZAR EL CONTRASTE SE TOMARA UNA MEDIA MAYOR A LA DE LA HIPOTESIS Mu=25

p10 <- replicate(1000, 1-pnorm(mean(rnorm(10,25,36)),20,36/(sqrt(10)))) # PARA UNA MUESTRA
hist(p10)                                                               # DE TAMAÑO 10

p20 <- replicate(1000, 1-pnorm(mean(rnorm(20,25,36)),20,36/(sqrt(20)))) # PARA UNA MUESTRA
hist(p20)                                                               # DE TAMAÑO 20

p50 <- replicate(1000, 1-pnorm(mean(rnorm(50,25,36)),20,36/(sqrt(50)))) # PARA UNA MUESTRA
hist(p50)                                                               # DE TAMAÑO 50

# CALCULO DE LA POTENCIA DE LA PRUEBA

x<-seq(20,30,0.1)

plot(1-pnorm(qnorm(0.95,0,1)-(x-20)*sqrt(10)/6),type="n",ylim=c(0.05,1),       
     xlim=c(20,30),ylab="Potencia",xlab="Theta",cex.main=1,xaxs="i",bty="n")

# graficos las diferentes curvas de potencia   

curve(1-pnorm(qnorm(0.95,0,1)-(x-20)*sqrt(10)/6,0,1),from=20,to=30,add=T,col=1,lty=1,lwd=2)

curve(1-pnorm(qnorm(0.95,0,1)-(x-20)*sqrt(20)/6,0,1),from=20,to=30,add=T,col=2,lty=2,lwd=2)

curve(1-pnorm(qnorm(0.95,0,1)-(x-20)*sqrt(50)/6,0,1),from=20,to=30,add=T,col=3,lty=3,lwd=2)                                                      

legend("right",,paste("n",c(10,20,50),sep="="),lty=1:3,col=1:3,   
       title="Alpha=0.05",,bty="n")
