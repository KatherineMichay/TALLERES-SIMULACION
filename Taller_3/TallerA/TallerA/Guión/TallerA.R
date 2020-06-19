### Ejercicio 8:


#####literal a

library(numbers)

a1<-65
c1<-1
m1<-2048
#Tomemos p=m 

######################

#1: c y m son primos relativos
res<-  mGCD(c(c1,m1))
res
###2:a-1 es multiplo de todos los factores primos de m
b<-a1-1
b
primeFactors(m1)
d<-1 %% 2
d
#### por lo tanto d=a1

###3.si m es múltiplo de 4, entonces a-1 lo a de ser

m<-m1 %% 4

a<-(a1-1) %% 4
a
#Con lo que podemos concluir que es de ciclo máximo



######## Literal b
######################

source("Funciones/RANDC.R")   # Cargar RANDC.r
x<- initRANDC(semilla=600,a=65,c=1,m=2048)
nsim <- 1000
u <- RANDCN(nsim)  # Generar
u
source("cpu.time.R")
cpu.time(u,total = TRUE)

hist(u, freq = FALSE)
abline(h = 1)   

ks.test(u, "punif", 0, 1)


### Literal c):

source("TallerA/TallerA/Material/RANDC.r") # Se abre el archivo RANDC

initRANDC(semilla = 100, a = 65, c = 1, m = 2048) # La semilla de nuestro grupo y los parámetros del generador congruencial.
initRANDC(54321)    # Fijar semilla para reproductibilidad
nsim <- 1000
u <- RANDCN(nsim) # Este tramo de código se extrajo del Análisis RANDU de IMB

y <- matrix(u, ncol = 2, byrow = TRUE) # Se arman las 2-uplas con los valores consecutivos
plot(y) # Se grafícan las uplas en el plano cartesiano

### Literal d):

# La prueba se realiza 500 veces
x <- replicate(500,Box.test(RANDCN(50), lag = 10, type = "Ljung")[3]) # Se guarda el p-valor
z <- replicate(500,Box.test(RANDCN(50), lag = 10, type = "Ljung")[1]) # Se guarda el estadístico

hist(as.numeric(x)) # Se grafícan los p-valor
hist(as.numeric(z)) # Se grafícan los estadísticos

# Se verifica si el p-valor cumple con las proporciones
cat("\nProporcion de rechazos al 1% =", mean(x < 0.01), "\n") # Para alfa 0.01
cat("Proporcion de rechazos al 5% =", mean(x < 0.05), "\n") # Para alfa 0.05
cat("Proporcion de rechazos al 10% =", mean(x < 0.1), "\n") # Para alfa 0.1

