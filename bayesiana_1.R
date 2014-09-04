# INTRODUCCION A R

#Ejercicio 1
peso<-c(63,52,78,49,71,62,68,48,56,67)
summary(peso)

#Ejercicio 2
calificaciones<-c(68,84,75,82,68,90,62,88,76,93,73,79,88,73,60,93,71,59,85,75,
                  61,65,75,87,74,62,95,78,63,72,66,78,82,75,94,77,69,74,68,60,
                  96,78,89,61,75,95,60,79,83,71,79,62,67,97,78,85,76,65,71,75,
                  65,80,73,57,88,78,62,76,53,74,86,67,73,81,72,63,76,75,85,77)

#Tabla de frecuencia
mi_tabla<-table(calificaciones)
hist(calificaciones)
sum(table(calificaciones[calificaciones>=75]))

# SIMULACION

# Ejercicio 1

#Escriba un programa en R que implemente el método de cuadrados medios.

# Esta funcion solo acepta semillas mayoes a 4 digitos, si se introduce una semilla menor a 1000
# El algoritmo se detiene.
cuad_medios<-function(n,semilla=2310){
  if(nchar(as.character(semilla)) < 4) stop("semilla no valida")
  x<-numeric(length=(n+1))
  x[1]<-semilla
  i<-1 
  repeat{
    y<-as.character(x[i]^2)
    if (nchar(y)%%2==0){
      y<-substr(y, (nchar(y)-4)/2+1, nchar(y)-(nchar(y)-4)/2)
    }else{
      y<-substr(y, (nchar(y)-4)%/%2+1, nchar(y)-(nchar(y)-4)%/%2-(nchar(y)-4)%%2)
    }
    i<-i+1
    x[i]<-as.numeric(y)
    if (i>=length(x)) {break()} 
  }
  x[2:length(x)]/10000
}

# Esta funcion acepta semillas con menos digitos que el anterior, el problema de semillas pequenas es que despues
# de cierto numero de iteraciones se generan valores redundantes.
cuad_medios2<-function(n,semilla=2310){
  x<-numeric(length=(n+1))
  x[1]<-semilla
  i<-1 
  repeat{
    y<-as.character(x[i]^2)
    if(nchar(y) ==1) {y<-paste("000",y,sep="")}
    if(nchar(y) ==2) {y<-paste("00",y,sep="")}
    if(nchar(y) ==3) {y<-paste("0",y,sep="")}
    if (nchar(y)%%2==0){
      y<-substr(y, (nchar(y)-4)/2+1, nchar(y)-(nchar(y)-4)/2)
    }else{
      y<-substr(y, (nchar(y)-4)%/%2+1, nchar(y)-(nchar(y)-4)%/%2-(nchar(y)-4)%%2)
    }
    i<-i+1
    x[i]<-as.numeric(y)
    if (i>=length(x)) {break()} 
  }
  x[2:length(x)]/10000
}

# Escriba una función en R que implemente un generador congruencial

gen_congru<-function(n,semilla){
  
  U<-numeric(length=(n+1))
  U[1]<-semilla
  a<-62089911
  m<-(2^31)-1
  i<-1 
  repeat{
    i<-i+1
    U[i]<-(a*U[i-1])%%m
    if (i>=length(U)) {break()} 
  }
  U[2:length(U)]/m
}
#se generan 10,000 numeros aleatorios
x<-gen_congru(10000,14091988)
#grafica del valor contra el indice
plot(x,type="h")
#graficar hitograma
hist(x)
x<-gen_congru(20000,14091988)
plot(x[length(x)-1],x[length(x)])

# Ejercicio 2

# Haga un programa que permita generar n variables aleatorias

val_random <- function(n,p=c(0.05,0.1,0.15,0.3,0.4),num=c(10, 20, 30, 40, 50)){
  
  psum <- cumsum(p)
  U <- runif(n)
  val <- numeric(n)
  for (i in 1:n) {
    val[i] <- min(num[U[i]<psum])
  }
  val
}
  
x<-val_random(10000)
hist(x,col="Red",main="Histograma de X",sub="",xlab="Valor de x",ylab="Frecuencia")


# Realice una programa en R, para generar n de una distribución Gamma

mi_gamma<-function(n,alpha,beta){
  a<-floor(alpha)
  b<-a/alpha
  c<-alpha-a
  y<-numeric(length=n)
  i<-1
  j<-1
  while (i <= n){
    u=runif(a+1,0,1)
    x=-1/b*sum(log(u[1:a]))
    if (u[a+1]<(x/alpha)^(c)*exp(-(c*(x-a))/alpha)){
      y[i]=x/beta
      i=i+1
    }
    j=j+1
  }
  y
}
hist(mi_gamma(10000,11.4,0.7), freq=FALSE,col=3,
     main="Histograma de mi_gamma",sub="Distribucion Gamma",xlab="Valor de x",ylab="Densidad")

curve(dgamma(x,11.4,0.7), col=2, add=T)

# Use el metodo de rechazo comparando con una distribucion uniforme para generar variables
# que tengan una densidad triangular en [−1, 1]

mi_rechazo1<-function(n){
  x<-numeric(length=n)
  i<-0
  repeat{
    y<-2*runif(1)-1
    u<-runif(1)*1
    if (y<=0){
      fy<-(y+1)
    }else{
      fy<-(1-y)
    }
    if (u<fy){
      i<-i+1
      x[i]<-y
    }
    if (i>=n) {break()} 
  }
  x
}

hist(mi_rechazo1(100000), freq=FALSE,col=3,
     main="Histograma de mi_rechazo1",sub="Distribucion triangular",xlab="Valor de x",ylab="Densidad")
abline(1,1,col=2)  
abline(1,-1,col=2)  

# 1 Calcule, via Monte Carlo, P(X ≤ 3), donde X ∼ N(0,2)
monte_carlo1<-function(){
  n <- 10^6
  x <- runif(n)*(3-0)+0
  g<-(1/(sqrt(4*pi)))*exp(-(x^2)/(4))
  mt<-0.5+sum(g)*3/n
}
plot(x<-seq(-6,6,length=100),dnorm(x,0,sqrt(2)), col=2,type="l",
     main="Distribucion N(0,2)",sub="P(X<=3)",xlab="Valor de x",ylab="Densidad")
abline(h=0)
abline(v=0)
polygon(c(-6,seq(-6,3,length=100),3),c(0,dnorm(seq(-6,3,length=100),0,sqrt(2)),0),
        col = "gray", border = "red")
text(0,0.1,paste("P(X<=3)=",as.character(monte_carlo1())),srt=0,cex=1)


#2 para cualquier g. En este problema sepide la integral de 0 a infinito para cualquier G,
# en ese intervalo. Se hizo para la normal N(0,1) y su integral de 0 a infinito es 0.5;
# tambien para la exponencial y la ji-cuadrada, donde su integal de 0 a inf es 1.
feval <- function(f, a){
  f(a)
}

monte_carlo2<-function(f){
  n <- 10^6
  x <- runif(n)
  g <- feval(f,(1/x-1))
  h <- g/(x^2)
  mt <- sum(h)/n
}

mi_normal<-function(x){
  mu<-0
  sigma<-1
  fx<-(1/sigma/sqrt(2*pi)*exp(-((x-mu)^2)/(2*sigma^2)))
}

mi_chiq2<-function(x){
  gl<-2
  fx<-(1/(2^(gl/2)*gamma(gl/2))*x^(gl/2-1)*exp(-x/2))
}

mi_chiq3<-function(x){
  gl<-3
  fx<-(1/(2^(gl/2)*gamma(gl/2))*x^(gl/2-1)*exp(-x/2))
}

mi_exponencial<-function(x){
  lambda<-1
  fx<-(lambda*exp(-lambda*x))
}


q<-monte_carlo2(mi_normal)

plot(x<-seq(-6,6,length=100),dnorm(x,0,1), col=2,type="l",
     main="Distribucion N(0,1)",sub="Integral de 0 a Inf",xlab="Valor de x",ylab="Densidad")
abline(h=0)
abline(v=0)
polygon(c(0,seq(0,6,length=100),6),c(0,dnorm(seq(0,6,length=100),0,1),0),
        col = "gray", border = "red")
text(0,0.2,paste("I=",as.character(q)),srt=0,cex=1)
#La I es de integral

# w<-monte_carlo2(mi_chiq2)
# plot(x<-seq(0,10,length=100),dchisq(x,2), col=2,type="l",
#      main="Distribucion x^2(2)",sub="Integral de 0 a Inf",xlab="Valor de x",ylab="Densidad")
# abline(h=0)
# abline(v=0)
# polygon(c(0,seq(0,10,length=100),10),c(0,dchisq(seq(0,10,length=100),2),0),
#         col = "gray", border = "red")
# text(5,0.2,paste("I=",as.character(w)),srt=0,cex=1)
# #La I es de integral

o<-monte_carlo2(mi_chiq3)
plot(x<-seq(0,10,length=100),dchisq(x,3), col=2,type="l",
     main="Distribucion x^2(3)",sub="Integral de 0 a Inf",xlab="Valor de x",ylab="Densidad")
abline(h=0)
abline(v=0)
polygon(c(0,seq(0,10,length=100),10),c(0,dchisq(seq(0,10,length=100),3),0),
        col = "gray", border = "red")
text(5,0.2,paste("I=",as.character(o)),srt=0,cex=1)
#La I es de integral


s<-monte_carlo2(mi_exponencial)
plot(x<-seq(0,6,length=100),dexp(x), col=2,type="l",
     main="Distribucion Exp(0,1)",sub="Integral de 0 a Inf",xlab="Valor de x",ylab="Densidad")
abline(h=0)
abline(v=0)
polygon(c(0,seq(0,6,length=100),6),c(0,dexp(seq(0,6,length=100)),0),
        col = "gray", border = "red")
text(3,0.2,paste("I=",as.character(s)),srt=0,cex=1)
#La I es de integral


# 3 Encuentre un valor aproximado para la integral
monte_carlo3<-function(){
  n <- 10^6
  x <- runif(n)
  g <- sqrt(x+sqrt(x))
  mt <- sum(g)/n
}

#Encuentre un valor aproximado para la integral
monte_carlo3_var<-function(){
  n <- 10^6
  x <- runif(n)
  g <- sqrt(x+sqrt(x))
  mu <- sum(x*g)/n
  v <- sum((x-mu)*(x-mu)*g)/n
}
plot(c(seq(0,2,length=100)),sqrt(seq(0,2,length=100)+sqrt(seq(0,2,length=100))),
     col = "red",type="l",main="",sub="Integral de 0 a 1 ",xlab="x",
     ylab="f(x)")
abline(h=0)
abline(v=0)
polygon(c(0,seq(0,1,length=100),1),c(0,sqrt(seq(0,1,length=100)+sqrt(seq(0,1,length=100))),0),
        col = "gray", border = "blue")
text(0.5,0.5,paste("I=",as.character(monte_carlo3())),srt=0,cex=1)
text(0.5,0.3,paste("Var=",as.character(monte_carlo3_var())),srt=0,cex=1)
