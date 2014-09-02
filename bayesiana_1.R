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

x<-gen_congru(10000,14091988)
plot(x,c(1:length(x)))
plot(c(1:length(x)),x,type="h")
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
  
# Realice una programa en R, para generar n de una distribución Gamma

mi_gamma<-function(n,alfa,beta,lim){
  z<-seq(1/1000,lim,length=10000)
  fz<-max(((beta^alfa)/(gamma(alfa)))*exp(-beta*z)*(z)^(alfa-1))
  x<-numeric(length=n)
  i<-0
  repeat{
    y<-lim*runif(1)
    u<-runif(1)*fz
    fy<-((beta^alfa)/(gamma(alfa)))*exp(-beta*y)*(y)^(alfa-1)
    if (u<fy){
      i<-i+1
      x[i]<-y
    }
    if (i>n) {break()} 
  }
  x
}


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



monte_carlo1<-function(){
  n <- 10000000
  x <- runif(n)*3
  g<-(1/(sqrt(4*pi)))*exp(-(x*x)/(4))
  mt<-0.5+sum(g)/n
}

monte_carlo3<-function(){
  n <- 100000
  x <- runif(n)
  g <- sqrt(x+sqrt(x))
  mt <- sum(g)/n
}

monte_carlo3_var<-function(){
  n <- 100000
  x <- runif(n)
  g <- sqrt(x+sqrt(x))
  mu <- sum(x*g)/n
  v <- sum((x-mu)*(x-mu)*g)/n
}
