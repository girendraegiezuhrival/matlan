#Nim : 17523154
#Nama : Girendra Egir Z.

#No.1
#Data
data1 <- read.csv(file.choose(), header=TRUE)
data1
#Linear Regresion
model <-lm(x ~ y, data=data1)
summary(model)

#No.2
#Data
data1 <- read.csv(file.choose(), header=TRUE)
data1
#Linear Regresion
model <-lm(x ~ y, data=data1)
summary(model)
plot(x ~ y, data=data1)
abline(model, col = "red", lwd = 1)
# Predicting New Value based on our model
predict(model, data.frame(x = 55))

#No.3 (c)
#No.4 (c)
#No.5 (c). plot (xi,yi)
#          curve(fi)

#No.6 =(c).7
#No.7 =(d).tidak ada
#No.8 =(c).ketika re =0.0001
#No.9 =(b).relatve error
#N0.10 =(d) 14-13

#No.11
library(pracma)
trapz <- function(x)(x^2)-6
trapzfun(trapz,0,1)

#No.12
library(pracma)
trapz <- function(x)(x^3)+4*(x^2)-10
trapzfun(trapz,1,2)

#13. (a.) (L <- h*(f0+2*sum(fi)+fn)/2)
h <- 0.1
x <-seq(0,1,by=h)
f <-function(x){
  return(x^2)
}
f0 <-f(x[1])
fi <-sapply(x[2:10],f)
fn <- f(x[length(x)])
trap <- function(f0, fi,fn,h){
  L <- h*(f0+2*sum(fi)+fn)/2
  return(L)
}
trap(f0,fi,fn,h)

#14. (b.) 0.335
#15. (a) 
h <- 0.2
x <-seq(0,1,by=h)
f <-function(x){
  return(x^2)
}
f0 <-f(x[1])
fi <-sapply(x[2:5])
fn <- f(x[length(x)])
trap <- function(f0, fi,fn,h){
  L <- h*(f0+2*sum(fi)+fn)/2
  return(L)
}
trap(f0,fi,fn,h)