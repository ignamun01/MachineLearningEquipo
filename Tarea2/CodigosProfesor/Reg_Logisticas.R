
#Nos ubicamos en la ruta de trabajo
setwd("~/Documents/Cursos en la FC/Seminario y Diplomado/Ejemplos")

my_data <- read.delim("Crabs.txt",sep = "")
model <- glm(y ~ width, data = my_data, family = binomial)
summary(model)
L0=logLik(model)

alpha=model$coefficients[1]
beta=model$coefficients[2]
alpha
beta 
plot(my_data$width,my_data$y)
fexp<-function(x){
   exp(alpha+beta*x)/(1+exp(alpha+beta*x))
}


plot(my_data$width,my_data$y)
curve(fexp,from =20,to = 34, add = TRUE)

str(my_data)
my_data$color<-as.factor(my_data$color)
my_data$y<-as.factor(my_data$y)
str(my_data)
model2 <- glm( y ~ width+color, data = my_data, family = binomial)
summary(model2)

my_data$fcolor <- relevel(my_data$color, ref = "4")
str(my_data)
model3 <- glm( y ~ width+fcolor, data = my_data, family = binomial)
summary(model3)
model3$coefficients[1]
model3$coefficients[2]
model3$coefficients[3]
model3$coefficients[4]
model3$coefficients[5]

fexp1<-function(x){
  exp(model3$coefficients[1]+model3$coefficients[3]+model3$coefficients[2]*x)/(1+exp(model3$coefficients[1]+model3$coefficients[3]+model3$coefficients[2]*x))
}
fexp2<-function(x){
  exp(model3$coefficients[1]+model3$coefficients[4]+model3$coefficients[2]*x)/(1+exp(model3$coefficients[1]+model3$coefficients[4]+model3$coefficients[2]*x))
}
fexp3<-function(x){
  exp(model3$coefficients[1]+model3$coefficients[5]+model3$coefficients[2]*x)/(1+exp(model3$coefficients[1]+model3$coefficients[5]+model3$coefficients[2]*x))
}
fexp4<-function(x){
  exp(model3$coefficients[1]+model3$coefficients[2]*x)/(1+exp(model3$coefficients[1]+model3$coefficients[2]*x))
}


val<-seq(18,34,0.5)
val
plot(val,fexp1(val),pch=19,col="blue",type = "b",lwd=2,xlab="X",ylab="P(Y|X)")
lines(val,fexp2(val),pch=19,col="red",type = "b",lwd=2)
lines(val,fexp3(val),col="green",type = "b",lwd=2)
lines(val,fexp4(val),col="black",type = "b",lwd=2)
legend(x = "right",legend=c("color 1", "color 2","color 3","color 4"),
       col=c("blue", "red","green","black"), lty=1, cex=0.9,box.lty=0,bg=NULL,lwd=3)


L1=logLik(model3)
L0
L1
G2=-2*(L0-L1)
G2
