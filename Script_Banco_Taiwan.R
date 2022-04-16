#Analisis para ver si es posible otorgar o no credito a un cliente con una data de 30 mil datos
#provenientes de un banco en Taiwan

install.packages("fdth")
install.packages("e1071")
install.packages("faraway")
install.packages("dplyr")
install.packages("tidyverse")
creditos<-read.table("../Escritorio/Artículos/credit_scoring.txt", header=T)
summary(creditos)
library(fdth)
library(e1071)
library(faraway)
library(dplyr)
library(tidyverse)

#aplicando analisis de boxplot para visualizar outlier
boxplot(creditos$LIMIT_BAL)
boxplot(creditos$AGE)
boxplot(creditos$BILL_AMT1)
boxplot(creditos$BILL_AMT2)
boxplot(creditos$BILL_AMT3)
boxplot(creditos$BILL_AMT4)
boxplot(creditos$BILL_AMT5)
boxplot(creditos$BILL_AMT6)
boxplot(creditos$PAY_AMT1)
boxplot(creditos$PAY_AMT2)
boxplot(creditos$PAY_AMT3)
boxplot(creditos$PAY_AMT4)
boxplot(creditos$PAY_AMT5)
boxplot(creditos$PAY_AMT6)

#observamos gran cantidad de valores atipicos para cada una de las variables analizadas
#Utilizare el metodo de correccion basado en percentiles
quantile(creditos$LIMIT_BAL, probs=c(0.1))
quantile(creditos$LIMIT_BAL, probs=c(0.9))
quantile(creditos$AGE, probs=c(0.1))
quantile(creditos$AGE, probs=c(0.9))
quantile(creditos$BILL_AMT1, probs=c(0.1))
quantile(creditos$BILL_AMT1, probs=c(0.9))
quantile(creditos$BILL_AMT2, probs=c(0.1))
quantile(creditos$BILL_AMT2, probs=c(0.9))
quantile(creditos$BILL_AMT3, probs=c(0.1))
quantile(creditos$BILL_AMT3, probs=c(0.9))
quantile(creditos$BILL_AMT4, probs=c(0.1))
quantile(creditos$BILL_AMT4, probs=c(0.9))
quantile(creditos$BILL_AMT5, probs=c(0.1))
quantile(creditos$BILL_AMT5, probs=c(0.9))
quantile(creditos$BILL_AMT6, probs=c(0.1))
quantile(creditos$BILL_AMT6, probs=c(0.9))
quantile(creditos$PAY_AMT1, probs=c(0.1))
quantile(creditos$PAY_AMT1, probs=c(0.9))
quantile(creditos$PAY_AMT2, probs=c(0.1))
quantile(creditos$PAY_AMT2, probs=c(0.9))
quantile(creditos$PAY_AMT3, probs=c(0.1))
quantile(creditos$PAY_AMT3, probs=c(0.9))
quantile(creditos$PAY_AMT4, probs=c(0.1))
quantile(creditos$PAY_AMT4, probs=c(0.9))
quantile(creditos$PAY_AMT5, probs=c(0.1))
quantile(creditos$PAY_AMT5, probs=c(0.9))
quantile(creditos$PAY_AMT6, probs=c(0.1))
quantile(creditos$PAY_AMT6, probs=c(0.9))

#Sustituyendo los outlier por los valores obtenidos de percentiles
creditos$LIMIT_BAL[creditos$LIMIT_BAL>360000]<-360000
creditos$LIMIT_BAL[creditos$LIMIT_BAL<30000]<-30000
creditos$AGE[creditos$AGE>49]<-49
creditos$AGE[creditos$AGE<25]<-25
creditos$BILL_AMT1[creditos$BILL_AMT1>142133.7]<-142133.7
creditos$BILL_AMT1[creditos$BILL_AMT1<278.9]<-278.9
creditos$BILL_AMT2[creditos$BILL_AMT2<0]<-0
creditos$BILL_AMT2[creditos$BILL_AMT2>136905.5]<-136905.5
creditos$BILL_AMT3[creditos$BILL_AMT3<0]<-0
creditos$BILL_AMT43[creditos$BILL_AMT3>132051.3]<-132051.3
creditos$BILL_AMT4[creditos$BILL_AMT4<0]<-0
creditos$BILL_AMT5[creditos$BILL_AMT5>115883]<-115883
creditos$BILL_AMT5[creditos$BILL_AMT5<0]<-0
creditos$BILL_AMT6[creditos$BILL_AMT6>112110.4]<-112110.4
creditos$BILL_AMT6[creditos$BILL_AMT6<0]<-0
creditos$PAY_AMT1[creditos$PAY_AMT1>10300]<-10300
creditos$PAY_AMT1[creditos$PAY_AMT1<0]<-0
creditos$PAY_AMT2[creditos$PAY_AMT2<0]<-0
creditos$PAY_AMT2[creditos$PAY_AMT2>10401.1]<-10401.1
creditos$PAY_AMT3[creditos$PAY_AMT3<0]<-0
creditos$PAY_AMT3[creditos$PAY_AMT3>10000]<-10000
creditos$PAY_AMT4[creditos$PAY_AMT4<0]<-0
creditos$PAY_AMT4[creditos$PAY_AMT4>9570.6]<-9570.6
creditos$PAY_AMT5[creditos$PAY_AMT5<0]<-0
creditos$PAY_AMT5[creditos$PAY_AMT5>9500]<-9500
creditos$PAY_AMT6[creditos$PAY_AMT6<0]<-0
creditos$PAY_AMT6[creditos$PAY_AMT6>9600]<-9600

#Transformare las variables cualitativas, que en principio el programa las registra como cuantitativas
creditos$SEX<-as.factor(creditos$SEX)
creditos$EDUCATION<-as.factor(creditos$EDUCATION)
creditos$MARRIAGE<-as.factor(creditos$MARRIAGE)
creditos$PAY_0<-as.factor(creditos$PAY_0)
creditos$PAY_2<-as.factor(creditos$PAY_2)
creditos$PAY_3<-as.factor(creditos$PAY_3)
creditos$PAY_4<-as.factor(creditos$PAY_4)
creditos$PAY_5<-as.factor(creditos$PAY_5)
creditos$PAY_6<-as.factor(creditos$PAY_6)
creditos$default_payment_next_month<-as.factor(creditos$default_payment_next_month)


#Muestra train y test
install.packages("caret")
library(caret)

sub<-createDataPartition(y=creditos$default_payment_next_month, p=0.7, list=FALSE)
train <- creditos[sub,]
test  <- creditos[-sub,]

train%>%group_by(default_payment_next_month)%>%summarise(conteo=n())
test%>%group_by(default_payment_next_month)%>%summarise(conteo=n())
#analisis de los VIF oara detectar multicolinealidad
faraway::vif(select_if(creditos, is.numeric))
#VIF mayores a 10, significan problemas graves de multilinealidad

#creando los modelos de regresion logistica
modelo_nulo<-glm(default_payment_next_month~1, data=train, family=binomial)
modelo_nulo
modelo_full<-glm(default_payment_next_month ~ ., data=train, family=binomial)
modelo_full

#aplicando procesos de seleccion de variables para escoger el mejor modelo
step(modelo_nulo, scope=list(upper=modelo_full), data=train, direction='both')



#Construyendo el modelo logistico
modelo=glm(formula=default_payment_next_month~LIMIT_BAL+EDUCATION+SEX+MARRIAGE+AGE+PAY_0+PAY_4+PAY_3+PAY_5+PAY_6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT5+PAY_AMT6+BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4, family=binomial, data=train)

#Se excluyeron las variables BILL_AMT por ocasionar graves problemas de multicolinealidad
confint(modelo)

#Instalando paquete lmtest para aplicar test de lemeshow
install.packages("lmtest")
library(lmtest)
library(ResourceSelection)
hoslem.test(modelo$y, fitted(modelo))

#Apoyandonos en el enfoque de devianza
modelo$null.deviance
modelo$deviance
#La devianza del modelo nulo es superior al modelo con variables predictoras,
#por lo tanto, el modelo con predictores es mas preciso

install.packages("ROCR")
library(ROCR)
install.packages("ggplot2")
library(ggplot2)
pred<-predict(modelo, newdata=test, type="response")
roc_pred<-prediction(pred, test$default_payment_next_month)
roc_perf = performance(roc_pred, measure = "tpr", x.measure = "fpr")
plot(roc_perf,
     colorize = TRUE,
     text.adj = c(-0.2,1.7),
     print.cutoffs.at = seq(0,1,0.1))
abline(a=0,b=1,col="red")
auc = performance(roc_pred, measure = "auc")
auc = auc@y.values[[1]]
auc

#Matriz de confusion
install.packages("ISLR")
library(ISLR)
install.packages("MASS")
library(MASS)

install.packages("caret")
library(caret)
pred<-ifelse(pred<0.5,0,1)
confusionMatrix(as.factor(pred), as.factor(test$default_payment_next_month))

#Modelo con presicion de 81.35%, con 97.36% de sensibilidad se le escapa solo 2.64% de los verdaderos positivos
#especificidad del 25% 
#segun el valor del est de Mc-Nemann, se debe rechazar H0, es decir, el hecho que el cliente incumpla en el pago
#no estara sujeto al azar



