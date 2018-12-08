# R-Tarea-5
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("stargazer")
install.packages("readr")
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stargazer)
library(readr)
MSFT <- read_csv("MSFT.csv")
microsoft_precio <- read.table("MSFT.csv", sep = ",", head = TRUE)
n <- length(microsoft_precio$Adj.Close)

for(i in 1:n){
  microsoft_precio[i+1,8] = log(microsoft_precio[i+1,7]/microsoft_precio[i,7])
  if(i <= 2){microsoft_precio[i+1,9] = microsoft_precio[i+1,8] + microsoft_precio[i,8]}
  else{microsoft_precio[i+1,9] = microsoft_precio[i,9] + microsoft_precio[i+1,8]}
}

microsoft_precio = microsoft_precio %>% rename(retornos = "V8")
microsoft_precio = microsoft_precio %>% rename(retornos_acum = "V9")
retornosMSFT <- microsoft_precio[3:226,]
r <- length(retornosMSFT$retornos)
sum(retornosMSFT$retornos)

retornosMSFT %>% ggplot()+
  geom_line(aes(x=Date, y=retornos), linetype = "solid", color = "black", size=0.5) +
  xlab("Fecha retornos (mensual)") + ylab("retorno accion") +
  theme_economist_white()

retornosMSFT %>% ggplot()+
  geom_line(aes(x=Date, y=retornos_acum), linetype = "solid", color = "red", size=0.5) +
  xlab("Fecha retornos (mensual)") + ylab("retorno accion") +
  theme_economist_white()

m1 <- sum(retornosMSFT$retornos)/r #media aritmetica
m2 <- sum((retornosMSFT$retornos - m1)^2) #usado en ambos denominadores 
m3 <- sum((retornosMSFT$retornos - m1)^3) #numerador del coeficiente de asimetria 
m4 <- sum((retornosMSFT$retornos - m1)^4) #numerador de kurtosis 
sm1 <- (m3/m2^(3/2))^2 #calculo coeficiente de asimetria 
sm2 <- (m4/m2^2) #calculo kurtosis 
JB <- r*sm1/6+(sm2-3)^2/24
test_chi <- 1 - pchisq(JB, df = 2)
#H_0: los retornos son normales 
#H_1: los retornos no son normales 
if(JB > test_chi){"Se rechaza la hipotesis nula"}else{"No se rechaza la hipotesis nula"}


#Retornos Apple (AAPL)
apple_precio <- read.table("AAPL.csv", sep = ",", head = TRUE)
a <- length(apple_precio$Adj.Close)

for(i in 1:a){
  apple_precio[i+1,8] = log(apple_precio[i+1,7]/apple_precio[i,7])
  if(i <= 2){apple_precio[i+1,9] = apple_precio[i+1,8] + apple_precio[i,8]}
  else{apple_precio[i+1,9] = apple_precio[i,9] + apple_precio[i+1,8]}
}

apple_precio = apple_precio %>% rename(retornos = "V8")
apple_precio = apple_precio %>% rename(retornos_acum = "V9")
retornosAAPL <- apple_precio[3:226,]
p <- length(retornosAAPL$retornos)
sum(retornosAAPL$retornos)

retornosAAPL %>% ggplot()+
  geom_line(aes(x=date, y=retornos), linetype = "solid", color = "black", size=0.5) +
  xlab("Fecha retornos (mensual)") + ylab("retorno accion") +
  theme_economist_white()

retornosAAPL %>% ggplot()+
  geom_line(aes(x=date, y=retornos_acum), linetype = "solid", color = "green", size=0.5) +
  xlab("Fecha retornos (mensual)") + ylab("retorno accion") +
  theme_economist_white()

a1 <- sum(retornosAAPL$retornos)/p #media aritmetica
a2 <- sum((retornosAAPL$retornos - a1)^2) #usado en ambos denominadores 
a3 <- sum((retornosAAPL$retornos - a1)^3) #numerador del coeficiente de asimetria 
a4 <- sum((retornosAAPL$retornos - a1)^4) #numerador de kurtosis 
sa1 <- (a3/a2^(3/2))^2 #calculo coeficiente de asimetria 
sa2 <- (a4/a2^2) #calculo kurtosis 
JB_a <- p*sa1/6+(sa2-3)^2/24
test_chi_apple <- 1 - pchisq(JB_a, df = 2)
#H_0: los retornos son normales 
#H_1: los retornos no son normales 
if(JB_a > test_chi_apple){"Se rechaza la hipotesis nula"}else{"No se rechaza la hipotesis nula"}

