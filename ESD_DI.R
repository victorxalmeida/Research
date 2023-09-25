#install.packages("devtools")
#devtools::install_github("hrbrmstr/AnomalyDetection", dependencies = FALSE)
# install.packages("hrbrthemes")
# install.packages("quantmod")
#install.packages("readxl")
library(quantmod)
library(readxl)

#Lendo Base DI
base_di <- read_excel("BaseDIVar.xlsx")

#Criando DataFrame
YIELD <- data.frame(base_di)

#Retirando NA, colocando zero
YIELD[is.na(YIELD)] <- 0

library(AnomalyDetection)
library(hrbrthemes)

sub_YIELD <- YIELD[,c("Date", "X10Y")]
sub_YIELD$Date <- as.POSIXct(sub_YIELD$Date)

#Transformo data de uma string em um tipo POSIXct.
#Preciso que a máquina aprenda os padrões que existem na base de dados
#Verifico se os padrões existem realmente e se a máquina conseguiu aprende-los em uma base de dados separados
#Fazendo apenas com um pedaço da base para simular umt reinamento com 50% da base

train_size <- dim(YIELD)[1]*0.4 #Pega um pedaço da amostra (50%)
anomalias_YIELD <- ad_ts(sub_YIELD[1:train_size,], max_anoms = 0.05, direction = 'both', alpha = 0.05)
#Rodando função de anomalias para o treinamento de 1 até o "trainsize" (linha de cima) e 5% de confiança. Ambas direções, (alta e baixa).

summary(anomalias_YIELD) #Resumo das variações, onde conseguimos observar a variação máxima
dim(anomalias_YIELD)

library(ggplot2)

ggplot() +
  geom_line(
    data = sub_YIELD, aes(Date, X10Y),
    size = 0.125, color = "blue"
  )  +
  geom_point(
    data = anomalias_YIELD, aes(timestamp, anoms), color = "#cb181d", alpha = 1/3
  ) +
  scale_x_datetime(date_labels = "%b/%y") +
  scale_y_comma()


multiplier <- 0.5
anom_posi <- mean(data.matrix(anomalias_YIELD[anomalias_YIELD$anoms > 0, "anoms"]))*multiplier
anom_neg <- mean(data.matrix(anomalias_YIELD[anomalias_YIELD$anoms < 0, "anoms"]))*multiplier
# Calcular o ponto de corte, toda vez que o retorno for > do que anomalia positiva vou fazer alguma coisa e o inverso pra negativa
# Por isso pegoa a média das anomalias, transforma em matriz pra fazer o calculo certinho por ser média
# A média dividido por 2 pra conseguir ter mais operações
anom_posi
anom_neg

TRAIN_ASSET <- YIELD[1:train_size,]
#Cria uma coluna que chama anoms e atribuo o valor do retorno > anomalia positiva = 1 e toda vez que o valor do retorno for < anom_neg = -1
TRAIN_ASSET$anoms <- ifelse(TRAIN_ASSET$X10Y > anom_posi, 1, 0)
TRAIN_ASSET$anoms <- ifelse(TRAIN_ASSET$X10Y < anom_neg, -1, TRAIN_ASSET$anoms)

sum(TRAIN_ASSET$anoms != 0)

#Criando o alvo: tenho valor de hoje e quero saber o que vai ocorrer amanhã
desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

TRAIN_ASSET$Alvo <- desloca(TRAIN_ASSET$X10Y, 1)
TRAIN_ASSET[is.na(TRAIN_ASSET)] <- 0
#funçao que faz o deslocamento dos retornos em uma casa (n) e x o valor que vou deslocar.
#No vetor pego a sequencia da quantidade e faço a repetição do NA

#Averiguando resultado
trend <- -1 #1 momentum -1 retorno a média, essa variável serve para ensinar o modelo, no caso de ser manual, o modelo não sabia se era momentum ou retorno a média

TRAIN_ASSET$X10Y <- TRAIN_ASSET$anoms*TRAIN_ASSET$Alvo*trend

retorno_modelo_acumulado_TRAIN <- cumsum(TRAIN_ASSET$X10Y)

plot(retorno_modelo_acumulado_TRAIN, type = "l", col = "red", lwd = 2,
     main = "Retorno do Trading System Anomalia - Treinamento",
     xlab = "Periodo",
     ylab = "Retorno em %")


## TRADE SYSTEM

TEST_ASSET <- YIELD[(train_size+1):dim(YIELD)[1],]
TEST_ASSET$anoms <- ifelse(TEST_ASSET$X10Y > anom_posi, 1, 0)
TEST_ASSET$anoms <- ifelse(TEST_ASSET$X10Y < anom_neg, -1, TEST_ASSET$anoms)

sum(TEST_ASSET$anoms != 0)

#Criando o alvo
TEST_ASSET$Alvo <- desloca(TEST_ASSET$X10Y, 1)
TEST_ASSET[is.na(TEST_ASSET)] <- 0

#Averiguando resultados
trend <- -1
TEST_ASSET$Resultado <- TEST_ASSET$anoms*TEST_ASSET$Alvo*trend
retorno_modelo_acumulado <- cumsum(TEST_ASSET$Resultado)

plot(retorno_modelo_acumulado, type = "l", col = "red", lwd = 2,
     main = "Retorno do Trading System Anomalia - Test",
     xlab = "Periodo",
     ylab = "Retorno em %")