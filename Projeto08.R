getwd()
setwd("C:/FCD/MachineLearning/Cap20/Proj08")

carregarPacotes <- function(){
  library(patchwork)
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(corrplot)
  library(randomForest)
  library(caret)
}

carregarPacotes()

dataset <- read.csv("projeto8-training.csv")
View(dataset)

sum(is.na(dataset))
#Dataset não possui valores missing, o que é ótimo

colnames(dataset)

testDataset <- read.csv('projeto8-testing.csv')
View(testDataset)

colnames(testDataset)

dfColnames = data.frame(colnames(dataset))

write.csv(dfColnames,file = "colunas.csv")

datasetCopy <- dataset

summary(datasetCopy)

corr <- cor(datasetCopy[,2:30])

corrplot(corr)

View(datasetCopy)

corr <- cor(datasetCopy[,2:28])
corrplot(corr)

summary(datasetCopy$NSM)
summary(datasetCopy$T_out)

#algumas colunas são categóricas
#Assim irei fazer com que se tornem numéricas

unique(datasetCopy$WeekStatus)

datasetCopy <- datasetCopy%>%mutate(weekend = if_else(WeekStatus == "Weekday",1,0))

unique(datasetCopy$Day_of_week)

convertWeekday <- function(dia){
  diaRet = ''
  if (dia == "Sunday"){
    diaRet = 1
  }else if (dia == "Monday"){
    diaRet = 2
  }else if (dia == "Tuesday"){
    diaRet = 3
  }else if (dia == "Wednesday"){
    diaRet = 4
  }else if (dia == "Thursday"){
    diaRet = 5
  }else if (dia == "Friday"){
    diaRet = 6
  }else if (dia == "Saturday"){
    diaRet = 7
  }
  return(as.numeric(diaRet))
}

View(datasetCopy)

datasetCopy <-datasetCopy%>%mutate(diaNumero = convertWeekday(Day_of_week))

datasetCopy$diaNumero <- convertWeekday(datasetCopy$Day_of_week)

datasetCopy$diaNumero = lapply(datasetCopy$Day_of_week,convertWeekday)


#Creio que as variáveis categóricas da forma como estavam não iriam contribuir muito para um eventual modelo preditivo
#Dessa forma irei transformar as variáveis de data em horas e meses com o seguinte intuito
#Verificar se há alguma relação da hora do dia com com o consumo de eletricidade
#Verificar se há alguma relação da estação do ano com o consumo de eletricidade


datasetCopy$hora <- hour(datasetCopy$date)
datasetCopy$month <- month(datasetCopy$date)

for (n in c(1:length(datasetCopy$diaNumero))){
  datasetCopy[n,31] = convertWeekday(dataset[n,34])
}

#Excluindo as variáveis que já foram transformadas
datasetCopy$date = NULL
datasetCopy$WeekStatus = NULL
datasetCopy$Day_of_week = NULL

View(datasetCopy)

datasetCopy$diaNumero<-as.numeric(datasetCopy$diaNumero)

corr <- cor(datasetCopy)

corrplot(corr)

#Até o ponto atual, apesar de ter encontrado boa parte do significado das variáveis uma delas ainda não sei o que significa, temos:
#Appliances ==> Eletrodomésticos, lights ==> luzes, T1..T9 ==> Temperaturas, RH_1..RH_9 ==> Humidades relativas, T_out/RH_out ==> Temperatura e humidade do lado de fora
#Press_mm_hg ==> pressão atmoférica, Windspeed ==> Velocidade do vento, Visibility ==> Visibilidade, Tdewpoint ==> Ponto de orvalho (condensação), NSM ==> ainda desconhecido
#weekend ==> Fim de semana, diaNumero ==> dia da semana em número para facilitar e entregar ao modelo (variável criada), hora ==> hora do dia (variável criada)
#Month ==> mês (variável criada)
#Apenas pelo nome das variáveis é possível supor (embora não se tome decisões baseadas em suposições), que algumas das variáveis não terão relação com consumo de energia
#Usaremos aprendizado de máquina para inferir quais variáveis são mais significantes.

byHour <- datasetCopy%>%group_by(hora)

byHour%>%summarise(
  lights = mean(lights)
)
View(byHour)

luzPorHora <- datasetCopy%>%group_by(hora)%>%summarise(mediaLuz = mean(lights))

View(luzPorHora)

eletroPorHora <- datasetCopy%>%group_by(hora)%>%summarise(mediaEletros = mean(Appliances))

grafLuz = ggplot(data = luzPorHora, mapping = aes(hora,mediaLuz))+
  geom_point()+
  labs(title = "luz por hora", x = 'Hora do dia', y = "Consumo de luz")

grafEletros = ggplot(data = eletroPorHora, mapping = aes(hora,mediaEletros))+
  geom_point()+
  labs(title = "Eletrodomesticos por hora", x = "hora do dia",y = "Consumo eletros")

grafLuz+grafEletros

#Ao analisar os gráficos lado a lado podemos chegar facilmente a uma correlação entre o consumo de luz com o de eletrodomésticos em função da hora do dia


#########################################################################################################################################################
#################################################Aplicando machine Learning para seleção de variáveis####################################################
#########################################################################################################################################################


#Iniciarei utilizando o randomForest no modo não-supervisionado, conforme é colocado abaixo:
#https://horvath.genetics.ucla.edu/html/publications/FullManuscriptRFclustering1_final.pdf
colnames(datasetCopy)
us.rf <- randomForest(Appliances~., data = datasetCopy, importance = TRUE)

varImpPlot(us.rf)

#De acordo com o randomForest as variáveis que menos contribuem com o modelo seriam
#diaNumero, Visibility, weekend, month, rv1, rv2, que deverão ser retiradas do dataset

variaveisNaoPred <- c('diaNumero','month','weekend','rv1','rv2','Visibility')

datasetReduzido <- datasetCopy%>%select(-variaveisNaoPred)

View(datasetReduzido)

#alguns dos métodos que usarei precisam de normalização, assim sendo já irei aplicá-la de maneira a permitir que os métodos funcionem bem

datasetReduzido[2:27]<- as.data.frame(scale(datasetReduzido[2:27]))

View(datasetReduzido)

#podemos visualizar novamente a matriz de correlação antes de iniciar a aplicação de machine learning
corr <- cor(datasetReduzido)
corrplot(corr)

#Aqui iniciarei a criação de vários modelos
#Entretanto para facilitar a transformação dos modelos criei uma função que transforma o dataset original exatamente no formato que precise

source("Tools.R")

rawData <- read.csv("projeto8-training.csv")
rawTestData <- read.csv("projeto8-testing.csv")

train<- convert.Dataset(rawData)
test <- convert.Dataset(rawTestData)

#Iniciar os testes utilizando regressão linear
modeloLM <- lm(Appliances ~., train)

predLM <- predict(modeloLM, test[-1])

RMSE(predLM, test$Appliances)

saveRDS(modeloLM, file = "modeloLM.RDS")

#Verificando a performance utilizando o random Forest
modeloRF <- randomForest(Appliances ~., train)

modeloRF <- readRDS("modeloRF.RDS")

predRF <- predict(modeloRF, test[-1])

RMSE(predRF,test$Appliances)

saveRDS(modeloRF,"modeloRF.RDS")

#SVM

#cuidado
modeloSVM <- caret::train(x = train[-1], y = train$Appliances, method = 'svmRadial')

predSVM <- predict(modeloSVM,test[-1])

RMSE(predSVM, test$Appliances)

#XGBData
modeloXGBTree <- caret::train(x = train[-1], y= train$Appliances, method = "xgbTree")

modeloXGBTree <- readRDS("modeloXGBTree.RDS")

predXGB <- predict(modeloXGBTree, test[-1])

RMSE(predXGB,test$Appliances)

saveRDS(modeloXGBTree,"modeloXGBTree.RDS")

#Ridge
modeloRidge <- caret::train(x = train[-1], y= train$Appliances, method = "ridge")

modeloRidge <- readRDS("modeloRidge.RDS")

predRidge <- predict(modeloRidge, test[-1])

RMSE(predRidge,test$Appliances)

saveRDS(modeloRidge,"modeloRidge.RDS")

source("Tools.R")

#o seguinte comando cria diversos modelos, os treina e calcula a acurácia deles, retornando o melhor de todos utilizando
#Como métrica o RMSE, esta função é muito intensiva computacionalmente e pode levar várias horas para ser executada
#

melhorModelo <- analyzeModels(c("lm","glm","rpart"),train,test,1)














