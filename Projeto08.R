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

#Iniciando com a carga dos datasets tanto de treino quanto de teste

dataset <- read.csv("projeto8-training.csv")
View(dataset)

sum(is.na(dataset))
#Dataset não possui valores missing, o que é ótimo

colnames(dataset)

testDataset <- read.csv('projeto8-testing.csv')
View(testDataset)

sum(is.na(testDataset))
#tanto no dataset de treino quanto no dataset de teste não existem valores missing

#Irei trabalhar com uma cópia do dataset orignal
datasetCopy <- dataset

summary(datasetCopy)


#Iniciarei traçando um gráfico de correlação entre as variáveis
#Inicialmente utilizando apenas as variáveis numéricas antes de realizar qualquer transformação nas variáveis categóricas
corr <- cor(datasetCopy[,2:30])

corrplot(corr)

summary(datasetCopy$NSM)
summary(datasetCopy$T_out)

summary(datasetCopy$Appliances)

#Como projeto se trata de prever o gasto com eletrodomésticos irei iniciar veriricando se não existem outliers
quantile(datasetCopy$Appliances, probs = c(0.1,0.5,0.75,0.90,0.95))


#O boxplot nos revela que a variável target "appliances" possui uma quantidade razoável de outliers
# a depender do algoritmo utilizado precisaremos ou removê-los ou substituítilos pela mediana/média
boxplot(datasetCopy[,2:29])


#algumas colunas são categóricas
#Assim irei fazer com que se tornem numéricas

#Começando pela coluna WeekStatus, que proporciona a informação de ser final de semana ou não
unique(datasetCopy$WeekStatus)
#Irei transformá-la em uma variável numérica
datasetCopy <- datasetCopy%>%mutate(weekend = if_else(WeekStatus == "Weekday",1,0))

unique(datasetCopy$Day_of_week)

#o próximo passo é converter a variável do dia da semana também em variável numérica
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


datasetCopy$diaNumero = sapply(datasetCopy$Day_of_week, convertWeekday)

#Creio que as variáveis categóricas da forma como estavam não iriam contribuir muito para um eventual modelo preditivo
#Dessa forma irei transformar as variáveis de data em horas e meses com o seguinte intuito
#Verificar se há alguma relação da hora do dia com com o consumo de eletricidade
#Verificar se há alguma relação da estação do ano com o consumo de eletricidade


datasetCopy$hora <- hour(datasetCopy$date)
datasetCopy$month <- month(datasetCopy$date)


#Excluindo as variáveis que já foram transformadas
datasetCopy$date = NULL
datasetCopy$WeekStatus = NULL
datasetCopy$Day_of_week = NULL

View(datasetCopy)

str(datasetCopy)

#Agora tendo todas as variáveis como numéricas é mais fácil para visualizar a correlação entre elas utilizando um plot de correlação
corr <- cor(datasetCopy)

corrplot(corr)


#Até o ponto atual, apesar de ter encontrado boa parte do significado das variáveis uma delas ainda não sei o que significa, temos:
#Appliances ==> Eletrodomésticos, lights ==> luzes, T1..T9 ==> Temperaturas, RH_1..RH_9 ==> Humidades relativas, T_out/RH_out ==> Temperatura e humidade do lado de fora
#Press_mm_hg ==> pressão atmoférica, Windspeed ==> Velocidade do vento, Visibility ==> Visibilidade, Tdewpoint ==> Ponto de orvalho (condensação), NSM ==> Variável desconhecida
#weekend ==> Fim de semana, diaNumero ==> dia da semana em número para facilitar e entregar ao modelo (variável criada), hora ==> hora do dia (variável criada)
#Month ==> mês (variável criada)
#Apenas pelo nome das variáveis é possível supor (embora não se tome decisões baseadas em suposições), que algumas das variáveis não terão relação com consumo de energia
#Usaremos aprendizado de máquina para inferir quais variáveis são mais significantes.

#Mas antes vamos realizar uma breve análise exploratória afim de encontrar alguns insights


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
#são altamente correlacionados. Os dois gráficos são muito parecidos

#Também podemos tentar verificar se o consumo dos eletrodomésticos no final de semana é maior que durante o final de semana
#Com o seguinte comando obtemos a informação de que sim
dataset%>%group_by(WeekStatus)%>%summarise(media = mean(Appliances))

#########################################################################################################################################################
#################################################Aplicando machine Learning para seleção de variáveis####################################################
#########################################################################################################################################################


colnames(datasetCopy)
#O Random forest pode ser utilizado tanto para tarefas de regressão quanto para classificação
#Também é muito útil para descobrir quanto cada uma das variáveis é relevante para o modelo, além de poder ser utilizado inclusive para aprendizagem não-supervisionada


us.rf <- randomForest(Appliances~., data = datasetCopy, importance = TRUE)

varImpPlot(us.rf)

#De acordo com o randomForest as variáveis que menos contribuem com o modelo seriam = 
#diaNumero, Visibility, weekend, month, rv1, rv2, que deverão ser retiradas do dataset

variaveisNaoPred <- c('diaNumero','month','weekend','rv1','rv2','Visibility')

varianaoPred2 <- c('weekend', 'month', 'RH_out','rv1','rv2')


datasetReduzido <- datasetCopy%>%select(-varianaoPred2)

View(datasetReduzido)

length(colnames(datasetReduzido))

#alguns dos métodos que usarei precisam de normalização, assim sendo já irei aplicá-la de maneira a permitir que os métodos funcionem bem

datasetReduzido[2:28]<- as.data.frame(scale(datasetReduzido[2:28]))


#podemos visualizar novamente a matriz de correlação antes de iniciar a aplicação de machine learning
corr <- cor(datasetReduzido)
corrplot(corr)

#Aqui iniciarei a criação de vários modelos
#Entretanto para facilitar a transformação dos datasets da maneira como desejo criei uma função que transforma o dataset original exatamente no formato que preciso
#Dessa forma posso aplicá-la tanto ao dataset de teste quanto de treino

source("Tools.R")

rawData <- read.csv("projeto8-training.csv")
rawTestData <- read.csv("projeto8-testing.csv")

train<- convert.Dataset(rawData)
test <- convert.Dataset(rawTestData)

#Iniciar os testes utilizando regressão linear
modeloLM <- lm(Appliances ~., train)

predLM <- predict(modeloLM, test[-1])

RMSE(predLM, test$Appliances)

MAE(predLM, test$Appliances)

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

saveRDS(modeloXGBTree,"modeloXGBTree.RDS")

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

#o seguinte comando cria diversos modelos, os treina e calcula a acurácia deles, retornando o melhor de todos utilizando
#Como métrica o RMSE, esta função é muito intensiva computacionalmente e pode levar várias horas para ser executada

source("Tools.R")
melhorModelo <- analyzeModels(c("lm","glm","rpart"),train,test,1)
#Em minha análise o modelo rpart alcançou uma acurácia medida pelo RMSE de 95%
#Quero ver se a remoção de alguns outliers do dataset de treino pode ajudar nessa acurácia

#95% dos exemplos no dataset de treino estão abaixo de 330
quantile(train$Appliances,c(0.95))

#o que farei ==>
# Copiar o dataset de treino => Identificar quais exemplos pertencem aos 5% maiores (outliers) ==> Substitui-los pela media

trainCopy <- train
mediaAppliances <- mean(trainCopy$Appliances)
medianaAppliances <- median(trainCopy$Appliances)


quantile95 <- quantile(train$Appliances,c(0.95))

for (row in c(1:length(trainCopy$Appliances))){

  if(trainCopy[row,1]>quantile95){
    trainCopy[row,1]<- mediaAppliances
  }
}

quantile(trainCopy$Appliances,c(0.95))

boxplot(trainCopy$Appliances)

melhorModelo <- analyzeModels(c("lm","rpart","xgbTree"),trainCopy,test,1)

#após ajustar e substituir os outliers pela média a performance do modelo não melhorou, pelo menos não
#com os modelos pesquisados
#o modelo com o menor erro avaliado pelo RMSE continua sendo o XGBoost antes da atualização dos outliers


predMelhorModelo <- predict(melhorModelo,test[-1])

RMSE(predMelhorModelo, test$Appliances)

saveRDS(melhorModelo,file = "melhorModelo.rds")




