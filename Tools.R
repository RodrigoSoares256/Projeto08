require(dplyr)
require(caret)

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

convert.Dataset <- function(RawData){
  
  
  datasetCopy <- RawData%>%mutate(weekend = if_else(WeekStatus == "Weekday",1,0))
  
  datasetCopy$hora <- hour(RawData$date)
  datasetCopy$month <- month(RawData$date)
  datasetCopy$diaNumero = sapply(datasetCopy$Day_of_week, convertWeekday)

  varsExcluir <-  c('weekend', 'month', 'RH_out','rv1','rv2',"date","Day_of_week","WeekStatus")
  
  datasetCopy <- datasetCopy%>%select(-varsExcluir)

  datasetCopy[2:length(datasetCopy)]<- as.data.frame(scale(datasetCopy[2:length(datasetCopy)]))
  
  
  return(datasetCopy)
}

analyzeModels <- function(models,trainDataset,testDataset,colunaTarget){
  #Essa função tem por objetivo o teste automático de vários modelos de machine learning através do pacote caret
  #e retornar aquele que apresenta a melhor acurácia de acordo com a métrica RMSE
  
  melhorModelo <- ''
  melhorAcuracia <- 0.0
  
  for (modelo in models){
    print(paste("Realizando treinamento com modelo ", modelo))
    modeloAtual <- caret::train(x = trainDataset[-colunaTarget],y = trainDataset[,colunaTarget], method = modelo)
    #O próximo passo é realizar predicoes com o modelo criado
    predModelo <- predict(modeloAtual,testDataset[-colunaTarget])
    rmseModelo <- RMSE(predModelo,testDataset[,colunaTarget])
    if (rmseModelo>melhorAcuracia){
      melhorAcuracia <- rmseModelo
      melhorModelo <- modelo
      modeloRetorno <- modeloAtual
    }
    print(paste("Acuracia do modelo ", modelo," = ",rmseModelo))
    print(paste(" melhor modelo ", melhorModelo, " com acuracia de ", melhorAcuracia))
  }
  return(modeloRetorno)
}
