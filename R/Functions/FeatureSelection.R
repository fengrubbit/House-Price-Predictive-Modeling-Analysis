

FeatureSelection<-function(DataUse = Train,
                           TargetVariable = "SalePrice",
                           FindZero = TRUE,
                           RMLinear = TRUE,
                           FdImportant = FALSE,
                           FTSelection = TRUE,
                           RMRedundant = TRUE){
  
  reduced_data <- Train
  require(caret)
  # Find zeor- and near zero-Variance Predictors
  if(FindZero){
    nzv <- nearZeroVar(reduced_data, saveMetrics = TRUE)
    reduced_data <- reduced_data[,c(rownames(nzv[!(nzv$nzv),]))]
  }
  
  # Remove Linear Dependencies
  if(RMLinear){
    comboInfo <- findLinearCombos(reduced_data)
    if(isTRUE(comboInfo)){
      reduced_data<-reduced_data[,-comboInfo$remove]
    }
    
  }
  
  
  # Find Important Variable
  if(FdImportant){
    control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    model <- train(x = reduced_data[,which(colnames(reduced_data)!=TargetVariable)],
                   y = reduced_data[[TargetVariable]],
                   method = "xgbTree",
                   preProcess = "scale",
                   trControl = control)
    importance <- varImp(model, scale = FALSE)
    print(importance)
    ImportancePlot <- plot(importance)
    reduced_data<-reduced_data[,c(rownames(data.frame(importance$importance))[1:20], TargetVariable)]
  } else {ImportancePlot <- NULL}
  
  
  # Feature Selection
  if (FTSelection){
    require(mlbench)
    require(caret)
    control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
    model <- rfe(x = reduced_data[,which(colnames(reduced_data)!=TargetVariable)],
                 y = reduced_data[[TargetVariable]],
                 sizes = c(1:(length(reduced_data)-2)),
                 rfeControl = control)
    print(model)
    FSPlot <- plot(model, type=c("g","o"))
    
    reduced_data <- reduced_data[,c(model$optVariables, TargetVariable)]
  }else{FSPlot <- NULL}
  
  
  # Remove Redundant Feature
  if(RMRedundant){
    require(corrplot)
    #require(reshape2)
    correlationMatrix <- cor(reduced_data[,which(colnames(reduced_data)!=TargetVariable)])
    print(correlationMatrix)
    #correlationMatrix%>%melt()%>%filter(value>0.90)%>%droplevels()%>%ggplot(aes(x=Var1, y=Var2, fill=value))+geom_tile()
    RMRedPlot <- correlationMatrix%>%corrplot(method="number")
    highlyCorrelated <- sort(findCorrelation(correlationMatrix, cutoff = 0.75))
    print(highlyCorrelated)
    reduced_data<-reduced_data[,-c(highlyCorrelated)]
  }else{RMRedPlot <- NULL}
  
  results <- list(reduced_data, ImportancePlot, FSPlot, RMRedundant)
  
  return(results)
}

#a<- FeatureSelection()
