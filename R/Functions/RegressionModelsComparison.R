
######Prediction Function
Pricing_Pred<-function(newdata,Model_lm, Model_SVR, Model_tree, Model_forest, Model_gbm, Model_boost){
  Pred_lm<-predict(Model_lm, newdata, interval="predict")
  Pred_Rand<-predict(Model_SVR, newdata)
  Pred_Tree<-predict(Model_tree, newdata)
  Pred_Forest<-predict(Model_forest,newdata)
  Pred_gbm<-predict(Model_gbm,newdata)
  Pred_boost<-predict(Model_boost,newdata)
  
  Pred_Target<-data.frame(cbind(Pred_lm,  Pred_Rand, Pred_Tree,Pred_Forest,  Pred_gbm, Pred_boost))
  
  names(Pred_Target)<-c("Pred.Share_LM","Pred.Share_LM_lwr","Pred.Share_LM_upr", 
                        "Pred.Share_SVR","Pred.Share_Tree",
                        "Pred.Share_Forest","Pred.Share_gbm","Pred.Share_boost")
  
  return(Pred_Target)
}


# Predictive Model Comparison Plot
PredModel_Comparison_line<-function(dataset= FOL_Core_Pred, 
                                    ChangeVariable = FOL_Core_Pred$FOL_Core_APR,
                                    Model=c("LM","SVR","Tree","Forest","gbm","boost")){
  if("LM" %in% Model){
    lines(ChangeVariable, dataset$Pred.Share_LM, 
          col="blue", pch=16, lwd=2, lty=1)
  }
  if("SVR" %in% Model){
    lines(ChangeVariable, dataset$Pred.Share_SVR,
          col="skyblue", pch=16, lwd=2, lty=1)
  }
  if("Tree" %in% Model){
    lines(ChangeVariable, dataset$Pred.Share_Tree,
          col="yellow", pch=16, lwd=2, lty=1)
  }
  if("Forest" %in% Model){
    lines(ChangeVariable, dataset$Pred.Share_Forest,
          col="green3", pch=16, lwd=2,lty=1)
  }
  if("gbm" %in% Model){
    lines(ChangeVariable, dataset$Pred.Share_gbm,
          col="black", pch=16, lwd=2,lty=1)
  }
  if("boost" %in% Model){
    lines(ChangeVariable, dataset$Pred.Share_boost,
          col="purple", pch=16, lwd=2,lty=1)
  }
}
       