
#### Model Evaluation based on RMSE & RSquared
Model_RMSE<-sapply(list(Model_lm, Model_Rand, Model_tree, Model_forest, Model_gbm, Model_boost), 
                   function(x) {postResample(predict(x, Train), Train$SalePrice)})%>%
  t()%>%data.frame()%>% mutate(Category = c("LM", "SVR", "DTree", "Forest", "GBM","XGBoost"))
Model_RMSE$Rank[order(Model_RMSE$RMSE)] <- 1:6


# Model Comparison Visualization with Historical data
Result_Data<-cbind(Train,
                   Pricing_Pred(newdata = Train, 
                                Model_lm = Model_lm, 
                                Model_SVR = Model_Rand, 
                                Model_tree = Model_tree, 
                                Model_forest =  Model_forest,
                                Model_gbm = Model_gbm, 
                                Model_boost = Model_boost))

## Plotting predicted results with different regression model in same plot
ggplot(Result_Data, aes(x=OverallQual))+
  geom_point(aes(y=SalePrice,colour="SalePrice"))+   
  #geom_point(aes(y=Pred.Share_LM,colour="LM" ))+    
  #geom_point(aes(y=Pred.Share_SVR, colour="SVR" ))+    
  #geom_point(aes(y=Pred.Share_Tree,colour="Tree" ))+   
  geom_point(aes(y=Pred.Share_Forest, colour="Forest" ))+    
  #geom_point(aes(y=Pred.Share_gbm,colour="GBM" ))+    
  geom_point(aes(y=Pred.Share_boost, colour="XGboost" ))+    
  scale_colour_manual("",
                      breaks = c("SalePrice", "LM","SVR", "Tree","Forest","GBM","XGboost"), 
                      values = c("blue","green", "red","yellow","purple","black","orange"),
                      labels = c("Observed Value","PredShare (LM)","PredShare (SVR)","PredShare (Tree)",
                                 "PredShare (RandomForest)","PredShare (GBM)","PredShare (XGboost)")) +
  ggtitle('Sale Price Observed Values VS Predicted Values')+
  ylab('Sale Price')+
  xlab('OverallQual')+
  theme(legend.position="bottom")
  theme_bw()
  
  
  
  
  
  
  
  