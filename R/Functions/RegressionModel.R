#=======================================================================================
#
# File:        PredictiveModelTemplate.R
# Author:      Jin Shang
# Description: This code illustrates the usage grid search of the caret package 
#              for finding the optimal predictive model with different machine learning methods
# Detail:      Possible machine learning methods include: Support Vector Regression Model(SVR),
#             BayesianOptimization for SVR,
#              
#=======================================================================================


# Grid Search for SVR Model
SVR_Tune<-function(formula, data){
  # Set up caret to perform 10-fold cross validation repeated 10
  # times and to use random search for optimal model hyperparamter
  # values.
  train_control <- trainControl(method = "repeatedcv", 
                                number = 10, #default
                                repeats = 10, 
                                search = "random")
  
  set.seed(911) 
  rand_search <- train(formula, data,
                       method = "svmRadial",
                       ## Create 20 random parameter values
                       tuneLength = 20,
                       metric = "RMSE",
                       preProc = c("center", "scale"),
                       trControl = train_control)
  #getTrainPerf(rand_search)
  
  # Rand_search Visualization
  #ggplot(rand_search) + scale_x_log10() + scale_y_log10()+theme_bw()
  Rand_Plot<-ggplot(data.frame(rand_search$results))+
    geom_point(aes(x=sigma,y=C,size=RMSE, color=Rsquared))+
    scale_x_log10() + scale_y_log10()+
    labs(color='R Square')+
    xlab('Sigma')+
    ylab('Cost')+
    theme_bw()
  
  set.seed(912)
  train_control <- trainControl(method = "repeatedcv", 
                                repeats = 5)
  final_search_rand<-train(formula, data,
                           method = "svmRadial",
                           tuneGrid = data.frame(C =rand_search$bestTune$C , 
                                                 sigma = rand_search$bestTune$sigma),
                           metric = "RMSE",
                           preProc = c("center", "scale"),
                           trControl = train_control)
  
  #return(list(Rand_Plot,final_search_rand))
  return(final_search_rand)
}


# Bayesian Optimization for SVR Model
# Rand_Search<-SVR_Tune(formula, data)
Bayesian_Search<-function(formula, data, Rand_Search){
  ## Define the resampling method
  train_control <- trainControl(method = "repeatedcv", 
                                repeats = 5)
  
  ## Use this function to optimize the model. The two parameters are 
  ## evaluated on the log scale given their range and scope. 
  svm_fit_bayes <- function(logC, logSigma) {
    ## Use the same model code but for a single (C, sigma) pair. 
    txt <- capture.output(
      mod <- train(formula, data,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   metric = "RMSE",
                   trControl = train_control,
                   tuneGrid = data.frame(C = exp(logC), sigma = exp(logSigma)))
    )
    ## The function wants to _maximize_ the outcome so we return 
    ## the negative of the resampled RMSE value. `Pred` can be used
    ## to return predicted values but we'll avoid that and use zero
    list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
  }
  
  ## Define the bounds of the search. 
  lower_bounds <- c(logC = -5, logSigma = -9)
  upper_bounds <- c(logC = 20, logSigma = -0.75)
  bounds <- list(logC = c(lower_bounds[1], upper_bounds[1]),
                 logSigma = c(lower_bounds[2], upper_bounds[2]))
  
  ## Create a grid of values as the input into the BO code
  initial_grid <- Rand_Search$results[, c("C", "sigma", "RMSE")]
  initial_grid$C <- log(initial_grid$C)
  initial_grid$sigma <- log(initial_grid$sigma)
  initial_grid$RMSE <- -initial_grid$RMSE
  names(initial_grid) <- c("logC", "logSigma", "Value")
  
  ## Run the optimization with the initial grid and do
  ## 30 iterations. We will choose new parameter values
  ## using the upper confidence bound using 1 std. dev. 
  set.seed(913)
  ba_search <- BayesianOptimization(svm_fit_bayes,
                                    bounds = bounds,
                                    init_grid_dt = initial_grid, 
                                    init_points = 0, 
                                    n_iter = 30,
                                    acq = "ucb", 
                                    kappa = 1, 
                                    eps = 0.0,
                                    verbose = TRUE)
  
  return(ba_search)
}


# Grid Search for XGboost Model
XGboost_Tune<-function(formula, data){
  # Set up caret to perform 10-fold cross validation repeated 3 
  # times and to use a grid search for optimal model hyperparamter
  # values.
  train_control <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 3,
                                search = "grid")
  
  # Generate A Candidate Set of Parameter Values for XGboost Model
  tune_grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                           nrounds = c(50, 75, 100),
                           max_depth = 6:8,
                           min_child_weight = c(2.0, 2.25, 2.5),
                           colsample_bytree = c(0.3, 0.4, 0.5),
                           gamma = 0,
                           subsample = 1)
  set.seed(823)
  Model_boost <- train(formula,data,
                       method = "xgbTree",
                       tuneGrid = tune_grid,
                       trControl = train_control)
  
  # Model_boost<-xgboost(data = data.matrix(dataset[,c("FOTL_PkSize","FOTL_BNS_Sales_Pct",
  #                                                    "Hanes_PkSize","FOTL_Hanes_APRGAP","FOTL_Hanes_AURGAP")]),
  #                      label = data.matrix(dataset$FOTL_Pk_Share),
  #                      nrounds = 500)
  # postResample(predict(Model_boost,data.matrix(dataset)), dataset$FOTL_Pk_Share)
  
  set.seed(8231)
  train_control <- trainControl(method = "repeatedcv", repeats = 5)
  final_boost<-train(formula, data,
                     method = "xgbTree",
                     tuneGrid = Model_boost$bestTune,
                     trControl = train_control)
  return(final_boost)
}


# Grid Search for GBM Model
GBM_Tune<-function(formula, data){
  # Set up caret to perform 10-fold cross validation repeated 3 
  # times and to use a grid search for optimal model hyperparamter
  # values.
  train_control <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 3,
                                search = "grid")
  # Generate A Candidate Set of Parameter Values for GBM Model
  tune_grid<- expand.grid(interaction.depth=2,
                          n.trees =500,
                          shrinkage=0.1,
                          n.minobsinnode=10)
  set.seed(824)
  Model_gbm<-train(formula, data,
                   method="gbm",
                   verbose=FALSE,
                   trControl=train_control,
                   tuneGrid=tune_grid)
  
  set.seed(8241)
  final_gbm<-train(formula, data,
                   method = "gbm",
                   verbose=FALSE,
                   tuneGrid = Model_gbm$bestTune,
                   trControl = train_control)
  return(final_gbm)
  
}

