####
# Feature Selection VS Exploratory Analysis
####

# FeatureSelection function is a self define function which help pre-processing
# dataset which include FindZero,Remove Linear Depedency, Find Important Features
# Feature Selection and Remove Redundant selections.
# This process might take a long time for the big dataset
FS_Train <- FeatureSelection(DataUse = Train, 
                             TargetVariable = "SalePrice",
                             FindZero = TRUE)
Train_DataforModel <- FS_Train[[1]]

# Exploratory Analysis for important factors to predict SalePrice
dim(Train_DataforModel)
cor(Train_DataforModel)%>%corrplot(method="number")
pairs(~OverallQual+X1stFlrSF+X2ndFlrSF+BsmtFinSF1+GarageArea+SalePrice, 
      data = Train_DataforModel)
pairs(~LotArea+MSSubClass+MSZoningRM+YearRemodAdd+Fireplaces+SalePrice, 
      data = Train_DataforModel)
pairs(~BsmtQualGd+KitchenQualGd+ExterQualGd+FullBath+SalePrice, 
      data = Train_DataforModel)

Remove <- c("GrLivArea", "CentralAirY","GarageTypeAttchd","OverallCond", "GarageYrBlt")
Train_DataforModel <- Train_DataforModel[, !(colnames(Train_DataforModel)%in% Remove)]

#### Fit all possible regression Models
Model_lm <- lm(SalePrice~.,data = Train_DataforModel)
summary(Model_lm)

Model_Rand<-SVR_Tune(SalePrice~.,data = Train_DataforModel)

Model_tree<-rpart(SalePrice~.,data = Train_DataforModel)

Model_forest<-randomForest(SalePrice~.,data = Train_DataforModel,
                           ntree=1000)

Model_boost<-XGboost_Tune(SalePrice~.,data = Train)


Model_gbm<-GBM_Tune(SalePrice~.,data = Train_DataforModel)


#### Model Evaluation visualization based on Train

Pred<-Pricing_Pred(newdata = data.frame(Train),
                   Model_lm = Model_lm, Model_SVR = Model_Rand, 
                   Model_tree = Model_tree, Model_forest = Model_forest,
                   Model_gbm = Model_gbm, Model_boost = Model_boost)

