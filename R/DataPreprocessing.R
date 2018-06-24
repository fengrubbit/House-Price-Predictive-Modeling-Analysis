# Import necessary library
#install.packages("RODBC")
# library(RODBC)
#install.packages("h2o")

library(dplyr)
require(tidyr)
require(ggplot2)
require(readr)
require(lubridate)
require(readxl)

require(rBayesianOptimization)
require(forecast)
require(e1071)
require(randomForest)
require(rpart)
require(xgboost)
require(caret)
require(corrplot)
require(mlbench)
require(h2o)

##########
# Pre-Process of the Train dataset
##########
# Import boston house pricing data 
Train <- read.csv("train.csv", stringsAsFactors = FALSE)[,-1]
# convert some of character variables into numeric variables
Train[,c("LotFrontage", "MasVnrArea", "GarageYrBlt")] <- sapply(Train[,c("LotFrontage", "MasVnrArea", "GarageYrBlt")], as.numeric)

####
# Numeric vs Categorical variables in the Train dataset
####

# Since each variables distribution are different, 
# so it need use different values to deal with NA's 

# Numeric Variables
Train_Num <- Train[,unlist(lapply(Train, is.numeric))]

# Replace NA's in LotFrontage by it's median
ggplot(data = Train_Num, aes(x=LotFrontage, y = SalePrice, color = LotFrontage))+geom_point()
Train_Num$LotFrontage[is.na(Train_Num$LotFrontage)]<-median(Train_Num$LotFrontage, na.rm = TRUE)

# Replace NA's in MasVnrArea by it's 0
hist(Train_Num$MasVnrArea)
Train_Num$MasVnrArea[is.na(Train_Num$MasVnrArea)]<-0

# NA's in GarageYrBlt means theres is no Garage in the house
# and their price is relatively low, 
# so we replaced by it's min: 1900
Train_Num$GarageYrBlt[is.na(Train_Num$GarageYrBlt)]<-1900
plot(Train_Num$GarageYrBlt, Train_Num$SalePrice)


# Categorical variables
Train_Cat <- Train[,unlist(lapply(Train, is.character))]

# Replace NA's in Alley by "Other" since Grvl Alley looks has higher price compare with Pave
ggplot(data = Train[!(is.na(Train$Alley)),], aes(x= LotArea, y = SalePrice, color = Alley))+geom_point()



Train_Cat[,colnames(Train_Cat)[colSums(is.na(Train_Cat))>0]] <- sapply(Train_Cat[,colnames(Train_Cat)[colSums(is.na(Train_Cat))>0]],
                                                                       NAReplace_Char)
Train_Cat <- Train_Cat %>% lapply(as.factor) %>% data.frame()
  

# Convert factors into dummy variables via model.matrix
Train <- data.frame(model.matrix(SalePrice ~ ., data =cbind(Train_Num, Train_Cat)),
                    SalePrice = Train_Num$SalePrice)

