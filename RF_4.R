# Title: Random Forest model for chlorophyll a
# Author: Smitom Borah
# Date: 05/08/2021

# Background----
## This RScript is the third attempt at developing a random forest model for
## chlorophyll a in Jordan Lake. The intention of the author here is to apply 
## the RF model to the segment 1 and carry out a regression analysis. This 
## is step 1. Once smooth working of the model is established, the author intends
## to develop individual models for each segments in the reservoir. Based on the
## model performance in RF_2.R file, the data splitting tachnique seems to 
## work properly only in the first segment. The R2 values for the rest of the 
## segments were poor and not as expected. In this RScript, the author attempt to 
## use kfold method to see if better results can be produced.

# Clearing the global environment----
rm(list = ls())

# Loading the necessary packages----
library(tidyverse)
library(caret)
library(randomForest)
library(lubridate)


# loading the data frame----
load("chlLM_de19_BAE565_1008JoLa.RData")
Jorlak <- JoLa_wid    
# This data frame contains monthly data on
# chl a, TP, TN, Flushing rate, water temp.
# More details on this data frame can be found
# on DataExploration_1.R
# Jorlak[3:7] <- lapply(Jorlak[3:7],log)


# Segment 1----
set.seed(777)
Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 1'),]
training_index_seg1 <- createDataPartition(Jorlak_seg1$chl_ugL,
                                           p = 0.8,
                                           list = F)
Train_1 <- Jorlak_seg1[training_index_seg1,]
Test_1 <- Jorlak_seg1[-training_index_seg1,]

## Checking balance of training set
ggplot() +
  geom_density(data = Jorlak_seg1, aes(x = chl_ugL), fill = "black", alpha = 0.5) +
  geom_density(data = Train_1, aes(x = chl_ugL), color = "red", size = 2) +
  geom_density(data = Test_1, aes(x = chl_ugL), color = "blue", size = 2) +
  theme_bw()

# Step-0: Pre-processing the data
# Jorlak_seg1[3:7] <- lapply(Jorlak_seg1[3:7],log)


# Step-1: Data splicing for time series
set.seed(777)
kfolds <- createFolds(Train_1$chl_ugL,
                      k = 5,
                      returnTrain = TRUE)
kfolds


# Step-2: Training and testing
## Training the model with training data
out <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("mtry", "R2_Training", "R2_Testing"))
set.seed(777)

# Loop through the  kfolds
for (i in 1: length(kfolds)){
  fold <- kfolds[[i]]
  training <- Train_1[fold,]
  testing <- Train_1[-fold,]

  # Train model with training data
  model <- train(
    chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
    data = training,
    method = "rf",
    importance = TRUE,
    tuneLength = 3,
    trControl = trainControl(method = "oob"
    )
  )
  
  m <- model$bestTune[[1]]
  ## calculating the model performance with training data
  training %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = training))%>%
    summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2))) -> trained 
  
  ## calculating the model performance with testing data
  testing %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = testing))%>%
    summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2))) -> tested
  out[i,] <- c(m,trained,tested)
  
}

out
# Model summary
model


# Creating a model using all the data
set.seed(777)
model <- train(
  chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
  data = Train_1,
  method = "rf",
  importance = TRUE,
  tuneGrid = expand.grid(mtry = c(2)),
  trControl = trainControl(method = "oob")
)

# Model summary with entire data set
model

# calculating the model performance with training data
Train_1 <- Train_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Train_1))


Train_1%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)


plot(Train_1$chl_ugL,Train_1$pred_chl_ugL)



## calculating the model performance with testing data
Test_1 <- Test_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Test_1))
Test_1 %>% summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)))

plot(Test_1$chl_ugL,Test_1$pred_chl_ugL)

# Step-5: inspect the variable importance
importance(model$finalModel, scale = TRUE)
varImpPlot(model$finalModel)


# To plot the scatter plot of obs vs pred in log scale for entire segment 1 chl data
# Jorlak_seg1_rf[3:8] <- lapply(Jorlak_seg1_rf[3:8],log)
# Jorlak_seg1_rf %>% summarize(R2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)))
# 
# # Scatter plot with Test data set
# ggplot(data = Jorlak_seg1_rf)+
#   geom_point(aes(x = pred_chl_ugL, y = chl_ugL), color = "blue")+
#   labs(x=expression("Predicted chlorophyll ["*log[10]*"("*mu*g/l*")]"),
#        y=expression("Observed chlorophyll ["*log[10]*"("*mu*g/l*")]"))+ 
#   geom_abline(slope=1, intercept=0,color=gray(.55))

