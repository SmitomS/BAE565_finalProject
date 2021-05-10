# Title: Neural network model for chlorophyll a
# Author: Smitom Borah
# Date: 05/08/2021

# Background----
## This RScript is the third attempt at developing a random forest model for
## chlorophyll a in Jordan Lake. 

# Clearing the global environment----
rm(list = ls())

# Loading the necessary packages----
library(tidyverse)
library(caret)
library(NeuralNetTools)



# loading the data frame----
load("chlLM_de19_BAE565_1008JoLa.RData")
Jorlak <- JoLa_wid    
# This data frame contains monthly data on
# chl a, TP, TN, Flushing rate, water temp.
# More details on this data frame can be found
# on DataExploration_1.R
# Jorlak[3:7] <- lapply(Jorlak[3:7],log)


# Segment 1----

# Step-0: Pre-processing the data

Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 3'),]
Jorlak_seg1 <- Jorlak_seg1 %>% select(-c('cale_date','segm'))
Jorlak_seg1_preprocess <- preProcess(Jorlak_seg1, method = c("range"))
Jorlak_seg1 <- predict(Jorlak_seg1_preprocess, newdata = Jorlak_seg1)
summary(Jorlak_seg1)

set.seed(777)

training_index_seg1 <- createDataPartition(Jorlak_seg1$chl_ugL,
                                           p = 0.75,
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


# Step-1: k-folds
set.seed(777)
kfolds <- createFolds(Train_1$chl_ugL,
                      k = 5,
                      returnTrain = TRUE)
kfolds


# Step-2: Training and testing
## Training the model with training data
out <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("size", "decay", "R2_Training", "R2_Testing"))
set.seed(777)

# Loop through the  kfolds
for (i in 1: length(kfolds)){
  fold <- kfolds[[i]]
  training <- Train_1[fold,]
  testing <- Train_1[-fold,]
  
  # Train model with training data
  model <- train(
    chl_ugL ~ TP_ugL+T_w+TN_ugL,
    data = training,
    method = "nnet",
    importance = TRUE,
    tuneLength = 10,
    trace = F,
    trControl = trainControl(method = "cv")
  )
  
  m <- model$bestTune[[1]]
  d <- model$bestTune[[2]]
  ## calculating the model performance with training data
  training %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = training))%>%
    summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2))) -> trained 
  
  ## calculating the model performance with testing data
  testing %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = testing))%>%
    summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2))) -> tested
  out[i,] <- c(m,d,trained,tested)
  
}

out
# Model summary
model


# Creating a model using all the data
set.seed(777)
model <- train(
  chl_ugL ~ TP_ugL+T_w+TN_ugL,
  data = Train_1,
  method = "nnet",
  importance = TRUE,
  # tuneLength = 10,
  tuneGrid = expand.grid(size = c(0),
                          decay = 0.00316,
  trControl = trainControl(method = "cv")
  
  )

# # Model summary with entire data set
# model

# calculating the model performance with training data
Train_1 <- Train_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Train_1))


Train_1%>%summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
             pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)


plot(Train_1$chl_ugL,Train_1$pred_chl_ugL)



## calculating the model performance with testing data
Test_1 <- Test_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Test_1))
Test_1 %>% summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)))

plot(Test_1$chl_ugL,Test_1$pred_chl_ugL)
