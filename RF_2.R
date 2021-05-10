# Title: Random Forest model for chlorophyll a
# Author: Smitom Borah
# Date: 05/08/2021

# Background----
## This RScript is the second attempt at developing a random forest model for
## chlorophyll a in Jordan Lake. The intention of the author here is to apply 
## the RF model to the segment 1 and carry out a regression analysis. This 
## is step 1. Once smooth working of the model is established, the author intends
## to develop individual models for each segments in the reservoir. Based on the
## model performance in RF_1.R file, timeslice is not a good approach to
## to carry out regression analysis. It gives miserably poor values, indicating
## there is no correlation with time and chlorophyll a data. In this file,
## the author tried to evaluate the model performance based on data splitting.

# Clearing the global environment----
rm(list = ls())

# Loading the necessary packages----
library(tidyverse)
library(caret)
library(randomForest)

# loading the data frame----
load("chlLM_de19_BAE565_1008JoLa.RData")
Jorlak <- JoLa_wid    # This data frame contains monthly data on
                      # chl a, TP, TN, Flushing rate, water temp.
                      # More details on this data frame can be found
                      # on DataExploration_1.R
# Jorlak[3:7] <- lapply(Jorlak[3:7],log)

# Segment 1----


# Step-0: Pre-processing the data
Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 1'),]


# Step-1: Data splicing for time series
set.seed(777)

data_split <- createDataPartition(Jorlak_seg1$chl_ugL,
                                           p = 0.8,
                                           list = F)
Train_1 <- Jorlak_seg1[data_split,]
Test_1 <- Jorlak_seg1[-data_split,]

training_index_seg1 <- createDataPartition(Train_1$chl_ugL,
                                           p = 0.8,
                                           list = F)
training <- Train_1[training_index_seg1,]
testing <- Train_1[-training_index_seg1,]

## Check balance of training set
ggplot() +
  geom_density(data = Train_1, aes(x = chl_ugL), fill = "black", alpha = 0.5) +
  geom_density(data = training, aes(x = chl_ugL), color = "red", size = 2) +
  geom_density(data = testing, aes(x = chl_ugL), color = "blue", size = 2) +
  theme_bw()

# Step-2: Training and testing
## Training the model with training data
set.seed(777)
model <- train(
  chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
  data = training,
  method = "rf",
  importance = TRUE,
  tuneLength = 3,
  trControl = trainControl(method = "oob"
  )
)

# Model summary
model

## calculating the model performance with training data
training %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = training))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

## calculating the model performance with testing data
testing %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = testing))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)
best_fit <- model$bestTune[[1]]
# Creating a model using all the data
set.seed(777)
model <- train(
  chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
  data = Train_1,
  method = "rf",
  importance = TRUE,
  tuneGrid = expand.grid(mtry = c(best_fit)),
  trControl = trainControl(method = "oob")
)

# Model summary with entire data set
model

## calculating the model performance with training data
Train_1 <- Train_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Train_1))


Train_1%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

plot(Train_1$chl_ugL,Train_1$pred_chl_ugL)


## calculating the model performance with absolutely new testing data
Test_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Test_1))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)




