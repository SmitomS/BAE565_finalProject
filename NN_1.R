# Title: Neural Network for chlorophyll a
# Author: Smitom Borah
# Date: 05/08/2021

# Background:----
## This is first attempt at developing a neural network model
## for the chlorophyll model. Data splitting method is used in this 
## RScript.

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

# function for normalization
mean_normalization <- function(x){
  x <- (x-mean(x))/(max(x)-min(x))
  return(x)
}

# Segment 1----
# Step-0: Pre-processing the data

Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 3'),]
Jorlak_seg1 <- Jorlak_seg1 %>% select(-c('cale_date','segm'))
Jorlak_seg1 <- Jorlak_seg1 %>% mutate(chl_ugL = mean_normalization(chl_ugL),
                                      TP_ugL = mean_normalization(TP_ugL),
                                      T_w    = mean_normalization(T_w),
                                      TN_ugL = mean_normalization(TN_ugL),
                                      flSeg = mean_normalization(flSeg))
# Jorlak_seg1_preprocess <- preProcess(Jorlak_seg1, method = c("scale","center"))
# Jorlak_seg1 <- predict(Jorlak_seg1_preprocess, newdata = Jorlak_seg1)
summary(Jorlak_seg1)

# 
# Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 4'),]
# Jorlak_seg1 <- Jorlak_seg1 %>% select(-c('cale_date','segm'))
# Jorlak_seg1_preprocess <- preProcess(Jorlak_seg1, method = c("range"))
# Jorlak_seg1 <- predict(Jorlak_seg1_preprocess, newdata = Jorlak_seg1)
# # Jorlak_seg1 <- Jorlak_seg1 %>% mutate(chl_ugL = chl_ugL*50,
# #                                       TP_ugL  = TP_ugL*50,
# #                                       T_w     = T_w*50,
# #                                       TN_ugL  = TN_ugL*50,
# #                                       flSeg   = flSeg*50)
# summary(Jorlak_seg1)


# Step-1: Data splicing for time series
set.seed(777)

Training_index_seg1 <- createDataPartition(Jorlak_seg1$chl_ugL,
                                           p = 0.75,
                                           list = F)
Train_1 <- Jorlak_seg1[Training_index_seg1,]
Test_1 <- Jorlak_seg1[-Training_index_seg1,]


## Checking balance of training set
# ggplot() +
#   geom_density(data = Jorlak_seg1, aes(x = chl_ugL), fill = "black", alpha = 0.5) +
#   geom_density(data = Train_1, aes(x = chl_ugL), color = "red", size = 2) +
#   geom_density(data = Test_1, aes(x = chl_ugL), color = "blue", size = 2) +
#   theme_bw()

## Training dataset
set.seed(777)
training_index_seg1 <- createDataPartition(Train_1$chl_ugL,
                                           p = 0.75,
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
  method = "nnet",
  importance = TRUE,
  trace = F,
  trControl = trainControl(method ="cv")
)

# Model summary
model

## calculating the model performance with training data
training %>% mutate(pred_chl_ugL = predict(model, newdata = training))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

## calculating the model performance with testing data
testing %>% mutate(pred_chl_ugL = predict(model, newdata = testing))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)
best_size <- model$bestTune[[1]]
best_decay <- model$bestTune[[2]]
# # Creating a model using all the data
# set.seed(777)
# model <- train(
#   chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
#   data = Train_1,
#   method = "nnet",
#   importance = TRUE,
#   trace = FALSE,
#   tuneLength = 10,
#   # tuneGrid = expand.grid(size = c(best_size),
#   #                        decay = best_decay),
#   trControl = trainControl(method = "cv")
# )
# 
# # Model summary with entire data set
# model
# 
# ## calculating the model performance with training data
# Train_1 <- Train_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Train_1))
# 
# 
# Train_1%>%
#   summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
#             pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)
# 
# plot(Train_1$chl_ugL,Train_1$pred_chl_ugL)
# 
# ## calculating the model performance with absolutely new testing data
# Test_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Test_1))%>%
#   summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
#             pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)
# 
