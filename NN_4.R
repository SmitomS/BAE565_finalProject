# Title: Developing a neural network with neuralnet package
# Author: Smitom
# Date: 05/09/2021

# Background----
## The nnet package failed to deliver. An attempt is made to repeat the previous 
## model with neuralnet package

# Clearing the global environment
rm(list = ls())
# cat("\014"); rm(list=ls(all=TRUE)); graphics.off()


# Loading packages
library(tidyverse)
library(caret)
library(neuralnet)
library(NeuralNetTools)

# Loading the data
load("chlLM_de19_BAE565_1008JoLa.RData")
Jorlak <- JoLa_wid 


# Segment 1----
# Step-0: Pre-processing the data

Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 1'),]
Jorlak_seg1 <- Jorlak_seg1 %>% select(-c('cale_date','segm'))

Jorlak_seg1_preprocess <- preProcess(Jorlak_seg1, method = c('range'))
Jorlak_seg1 <- predict(Jorlak_seg1_preprocess, newdata = Jorlak_seg1)
summary(Jorlak_seg1)

# Step-1: Data splitting----

set.seed(777)

Training_index_seg1 <- createDataPartition(Jorlak_seg1$chl_ugL,
                                           p = 0.75,
                                           list = F)
Train_1 <- Jorlak_seg1[Training_index_seg1,]
Test_1 <- Jorlak_seg1[-Training_index_seg1,]

training_index <- createDataPartition(Train_1$chl_ugL,
                                      p = 0.75,
                                      list = F)
training <- Train_1[training_index,]
testing <- Train_1[-training_index,]


# Step-2: Training and Testing
x <- c(12,6)
set.seed(777)
nn <- neuralnet(
  chl_ugL~ TP_ugL+TN_ugL+T_w+flSeg,
  data = training,
  hidden = x,
  linear.output = T
)

# plot(nn)


## calculating the model performance with training data
set.seed(777)
training %>% mutate(pred_chl_ugL = predict(nn, newdata = training))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)


## calculating the model performance with testing data
set.seed(777)
testing %>% mutate(pred_chl_ugL = predict(nn, newdata = testing))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)


# Step-3: Training entire model----
set.seed(2000)
nn <- neuralnet(
  chl_ugL~ TP_ugL+TN_ugL+T_w+flSeg,
  data = Train_1,
  hidden = x,
  linear.output = T
)

## calculating the model performance with training data
set.seed(2000)
Train_1 %>% mutate(pred_chl_ugL = predict(nn, newdata = Train_1))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

# Step-4: Model performance against completely new data----
set.seed(2000)
Test_1 %>% mutate(pred_chl_ugL = predict(nn, newdata = Test_1))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

# Step-5: feature importance
## Plot network
plotnet(nn) 

# Predictor variable importance
olden(nn) 
# garson(nn)

# Weights
nn$weights

