# Title: Another attempt at NN model for chlorophyll a
# Author: Smitom
# Date: 05/08/2021

# clearing the global environment
rm(list = ls())

# loading the package
library(tidyverse)
library(caret)
library(NeuralNetTools)

# loading data
load("chlLM_de19_BAE565_1008JoLa.RData")
Jorlak <- JoLa_wid 

# Step 0: data preprocessing
Jorlak_seg1 <- Jorlak[which(Jorlak$segm=="Segment 2"),]
Jorlak_seg1 <- Jorlak_seg1 %>% select(-c('cale_date','segm'))
Jorlak_preprocess <- preProcess(Jorlak[3:7],method = c("range"))
Jorlak <- predict(Jorlak_preprocess,newdata = Jorlak)
summary(Jorlak)
Jorlak <- data.frame(Jorlak)

# Step 1: creating the training and testing sets
set.seed(777)
training_index <- createDataPartition(Jorlak_seg1$chl_ugL,
                                      p = 0.75,
                                      list = FALSE)
training <- Jorlak_seg1[training_index,]
testing <- Jorlak_seg1[-training_index,]

# Step 2: Train model with training data
set.seed(777)
model <- 
  train(
    chl_ugL ~ .,
    data = training,
    method = "nnet",
    importance = TRUE,
    tuneLength = 5,
    trace = F,
    trControl = trainControl(method = "cv")
    )

## summarize model
model

## Calculate our own model statistics
training %>%
  mutate(pred_chl_ugL = predict(model$finalModel, newdata = training)) %>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

# Step 3. Quantify performance with testing data
testing %>%
  mutate(pred_chl_ugL = predict(model$finalModel, newdata = testing)) %>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

# library(quantmod) #for Lag()



# #Make toy dataset
# y <- sin(seq(0, 20, 0.1))
# te <- data.frame(y, x1=Lag(y), x2=Lag(y,2))
# names(te) <- c("y", "x1", "x2")
# te <- te[-c(1,2),]
# 
# #Fit model
# model <- train(y ~ x1 + x2, te, method='nnet', linout=TRUE, trace = FALSE,
#                #Grid of tuning parameters to try:
#                tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1)),
#                trControl = trainControl(method = "cv")) 
# ps <- predict(model, te)
# 
# #Examine results
# model
# 
# te %>%
#   mutate(pred_y = predict(model$finalModel, newdata = te)) %>%
#   summarize(r2 = 1-(sum((y-pred_y)^2))/(sum((y-mean(y))^2)),
#             pearsonr2 = cor(y, pred_y, method = "pearson")^2)
# 
# plot(y)
# lines(ps, col=2)
