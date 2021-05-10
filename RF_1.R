# Title: Random Forest model for chlorophyll a
# Author: Smitom Borah
# Date: 05/08/2021

# Background----
## This RScript is the first attempt at developing a random forest model for
## chlorophyll a in Jordan Lake. The intention of the author here is to apply 
## the RF model to the segment 1 and carry out a regression analysis. This 
## is step 1. Once smooth working of the model is established, the author intends
## to develop individual models for each segments in the reservoir.

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
Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 2'),]

# Step-0: Pre-processing the data
# Jorlak_seg1_preprocess <- preProcess(Jorlak_seg1[,3:7], method = "center")
# Jorlak_seg1 <- predict(Jorlak_seg1_preprocess, newdata = Jorlak_seg1)
# summary(Jorlak_seg1)

# Jorlak_seg1[3:7] <- lapply(Jorlak_seg1[3:7],log)
# Step-1: Data splicing for time series----
training_index_seg1 <- createTimeSlices(Jorlak_seg1$chl_ugL ,
                                   initialWindow = 162,
                                   horizon = 60,
                                   fixedWindow = FALSE)
training <- Jorlak_seg1[training_index_seg1$train[[1]],]
testing <- Jorlak_seg1[training_index_seg1$test[[1]],]

# training_index_seg1 <- createDataPartition(Jorlak_seg1$chl_ugL,
#                                            p = 0.8,
#                                            list = F)
# training <- Jorlak_seg1[training_index_seg1,]
# testing <- Jorlak_seg1[-training_index_seg1,]

# Step-2: Training and testing----
# ## Loop through each slice of time sereies
# for (i in 1:length(training_index_seg1$train)) {
#   training <- Jorlak_seg1[training_index_seg1$train[[i]],]
#   testing  <- Jorlak_seg1[training_index_seg1$test[[i]],]


## Check balance of training set
ggplot() +
  geom_density(data = Jorlak_seg1, aes(x = chl_ugL), fill = "black", alpha = 0.5) +
  geom_density(data = training, aes(x = chl_ugL), color = "red", size = 2) +
  geom_density(data = testing, aes(x = chl_ugL), color = "blue", size = 2) +
  theme_bw()

  # Training the model with training data
  model <- train(
    chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
    data = training,
    method = "rf",
    importance = TRUE,
    tuneLength = 3,
    trControl = trainControl(method = "timeslice",
                             initialWindow = 100,
                             horizon = 20,
                             fixedWindow = FALSE
                             )
  )
# }

# # Training the model with training data
# model <- train(
#   chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
#   data = training,
#   method = "rf",
#   importance = TRUE,
#   tuneLength = 3,
#   trControl = trainControl(method = "oob")
# )
# 
# # calculating the model performance with training data
training %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = training))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)

# calculating the model performance with testing data
testing %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = testing))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)
































# Refereneces ----
## While creating this RScripts, following sources were used:
## 1. BAE565 lecture slides
## 2. BAE565 R exercises, especially 9_rf.R
## 3. Hands-on Machine Learning with R: https://bradleyboehmke.github.io/HOML/random-forest.html
## 4. The caret package: https://topepo.github.io/caret/data-splitting.html


