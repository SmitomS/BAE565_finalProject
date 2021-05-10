RF model with time slice
================
Smitom Swapna Borah
5/10/2021

# Background

This RScript is the third attempt at developing a random forest model
for chlorophyll a in Jordan Lake. The intention of the author here is to
apply the RF model to the segment 1 and carry out a regression analysis.
Based on the model performance in RF\_2.R file, the data splitting
tachnique seems to work properly only in the first segment. In this
RScript, the author attempt to use timeslices again to see if better
results can be produced.

# Packages

``` r
# Clearing the global environment----
rm(list = ls())

# Loading the necessary packages----
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.5
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## Warning: package 'tibble' was built under R version 4.0.4

    ## Warning: package 'tidyr' was built under R version 4.0.4

    ## Warning: package 'dplyr' was built under R version 4.0.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.0.5

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 4.0.5

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(readxl)
```

# Data

``` r
# reading the data file
Jorlak <- read_excel("Lake_data.xlsx")
```

# Step 1

``` r
# Step-0: Pre-processing the data

Jorlak_seg1 <- Jorlak[which(Jorlak$segm=='Segment 1'),]

# Jorlak_seg1_preprocess <- preProcess(Jorlak_seg1[,3:7], method = c("range"))
# Jorlak_seg1 <- predict(Jorlak_seg1_preprocess, newdata = Jorlak_seg1)
# summary(Jorlak_seg1)


trainlen_1 <- round(0.8*nrow(Jorlak_seg1))
Train_1 <- Jorlak_seg1[seq(1,trainlen_1),]
Test_1 <- Jorlak_seg1[seq(trainlen_1+1, nrow(Jorlak_seg1)),]

# Step-1: Data splicing for time series
set.seed(777)


training_index_seg1 <- createTimeSlices(Train_1$chl_ugL ,
                                        initialWindow = 100,
                                        horizon = 1,
                                        fixedWindow = FALSE)
```

# Step 2+3

``` r
# Step-2+3: Training and testing
## Training the model with training data
out <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("mtry", "R2_Training", "R2_Testing"))
set.seed(777)

# Loop through the  kfolds
for (i in 1:length(training_index_seg1$train)){
  training <- Train_1[training_index_seg1$train[[i]],]
  testing <- Train_1[training_index_seg1$test[[i]],]
  # Train model with training data
  model <- train(
    chl_ugL ~ TP_ugL+T_w+TN_ugL+flSeg,
    data = training,
    method = "rf",
    importance = TRUE,
    tuneLength = 3,
    trControl = trainControl(method = "oob",
                             # verboseIter = TRUE
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
```

    ##     mtry R2_Training R2_Testing
    ## 1      2   0.7654235       -Inf
    ## 2      2   0.7758132       -Inf
    ## 3      2   0.7787515       -Inf
    ## 4      2   0.7755265       -Inf
    ## 5      2   0.7736061       -Inf
    ## 6      2   0.7635067       -Inf
    ## 7      2   0.7678220       -Inf
    ## 8      2   0.7833697       -Inf
    ## 9      2   0.7993835       -Inf
    ## 10     2   0.8012637       -Inf
    ## 11     2   0.7986331       -Inf
    ## 12     2   0.8023447       -Inf
    ## 13     2   0.8011431       -Inf
    ## 14     2   0.7995454       -Inf
    ## 15     2   0.7990836       -Inf
    ## 16     2   0.7987003       -Inf
    ## 17     2   0.8022318       -Inf
    ## 18     2   0.7991086       -Inf
    ## 19     2   0.8084692       -Inf
    ## 20     2   0.8085322       -Inf
    ## 21     2   0.8184484       -Inf
    ## 22     2   0.8123958       -Inf
    ## 23     2   0.8079044       -Inf
    ## 24     3   0.8243741       -Inf
    ## 25     2   0.8276002       -Inf
    ## 26     2   0.8329021       -Inf
    ## 27     2   0.8421187       -Inf
    ## 28     3   0.8531936       -Inf
    ## 29     3   0.8560312       -Inf
    ## 30     3   0.8539772       -Inf
    ## 31     2   0.8473741       -Inf
    ## 32     2   0.8452274       -Inf
    ## 33     2   0.8465532       -Inf
    ## 34     2   0.8476787       -Inf
    ## 35     2   0.8412146       -Inf
    ## 36     2   0.8453476       -Inf
    ## 37     2   0.8539473       -Inf
    ## 38     2   0.8485154       -Inf
    ## 39     2   0.8487944       -Inf
    ## 40     2   0.8512102       -Inf
    ## 41     2   0.8560659       -Inf
    ## 42     3   0.8641713       -Inf
    ## 43     2   0.8553187       -Inf
    ## 44     2   0.8563758       -Inf
    ## 45     2   0.8548084       -Inf
    ## 46     2   0.8510639       -Inf
    ## 47     2   0.8516637       -Inf
    ## 48     2   0.8562576       -Inf
    ## 49     2   0.8599952       -Inf
    ## 50     2   0.8584323       -Inf
    ## 51     2   0.8563807       -Inf
    ## 52     2   0.8575027       -Inf
    ## 53     3   0.8612701       -Inf
    ## 54     2   0.8585759       -Inf
    ## 55     2   0.8554963       -Inf
    ## 56     2   0.8618975       -Inf
    ## 57     2   0.8649807       -Inf
    ## 58     2   0.8652868       -Inf
    ## 59     2   0.8679254       -Inf
    ## 60     2   0.8559632       -Inf
    ## 61     2   0.8615784       -Inf
    ## 62     2   0.8680118       -Inf
    ## 63     2   0.8652361       -Inf
    ## 64     2   0.8565621       -Inf
    ## 65     2   0.8612576       -Inf
    ## 66     2   0.8589960       -Inf
    ## 67     2   0.8618502       -Inf
    ## 68     2   0.8614279       -Inf
    ## 69     2   0.8572691       -Inf
    ## 70     2   0.8641655       -Inf
    ## 71     2   0.8564203       -Inf
    ## 72     2   0.8602165       -Inf
    ## 73     2   0.8636835       -Inf
    ## 74     2   0.8640577       -Inf
    ## 75     2   0.8615756       -Inf
    ## 76     2   0.8640108       -Inf
    ## 77     2   0.8618053       -Inf
    ## 78     2   0.8694924       -Inf
    ## 79     3   0.8715590       -Inf
    ## 80     2   0.8623981       -Inf
    ## 81     2   0.8616898       -Inf
    ## 82     2   0.8665618       -Inf
    ## 83     2   0.8636704       -Inf
    ## 84     2   0.8636692       -Inf
    ## 85     2   0.8648716       -Inf
    ## 86     2   0.8664548       -Inf
    ## 87     2   0.8651940       -Inf
    ## 88     2   0.8652838       -Inf
    ## 89     2   0.8691894       -Inf
    ## 90     2   0.8729306       -Inf
    ## 91     2   0.8703583       -Inf
    ## 92     2   0.8668017       -Inf
    ## 93     2   0.8688172       -Inf
    ## 94     2   0.8679425       -Inf
    ## 95     2   0.8621883       -Inf
    ## 96     2   0.8662437       -Inf
    ## 97     2   0.8679691       -Inf
    ## 98     3   0.8705253       -Inf
    ## 99     2   0.8692892       -Inf
    ## 100    2   0.8621952       -Inf
    ## 101    4   0.8765353       -Inf
    ## 102    2   0.8580518       -Inf
    ## 103    2   0.8611758       -Inf
    ## 104    2   0.8695586       -Inf
    ## 105    2   0.8653935       -Inf
    ## 106    2   0.8625690       -Inf
    ## 107    2   0.8652880       -Inf
    ## 108    2   0.8660264       -Inf
    ## 109    2   0.8652338       -Inf

``` r
# Model summary
model
```

    ## Random Forest 
    ## 
    ## 208 samples
    ##   4 predictor
    ## 
    ## No pre-processing
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared 
    ##   2     20.81558  0.3052499
    ##   3     20.90428  0.2993163
    ##   4     21.04476  0.2898670
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 2.

# Step 4

``` r
# Step-4: Final model
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
```

    ## Random Forest 
    ## 
    ## 209 samples
    ##   4 predictor
    ## 
    ## No pre-processing
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared
    ##   20.72032  0.308299
    ## 
    ## Tuning parameter 'mtry' was held constant at a value of 2

``` r
# calculating the model performance with training data
Train_1 <- Train_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Train_1))


Train_1%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)),
            pearsonr2 = cor(chl_ugL, pred_chl_ugL, method = "pearson")^2)
```

    ## # A tibble: 1 x 2
    ##      r2 pearsonr2
    ##   <dbl>     <dbl>
    ## 1 0.869     0.911

``` r
plot(Train_1$chl_ugL,Train_1$pred_chl_ugL)
```

![](RF_timeslice_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
## calculating the model performance with testing data
Test_1 %>% mutate(pred_chl_ugL = predict(model$finalModel, newdata = Test_1))%>%
  summarize(r2 = 1-(sum((chl_ugL-pred_chl_ugL)^2))/(sum((chl_ugL-mean(chl_ugL))^2)))
```

    ## # A tibble: 1 x 1
    ##      r2
    ##   <dbl>
    ## 1 0.234
