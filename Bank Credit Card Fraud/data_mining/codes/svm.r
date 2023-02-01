library(rpart)
library(rpart.plot)
library(data.table)
library(ROSE)
require(tree)
library(cvms)
library(tibble)
library(e1071)
library(tidyverse)
library(pROC)
library(caret)
library(broom)

draw_confusion_matrix <- function(cm, ratio,train=TRUE,whole=FALSE) {
  
  layout(matrix(c(1, 1, 2)))
  par(mar = c(2, 2, 2, 2))
  plot(c(100, 345), c(300, 450),
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  set = ifelse(train,'TRAINING','TESTING')
  set0 = ifelse(whole==TRUE,'WHOLE',set)
  title(paste("TRAIN-SET PERCENTAGE:",
              toString(ratio * 100), "%;","RESULT ON",set0,"SET"), 
        cex.main = 1.5)
  
  # create the matrix
  rect(150, 430, 240, 370, col = "#00B9B1")
  text(195, 435, "non-fraud", cex = 1.2)
  rect(250, 430, 340, 370, col = "#FFAD00")
  text(295, 435, "fraud", cex = 1.2)
  text(125, 370, "Predicted", cex = 1.3, srt = 90, font = 2)
  text(245, 450, "Actual", cex = 1.3, font = 2)
  rect(150, 305, 240, 365, col = "#FFAD00")
  rect(250, 305, 340, 365, col = "#00B9B1")
  text(140, 400, "non-fraud", cex = 1.2, srt = 90)
  text(140, 335, "fraud", cex = 1.2, srt = 90)
  
  # add in the cm results
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex = 1.6, font = 2, col = "white")
  text(195, 335, res[2], cex = 1.6, font = 2, col = "white")
  text(295, 400, res[3], cex = 1.6, font = 2, col = "white")
  text(295, 335, res[4], cex = 1.6, font = 2, col = "white")
  
  # add in the specifics
  plot(c(100, 0), c(100, 0), type = "n", xlab = "", ylab = "",
       main = "DETAILS", xaxt = "n", yaxt = "n")
  text(10, 85, names(cm$byClass[1]), cex = 1.2, font = 2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex = 1.2)
  text(30, 85, names(cm$byClass[2]), cex = 1.2, font = 2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex = 1.2)
  text(50, 85, names(cm$byClass[5]), cex = 1.2, font = 2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex = 1.2)
  text(70, 85, names(cm$byClass[6]), cex = 1.2, font = 2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex = 1.2)
  text(90, 85, names(cm$byClass[7]), cex = 1.2, font = 2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex = 1.2)
  
  # add in the accuracy information
  text(30, 35, names(cm$overall[1]), cex = 1.5, font = 2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex = 1.4)
  text(70, 35, names(cm$overall[2]), cex = 1.5, font = 2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex = 1.4)
}

dat <- read.csv('card_transdata.csv')

#shuffle the data by row
set.seed(321)
shuffle_index <- sample(1:nrow(dat))
dat <- dat[shuffle_index, ]

#numeric to factor levels
dat <- as.data.table(dat)
dat[,repeat_retailer:= as.factor(repeat_retailer)]
dat[,used_chip:= as.factor(used_chip)]
dat[,online_order:= as.factor(online_order)]
dat[,used_pin_number:= as.factor(used_pin_number)]
dat[,fraud:= as.factor(fraud)]

#train,test partition
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


##############################################
#Support vector machine
##############################################
dat[,repeat_retailer:= as.numeric(repeat_retailer)]
dat[,used_chip:= as.numeric(used_chip)]
dat[,online_order:= as.numeric(online_order)]
dat[,used_pin_number:= as.numeric(used_pin_number)]

#standardization
dat_std <- cbind(scale(dat[,-c('fraud')]),data.table(dat$fraud))
dat_std <- dat_std %>% rename(fraud = V1)

#downsampling
dat_sub <-  ovun.sample(fraud~.,data = dat_std,p=0.5,
                        seed=1,method='under',)$data %>% arrange(fraud)
#select 80000 fraud and 80000 non-fraud, reshuffle
dat_sub_y <- dat_sub %>% filter(fraud == 1) %>% head(80000)
dat_sub_n <- dat_sub %>% filter(fraud == 0) %>% head(80000)
dat_sub <- rbind(dat_sub_y,dat_sub_n)

set.seed(1111)
shuffle_index <- sample(1:nrow(dat_sub))
dat_sub <- dat_sub[shuffle_index, ]


dat_train_std <- create_train_test(dat_sub, 0.8, train = TRUE)
dat_test_std <- create_train_test(dat_sub, 0.8, train = FALSE)

#set.seed(1234)
#svm_tune <- tune.svm(fraud ~. ,
#                     data=dat_sub, 
#                     gamma = c(0.5,50,500),
#                     cost=0.01,
#                     kernel='radial')

fit_svm <- svm(fraud ~. ,
               data=dat_train_std, 
               gamma = 0.5,
               cost= 0.01,
               kernel='radial')


predict_train <-predict(fit_svm, dat_train_std, type = 'class')
predict_test <-predict(fit_svm, dat_test_std, type = 'class')
predict_whole <-predict(fit_svm, dat_std, type = 'class')


draw_confusion_matrix(confusionMatrix(data = factor(predict_whole),
                                      reference = factor(dat_std$fraud), 
                                      positive = "1"), 0.8,train=TRUE,
                      whole = TRUE)

draw_confusion_matrix(confusionMatrix(data = factor(predict_train),
                                      reference = factor(dat_train_std$fraud), 
                                      positive = "1"), 0.8,train=TRUE)

draw_confusion_matrix(confusionMatrix(data = factor(predict_test),
                                      reference = factor(dat_test_std$fraud), 
                                      positive = "1"), 0.8,train=FALSE)



