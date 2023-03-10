library(e1071)
library(caTools)
library(class)
library(ROSE)
library(caret)

draw_confusion_matrix <- function(cm, ratio) {

    layout(matrix(c(1, 1, 2)))
    par(mar = c(2, 2, 2, 2))
    plot(c(100, 345), c(300, 450),
        type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    title(paste("TRAIN-SET PERCENTAGE:",
        toString(ratio * 100), "%"), cex.main = 2)

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

data <- read.csv("card_transdata.csv") # read data
data$fraud <- factor(data$fraud) # data processing
data[, 1:3] <- scale(data[, 1:3])

balance_data <- ovun.sample(fraud ~ ., data, method = "under",
        N = nrow(data) / 5, seed = 12345)$data

for  (j in c(0.8, 0.6, 0.4)){
    testsub <- sample(seq_len(nrow(balance_data)), nrow(balance_data) * j)
    trainset <- balance_data[testsub, ]
    testset <- balance_data[-testsub, ]
    print("sample done")

    classifier_knn <- knn(train = trainset, test = data,
        cl = trainset$fraud, k = 5)
    print("knn done")
    draw_confusion_matrix(confusionMatrix(data = factor(classifier_knn),
        reference = factor(data$fraud), positive = "1"), j)
    print("draw done")
}