library(caret)
library(rpart)
library(ggplot2)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(gridExtra)
set.seed(123)

cn <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang",
        "oldpeak","slope","ca","thal","num")
temp <- list.files(pattern="*.txt")
temp <- temp[2:4]


data <- do.call(rbind, lapply(temp, function(x) read.table(x, sep = ",", na.string = "?",
                                                            col.names = cn)))

data <- data[!is.na(data$num),]
data$sex <- factor(data$sex)
data$restecg <- factor(data$restecg)
data$exang <- factor(data$exang)
data$num <- factor(data$num)

dim(data)

inTrain <- createDataPartition(y=factor(data$num), p=0.6, list=FALSE)
train <- data[inTrain, ]
test <- data[-inTrain, ]
inCV <- createDataPartition(y=factor(test$num), p=0.5, list=FALSE)
cval <- test[inCV, ]
test <- test[-inCV, ]


myDataNZV <- nearZeroVar(train, saveMetrics=TRUE)
nameNZM <- row.names(myDataNZV[which(myDataNZV$nzv == TRUE, arr.ind = T),])
trainNZV <- names(train) %in% nameNZM
subtrain <- train[!trainNZV]
avgna <- function(x) {
    n <- length(x)
    na.count <- sum(is.na(x))
    return((n - na.count)/n)
}
# Grab frequency of NA's and subset out those columns
nacols <- apply(subtrain, 2, avgna)
subtrain <- subtrain[, nacols > 0.9]


rpartMod <- train(factor(num)~., method = "rpart", data = subtrain)
print(rpartMod$finalModel)
fancyRpartPlot(rpartMod$finalModel,cex=1,under.cex=2,shadow.offset=0)
numpredict=predict(rpartMod,cval)
confusionMatrix(cval$num, numpredict)


rfMod <- randomForest(num ~ ., data = subtrain)
order(varImp(rfMod), decreasing = T)
numpredict2=predict(rfMod,cval)
confusionMatrix(cval$num,numpredict2)


glmMod <- train(num ~ ., data=subtrain, method="glm", family = "binomial")
numpredict3=predict(glmMod,cval)
confusionMatrix(cval$num,numpredict3)

