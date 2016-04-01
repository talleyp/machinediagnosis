library(data.table)

counties <- read.table("data/counties_list_18.txt",sep="\t", header=TRUE)
counties$NAME <- as.character.factor(counties$NAME)
indi <- read.csv("data/indiana.csv")

countyname <- counties$NAME
countyname <- strsplit(countyname, split = " ")
countyname <- unlist(countyname)
countyindex <- grepl("County",countyname)
countyname <- countyname[!(countyindex)]
countyname[71] = "St. Joseph"
countyname <- countyname[-72]

counties$NAME <- countyname

counties <- counties[,c(4,11,12)]

colnames(counties) <- c("county","latitude","longitude")

indicounty <- merge(x = indi, y = counties, by = "county")
#indicounty$county <- as.character.factor(indicounty$county)

# Machine learning
library(caret)
library(rpart)
library(randomForest)
cn <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang",
        "oldpeak","slope","ca","thal","num")
temp <- list.files(path="training", pattern="*.txt", full.names=TRUE)




data <- do.call(rbind, lapply(temp, function(x) read.table(x, sep = ",", na.string = "?",
                                                           col.names = cn)))

data <- data[!is.na(data$num),]
data$sex <- factor(data$sex)
data$cp <- factor(data$cp)
data$fbs <- factor(data$fbs)
data$restecg <- factor(data$restecg)
data$exang <- factor(data$exang)
data$slope <- factor(data$slope)
data$thal <- factor(data$thal)

data$val <- ifelse(data$num==0, 0, 1)
data$num <- as.factor(data$val)
data$val <- NULL


inTrain <- createDataPartition(y=factor(data$num), p=1, list=FALSE)
train <- data[inTrain, ]


myDataNZV <- nearZeroVar(train, saveMetrics=TRUE)
nameNZM <- row.names(myDataNZV[which(myDataNZV$nzv == TRUE, arr.ind = T),])
trainNZV <- names(train) %in% nameNZM
subtrain <- train[!trainNZV]
avgna <- function(x) {
  n <- length(x)
  na.count <- sum(is.na(x))
  return((n - na.count)/n)
}
nacols <- apply(subtrain, 2, avgna)
subtrain <- subtrain[, nacols > 0.9]

rfMod <- train(num~.,data=subtrain,method="rf",
               trControl=trainControl(method="cv",number=5),
               prox=TRUE,allowParallel=TRUE)

predFrame <- data.frame(age=numeric(),
                        sex=factor(),
                        cp=factor(),
                        testbps=numeric(),
                        chol=numeric(),
                        fbs=factor(),
                        restecg=factor(),
                        thalach=numeric(),
                        exang=factor(),
                        oldpeak=numeric(),
                        slope=factor(),
                        ca=numeric(),
                        thal=factor())
predFrame[1,] <- 0