### R code for course project ###
library(caret)

training = read.csv("pml-training.csv", na.strings = c("NA","#DIV/0!",""))
testing = read.csv("pml-testing.csv", na.strings = c("NA","#DIV/0!",""))
saveTrain = training
saveTest = testing
head(training)
summary(training)
str(training)

# # Since we have the converted timestamp, we drop the raw timestamps
# training = training[,-c(3,4)]
# temp = training[training$new_window == "yes",]
# summary(temp)
# # Dropping the converted time stamps too
# training = training[,-3]

## Drop the first 6 rows as they have no information

set.seed(111)
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)
timer = Sys.time()
model = train(classe~., data = train,method = "rf",trControl = fitControl)
Sys.time() - timer


######## TESTING HERE ##############
# Dropping predictors with near zero variance
training = saveTrain

set.seed(3456)
trainIndex = createDataPartition(training$classe, times = 1, list = FALSE, p = 0.7)
train = training[trainIndex,]
test = training[-trainIndex,]
# training = apply(training,2, FUN = function(x) as.numeric(x))
index = nearZeroVar(train, saveMetrics = TRUE)
indexDrop = nearZeroVar(train)
train = train[,-indexDrop]
summary(train)


## Let's check how many variables have > 18000 missing values
missing = apply(X = train, MARGIN = 2, FUN = function(x){sum(is.na(x))})
sum(missing > 5000)
unique(missing) # Seems that values that are missing are always 19216, lets check if once missing the rest are as well
temp = training[is.na(training$max_roll_belt),]
summary(temp)
missing = apply(X = temp, MARGIN = 2, FUN = function(x){sum(is.na(x))})
unique(missing)
sum(missing > 18000) # Yes, once a value is missing, it is similarly missing for the rest of the variables as well
# Let's drop all variables with missing values
train = train[,-which(missing > 5000,arr.ind = TRUE)]
summary(training)

train = train[,-c(1:6)]


vars = colnames(train)
test = test[,colnames(test) %in% vars]

testPred = predict(model, test)
confusionMatrix(testPred,test$classe)
##############################################

training = training[,-c(1,3,4,5)]
testing = testing[,-index]
######### END OF TESTING CODE ##########