library(caret)
# auxiliary function to count number of nas in each feature
count.na.feat <- function(dt.frm){ 
   sapply(1:ncol(dt.frm), function(i) { sum(is.na(dt.frm[,i]))})
}

# read test file and clean irrelevant features 
tst.raw <- read.csv("pml-testing.csv")
count.na.feat(tst.raw)
logics <- which(sapply(1:ncol(tst.raw),function(i){class(tst.raw[,i])}) == "logical")
tst <- tst.raw[,-logics]
count.na.feat(tst)

# read train file and clean check that there are no NA left
trn.raw <- read.csv("pml-training.csv")
trn <- trn.raw[,-logics]
count.na.feat(trn)


# split train and test set from the trn set
outcomes <- trn[,ncol(trn)]
id.train <- createDataPartition(outcomes, p = 0.60,list=FALSE)
training <- trn[ id.train, -ncol(trn)]
testing  <- trn[-id.train, -ncol(trn)]

# preprocess to capture 80% of variance
classes <- sapply(1:ncol(training),function(i) {class(training[,i])})
nofactors <- which(classes != "factor")
pp<- preProcess(training[,-(1:7)],method="pca",thresh=0.8)
trainingpp <- data.frame(outcomes = outcomes[id.train], 
                         predict(pp, training[,-(1:7)]))
testingpp  <- data.frame(outcomes = outcomes[-id.train],
                         predict(pp, testing[,-(1:7)]))

# GLM with preprocess fit
fit.pp <- train(outcomes ~ ., data = trainingpp, method = "gbm")
confusionMatrix(data=predict(fit.pp, testingpp[,-1]),testingpp$outcomes)

# Random forest fit
trainingrf <- data.frame(outcomes = outcomes[id.train], training)
fit.rf <- train(outcomes ~ ., data=trainingrf)
confusionMatrix(data=predict(fit.rf, testing),outcomes[-id.train])




