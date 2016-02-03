




library(tm)
library(e1071)
library(SparseM)

data <- read.csv('/home/johannes/code/masterproject/data/data_onecol.csv')
# tokens <- read.csv('/home/johannes/code/masterproject/data/distinct_tokens.csv')$TOKEN

true_pairs <- data[data$DDI != "NONE",]
false_pairs <- data[data$DDI == "NONE",]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]
data<-rbind(true_pairs, false_downsampled)

index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/10))
testset <- data[testindex,]
trainset <- data[-testindex,]


corpus = Corpus(VectorSource(trainset$TEXT))
# dtm <- DocumentTermMatrix(corpus, control = list(removeStopwords=FALSE, dictionary=tokens))
dtm <- DocumentTermMatrix(corpus, control = list(removeStopwords=FALSE))

extracted_features <- dtm

trainddi <- trainset$DDI
testddi <- testset$DDI
trainddi <- as.factor(trainddi)

svm.model <- svm(x = extracted_features, y=trainddi, type="C-classification", cost = 8, gamma = 0.5)


corpus2 = Corpus(VectorSource(testset$TEXT))
dtm2 <- DocumentTermMatrix(corpus2, control = list(removeStopwords=FALSE, dictionary=Terms(dtm)))
svm.pred <- predict(svm.model, dtm2)
result<-cbind(svm.pred, testset[,c(2,3)])

conf <- table(svm.pred,testddi)
accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)
accuracy

