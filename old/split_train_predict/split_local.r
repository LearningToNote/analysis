




library(tm)
library(e1071)
library(SparseM)

data <- read.csv('/home/johannes/code/masterproject/data/data.csv')
true_pairs <- data[data$DDI != "NONE",]
false_pairs <- data[data$DDI == "NONE",]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]
data<-rbind(true_pairs, false_downsampled)

index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/10))
testset <- data[testindex,]
trainset <- data[-testindex,]



before_Corpus = Corpus(VectorSource(trainset$BEFORE))
before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE))
dict_before <- Terms(before_dtm)
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

between_Corpus = Corpus(VectorSource(trainset$BETWEEN))
between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE))
dict_between <- Terms(between_dtm)
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

after_Corpus = Corpus(VectorSource(trainset$AFTER))
after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE))
dict_after <- Terms(after_dtm)
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

extracted_features <- cbind(before_dtm, between_dtm, after_dtm)

trainddi <- trainset$DDI
testddi <- testset$DDI
trainddi <- as.factor(trainddi)

svm.model <- svm(x = extracted_features, y=trainddi, type="C-classification", cost = 8, gamma = 0.5)






before_Corpus = Corpus(VectorSource(testset$BEFORE))
before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_before))
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

between_Corpus = Corpus(VectorSource(testset$BETWEEN))
between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_between))
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

after_Corpus = Corpus(VectorSource(testset$AFTER))
after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_after))
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

# before_Corpus = Corpus(VectorSource(testset$BEFORE))
# before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_before))
# colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

# between_Corpus = Corpus(VectorSource(testset$BETWEEN))
# between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_between))
# colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

# after_Corpus = Corpus(VectorSource(testset$AFTER))
# after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_after))
# colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

extracted_features <- cbind(before_dtm, between_dtm, after_dtm)


svm.pred <- predict(svm.model, extracted_features)
result<-cbind(svm.pred, testset[,c(2,3)])

conf <- table(svm.pred,testddi)
accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)
accuracy

