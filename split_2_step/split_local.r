

library(tm)
library(e1071)
library(SparseM)

data <- read.csv('/home/johannes/code/masterproject/data/data.csv')

#### down sampling
true_pairs <- data[data$DDI != "NONE",]
false_pairs <- data[data$DDI == "NONE",]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]

data<-rbind(true_pairs, false_downsampled)


index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/10))
testset <- data[testindex,]
trainset <- data[-testindex,]

data <- trainset

#### feature extraction

before_Corpus = Corpus(VectorSource(data$BEFORE))
before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE))
dict_before <- Terms(before_dtm)
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

between_Corpus = Corpus(VectorSource(data$BETWEEN))
between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE))
dict_between <- Terms(between_dtm)
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

after_Corpus = Corpus(VectorSource(data$AFTER))
after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE))
dict_after <- Terms(after_dtm)
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

extracted_features <- cbind(before_dtm, between_dtm, after_dtm)

ddi <- data$DDI
ddi <- as.factor(ddi)

#### binarize target values

levels(ddi)[match('NONE',levels(ddi))] <- FALSE
levels(ddi)[match(c('advise','int','mechanism','effect'),levels(ddi))] <- TRUE

svm.model.binary <- svm(x = extracted_features, y=ddi, type="C-classification", cost = 8, gamma = 0.5)


##############################
ddi <- data$DDI
ddi <- as.factor(ddi)
extracted_features <- extracted_features[data$DDI != "NONE",]
ddi <- ddi[-which(ddi == "NONE")]
ddi <- droplevels(ddi)

svm.model.classes <- svm(x = extracted_features, y=ddi, type="C-classification", cost = 8, gamma = 0.5)








##################################################################

data <- testset[,-1]

before_Corpus = Corpus(VectorSource(data$BEFORE))
before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_before))
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

between_Corpus = Corpus(VectorSource(data$BETWEEN))
between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_between))
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

after_Corpus = Corpus(VectorSource(data$AFTER))
after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_after))
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

extracted_features <- cbind(before_dtm, between_dtm, after_dtm)


svm.pred.binary <- predict(svm.model.binary, extracted_features)
result<-cbind(svm.pred.binary, data[,c(1,2)])

colnames(result)[1] <- "DDI"
levels(result$DDI) <- c(levels(result$DDI), 'advise','int','mechanism','effect', 'NONE')

true_index <- which(svm.pred.binary == TRUE)
svm.pred.classes <- predict(svm.model.classes, extracted_features[true_index,])

result$DDI[result$DDI == FALSE] <- 'NONE'
result[true_index,]$DDI <- svm.pred.classes

result <- droplevels(result)

#### local eval
test <- cbind(as.data.frame(result[,1]),as.data.frame(testset[,1]))
colnames(test) <- c('pred', 'real')
accuracy <- nrow(test[which(test$pred == test$real),]) / nrow(test)
accuracy
