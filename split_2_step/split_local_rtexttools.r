
library(RTextTools)

data <- read.csv('/home/johannes/code/masterproject/data/data_numeric.csv')

#### down sampling
true_pairs <- data[data$DDI != "5",]
false_pairs <- data[data$DDI == "5",]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]

data <- rbind(true_pairs, false_downsampled)
data <- data[sample(nrow(data)),]

index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/10))
testset <- data[testindex,]
trainset <- data[-testindex,]

########## TRAINING

data <- trainset

########## BINARY CLASSIFICATION

#### feature extraction

before_dtm <- create_matrix(data$BEFORE, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
o_before_dtm <- before_dtm
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
between_dtm <- create_matrix(data$BETWEEN, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
o_between_dtm <- between_dtm
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
after_dtm <- create_matrix(data$AFTER, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
o_after_dtm <- after_dtm
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

features <- cbind(before_dtm, between_dtm, after_dtm)

container <- create_container(features,data$DDI,trainSize=1:nrow(data),virgin=FALSE)

models <- train_models(container, algorithms=c("SVM"), method = "C-classification")


##################################################################

########## PREDICTION
data <- testset[,-1]

before_dtm <- create_matrix(data$BEFORE, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_before_dtm)
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
between_dtm <- create_matrix(data$BETWEEN, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_between_dtm)
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
after_dtm <- create_matrix(data$AFTER, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_after_dtm)
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

features <- cbind(before_dtm, between_dtm, after_dtm)

container <- create_container(features,labels=testset[,1],testSize=1:nrow(data),virgin=FALSE)

results <- classify_models(container,models)



analytics <- create_analytics(container, results)
analytics@algorithm_summary

table(results$SVM_LABEL, testset[,1])


