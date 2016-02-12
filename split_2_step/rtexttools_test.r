library(RTextTools)

data <- read.csv('/home/johannes/code/masterproject/data/data.csv')

#### down sampling
true_pairs <- data[data$DDI != "NONE",]
false_pairs <- data[data$DDI == "NONE",]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]

data <- rbind(true_pairs, false_downsampled)
data <- data[sample(nrow(data)),]


before_dtm <- create_matrix(data$BEFORE, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
between_dtm <- create_matrix(data$BETWEEN, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
after_dtm <- create_matrix(data$AFTER, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

features <- cbind(before_dtm, between_dtm, after_dtm)

trainIndex = floor(nrow(features) * 0.9)
trainSize = 1:trainIndex
testSize = (trainIndex + 1):nrow(features)

container <- create_container(features,as.numeric(as.factor(data$DDI)),trainSize=trainSize, testSize=testSize,virgin=FALSE)

models <- train_models(container, algorithms=c("SVM"), method = "C-classification")

results <- classify_models(container,models)
analytics <- create_analytics(container, results)
analytics@algorithm_summary