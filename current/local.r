
library(RTextTools)

data <- read.csv('/home/johannes/code/masterproject/data/data.csv')

#### down sampling
true_pairs <- data[data$DDI != -1,]
false_pairs <- data[data$DDI == -1,]

false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
false_downsampled <- false_pairs[false_downsampled_index,]

data <- rbind(true_pairs, false_downsampled)
data <- data[sample(nrow(data)),]



docs <- unique(data$DOC_ID)
testdocs <- sample(docs, trunc(length(docs)/10))

testrows <- which(data$DOC_ID %in% testdocs)
test_data <- data[testrows,]
train_data <- data[-testrows,]


# index <- 1:nrow(data)
# testindex <- sample(index, trunc(length(index)/10))
# testset <- data[testindex,]
# trainset <- data[-testindex,]

########## TRAINING

data <- trainset

########## BINARY CLASSIFICATION

#### feature extraction

before_dtm <- create_matrix(data$BEFORE, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
o_before_dtm <- before_dtm[1,]
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
between_dtm <- create_matrix(data$BETWEEN, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
o_between_dtm <- between_dtm[1,]
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
after_dtm <- create_matrix(data$AFTER, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf)
o_after_dtm <- after_dtm[1,]
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")


p_before_dtm <- create_matrix(data$P_BEFORE, minWordLength=1, removeStopwords=FALSE)
o_p_before_dtm <- p_before_dtm[1,]
colnames(p_before_dtm) <- paste("pb", colnames(p_before_dtm), sep = "_")
p_between_dtm <- create_matrix(data$P_BETWEEN, minWordLength=1, removeStopwords=FALSE)
o_p_between_dtm <- p_between_dtm[1,]
colnames(p_between_dtm) <- paste("pi", colnames(p_between_dtm), sep = "_")
p_after_dtm <- create_matrix(data$P_AFTER, minWordLength=1, removeStopwords=FALSE)
o_p_after_dtm <- p_after_dtm[1,]
colnames(p_after_dtm) <- paste("pa", colnames(p_after_dtm), sep = "_")

features <- cbind(
		before_dtm, between_dtm, after_dtm,
		p_before_dtm, p_between_dtm, p_after_dtm,
		data$E1_TYPE, data$E2_TYPE,
		data$DIST)

ddi <- data$DDI

#### binarize target values

ddi[ddi != -1] <- 1
ddi[ddi == -1] <- 0
container <- create_container(features,ddi,trainSize=1:nrow(data),virgin=FALSE)

svm.model.binary <- train_models(container, algorithms=c("SVM"), method = "C-classification")

########## MULTI CLASS TRAINING

data <- data[data$DDI != -1,]


before_dtm <- create_matrix(data$BEFORE, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_before_dtm)
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
between_dtm <- create_matrix(data$BETWEEN, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_between_dtm)
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
after_dtm <- create_matrix(data$AFTER, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_after_dtm)
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")


p_before_dtm <- create_matrix(
	data$P_BEFORE,
	minWordLength=1,
	removeStopwords=FALSE,
	originalMatrix=o_p_before_dtm)
colnames(p_before_dtm) <- paste("pb", colnames(p_before_dtm), sep = "_")

p_between_dtm <- create_matrix(
	data$P_BETWEEN,
	minWordLength=1,
	removeStopwords=FALSE,
	originalMatrix=o_p_between_dtm)
colnames(p_between_dtm) <- paste("pi", colnames(p_between_dtm), sep = "_")

p_after_dtm <- create_matrix(
	data$P_AFTER,
	minWordLength=1,
	removeStopwords=FALSE,
	originalMatrix=o_p_after_dtm)
colnames(p_after_dtm) <- paste("pa", colnames(p_after_dtm), sep = "_")


features <- cbind(
		before_dtm, between_dtm, after_dtm,
		p_before_dtm, p_between_dtm, p_after_dtm,
		data$E1_TYPE, data$E2_TYPE,
		data$DIST)

ddi <- data$DDI

container <- create_container(features,ddi,trainSize=1:nrow(data),virgin=FALSE)

svm.model.classes <- train_models(container, algorithms=c("SVM"), method = "C-classification")





##################################################################

########## PREDICTION
data <- testset[,-1]

before_dtm <- create_matrix(data$BEFORE, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_before_dtm)
colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
between_dtm <- create_matrix(data$BETWEEN, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_between_dtm)
colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
after_dtm <- create_matrix(data$AFTER, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_after_dtm)
colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")


p_before_dtm <- create_matrix(
	data$P_BEFORE,
	minWordLength=1,
	removeStopwords=FALSE,
	originalMatrix=o_p_before_dtm)
colnames(p_before_dtm) <- paste("pb", colnames(p_before_dtm), sep = "_")

p_between_dtm <- create_matrix(
	data$P_BETWEEN,
	minWordLength=1,
	removeStopwords=FALSE,
	originalMatrix=o_p_between_dtm)
colnames(p_between_dtm) <- paste("pi", colnames(p_between_dtm), sep = "_")

p_after_dtm <- create_matrix(
	data$P_AFTER,
	minWordLength=1,
	removeStopwords=FALSE,
	originalMatrix=o_p_after_dtm)
colnames(p_after_dtm) <- paste("pa", colnames(p_after_dtm), sep = "_")


features <- cbind(
		before_dtm, between_dtm, after_dtm,
		p_before_dtm, p_between_dtm, p_after_dtm,
		data$E1_TYPE, data$E2_TYPE,
		data$DIST)

container <- create_container(features,labels=rep(0,nrow(data)),testSize=1:nrow(data),virgin=FALSE)

svm.pred.binary <- classify_models(container,svm.model.binary)

results <- svm.pred.binary


true_index <- which(results$SVM_LABEL == 1)
container <- create_container(features[true_index,],labels=rep(0, length(true_index)),testSize=1:length(true_index),virgin=FALSE)

svm.pred.classes <- classify_models(container, svm.model.classes)

levels(results$SVM_LABEL) <- c(levels(results$SVM_LABEL), levels(svm.pred.classes$SVM_LABEL), -1)
results[true_index,] <- svm.pred.classes
results[-true_index,] <- -1
results <- droplevels(results)



############### EVAL
container <- create_container(features,labels=testset[,1],testSize=1:nrow(data),virgin=FALSE)
analytics <- create_analytics(container, results)
analytics@algorithm_summary

table(results$SVM_LABEL, testset[,1])
################


result<-cbind(results[,1], data[,c(1,2)])
colnames(result)[1] <- "DDI"
