
library(RTextTools)

ltn.splitCopurs <- function(path) {
	data <- read.csv(path)

	#### down sampling
	true_pairs <- data[data$DDI != -1,]
	false_pairs <- data[data$DDI == -1,]

	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs) * 1.5)
	false_downsampled <- false_pairs[false_downsampled_index,]

	data <- rbind(true_pairs, false_downsampled)
	data <- data[sample(nrow(data)),]

	docs <- unique(data$DOC_ID)
	testdocs <- sample(docs, 20)

	testrows <- which(data$DOC_ID %in% testdocs)
	test_data <- data[testrows,-1]
	train_data <- data[-testrows,-1]

	return (list(train_data=train_data, test_data=test_data))
}

ltn.train_binary <- function(data) {
	#### feature extraction

	before_dtm <- create_matrix(data$BEFORE, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE)
	o_before_dtm <- before_dtm[1,]
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
	between_dtm <- create_matrix(data$BETWEEN, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE)
	o_between_dtm <- between_dtm[1,]
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
	after_dtm <- create_matrix(data$AFTER, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE)
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
			data$CHAR_DIST, data$WORD_DIST)

	ddi <- data$DDI

	#### binarize target values

	ddi[ddi != -1] <- 1
	ddi[ddi == -1] <- 0
	container <- create_container(features,ddi,trainSize=1:nrow(data),virgin=FALSE)

	svm.model.binary <- train_models(container, algorithms=c("SVM"), method = "C-classification")

	return (list(
		model=svm.model.binary,
		dictionaries=list(
			o_before_dtm=o_before_dtm,
			o_between_dtm=o_between_dtm,
			o_after_dtm=o_after_dtm,
			o_p_before_dtm=o_p_before_dtm,
			o_p_between_dtm=o_p_between_dtm,
			o_p_after_dtm=o_p_after_dtm
		)
	))
}

ltn.train_multiclass <- function(data,dictionaries) {
	data <- data[data$DDI != -1,]

	o_before_dtm=dictionaries$o_before_dtm
	o_between_dtm=dictionaries$o_between_dtm
	o_after_dtm=dictionaries$o_after_dtm
	o_p_before_dtm=dictionaries$o_p_before_dtm
	o_p_between_dtm=dictionaries$o_p_between_dtm
	o_p_after_dtm=dictionaries$o_p_after_dtm

	before_dtm <- create_matrix(data$BEFORE, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE, originalMatrix=o_before_dtm)
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
	between_dtm <- create_matrix(data$BETWEEN, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE, originalMatrix=o_between_dtm)
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
	after_dtm <- create_matrix(data$AFTER, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE, originalMatrix=o_after_dtm)
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
			data$CHAR_DIST, data$WORD_DIST)

	ddi <- data$DDI

	container <- create_container(features,ddi,trainSize=1:nrow(data),virgin=FALSE)

	svm.model.classes <- train_models(container, algorithms=c("SVM"), method = "C-classification")
	return (svm.model.classes)
}

ltn.predict <- function(svm.model.binary, svm.model.classes, data, dictionaries) {
	o_before_dtm=dictionaries$o_before_dtm
	o_between_dtm=dictionaries$o_between_dtm
	o_after_dtm=dictionaries$o_after_dtm
	o_p_before_dtm=dictionaries$o_p_before_dtm
	o_p_between_dtm=dictionaries$o_p_between_dtm
	o_p_after_dtm=dictionaries$o_p_after_dtm

	before_dtm <- create_matrix(data$BEFORE, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE, originalMatrix=o_before_dtm)
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
	between_dtm <- create_matrix(data$BETWEEN, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE, originalMatrix=o_between_dtm)
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
	after_dtm <- create_matrix(data$AFTER, minWordLength=1, removeStopwords=FALSE, weighting=tm::weightTfIdf, removePunctuation=FALSE, originalMatrix=o_after_dtm)
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
			data$CHAR_DIST, data$WORD_DIST)

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

	return (results)
}

ltn.precision <- function(data, type) {
	predicted = data[,2]
	actual = data[,1]
	tp = length(which(predicted == type & actual == type))
	fp = length(which(predicted == type & actual != type))
	return(tp/(tp+fp))
}

ltn.recall <- function(data, type) {
	predicted = data[,2]
	actual = data[,1]
	tp = length(which(predicted == type & actual == type))
	fn = length(which(predicted != type & actual == type))
	return(tp/(tp+fn))
}

ltn.precision.collection <- function(data) {
	types = sort(unique(data[,1]))
	result = matrix(ncol=4, nrow=0)
	for (type in types) {
		p = ltn.precision(data, type)
		r = ltn.recall(data, type)
		f = 2*p*r/(p+r)

		result <- rbind (result, c(type,round(p,2),round(r,2),round(f,2)))
	}
	result <- as.data.frame(result)
	colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE')
	return(result)
}

# path = '/home/johannes/code/masterproject/data/data.csv'
path = '/Users/mariyaperchyk/Documents/python_hana/analysis/rData/data.csv'


################MULTIPLE ITERATIONS
result = matrix(ncol=4, nrow=0)
colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE')

for(i in 1:10){
	print ("------------------------")
	print (i)
	splitData <- ltn.splitCopurs(path)
	train_data = splitData$train_data
	test_data = splitData$test_data

	print('training binary')
	binary <- ltn.train_binary(train_data)
	svm.model.binary = binary$model
	dictionaries = binary$dictionaries

	print('training multiclass')
	svm.model.classes <- ltn.train_multiclass(train_data, dictionaries)

	print('predicting')
	predicted <- ltn.predict(svm.model.binary, svm.model.classes, test_data[,-1], dictionaries)
	predicted <- predicted[,1]
	actual <- test_data[,1]

	a <- ltn.precision.collection(cbind(actual, as.data.frame(predicted)))
	a
	result = rbind(result,a)
}

aggregate(. ~ TYPE, data= result, FUN="mean")

