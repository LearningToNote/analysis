
library(RTextTools)

ltn.downsample <- function(data) {
	#### down sampling
	true_pairs <- data[data$DDI != -1,]
	false_pairs <- data[data$DDI == -1,]
	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs) * 1.5)
	false_downsampled <- false_pairs[false_downsampled_index,]

	data <- rbind(true_pairs, false_downsampled)
	return(data)
}

ltn.splitCopurs <- function(path) {
	data <- read.csv(path)

	data <- ltn.downsample(data)

	docs <- unique(data$DOC_ID)
	testdocs <- sample(docs, 20)

	testrows <- which(data$DOC_ID %in% testdocs)
	test_data <- data[testrows,-1]
	train_data <- data[-testrows,-1]

	return (list(train_data=train_data, test_data=test_data))
}

ltn.sampleN <- function(data, n, seed) {
	set.seed(seed)
	docs <- unique(data$DOC_ID)
	docs <- sample(docs, n+1)
	train_docs <- docs[1:n]
	test_docs <- docs[n+1]

	train_rows <- which(data$DOC_ID %in% train_docs)
	test_rows <- which(data$DOC_ID %in% test_docs)

	train_data <- ltn.downsample(data[train_rows,-1])
	test_data <- data[test_rows,-1]

	return (list(train_data=train_data, test_data=test_data))
}

ltn.train_binary <- function(data) {
	#### feature extraction

	before_dtm <- create_matrix(
		data$BEFORE,
		minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE)
	o_before_dtm <- before_dtm[1,]
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
	between_dtm <- create_matrix(
		data$BETWEEN,
		minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE)
	o_between_dtm <- between_dtm[1,]
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
	after_dtm <- create_matrix(
		data$AFTER,
		minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE)
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

	before_dtm <- create_matrix(
		data$BEFORE,
		minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE,
		originalMatrix=o_before_dtm)
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
	between_dtm <- create_matrix(
		data$BETWEEN, minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE,
		originalMatrix=o_between_dtm)
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

	before_dtm <- create_matrix(
		data$BEFORE,
		minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE,
		originalMatrix=o_before_dtm)
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
	between_dtm <- create_matrix(
		data$BETWEEN,
		minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE,
		originalMatrix=o_between_dtm)
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
	after_dtm <- create_matrix(
		data$AFTER,
		minWordLength=1,
		removeStopwords=FALSE,
		weighting=tm::weightTfIdf,
		removePunctuation=FALSE,
		originalMatrix=o_after_dtm)
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
	levels(results$SVM_LABEL) <- c(levels(results$SVM_LABEL), -1)

	if (length(true_index) > 0) {
		container <- create_container(features[true_index,],labels=rep(0, length(true_index)),testSize=1:length(true_index),virgin=FALSE)
		svm.pred.classes <- classify_models(container, svm.model.classes)
		levels(results$SVM_LABEL) <- c(levels(results$SVM_LABEL), levels(svm.pred.classes$SVM_LABEL))

		results[true_index,] <- svm.pred.classes
		results[-true_index,] <- -1
	} else {
		results[,1] <- -1
	}

	results <- droplevels(results)

	return (results)
}

ltn.precision <- function(data, type) {
	predicted = data[,2]
	actual = data[,1]
	tp = length(which(predicted == type & actual == type))
	fp = length(which(predicted == type & actual != type))
	if (tp+fp > 0) {
		return(tp/(tp+fp))
	} else {
		return(0.0)
	}
}

ltn.recall <- function(data, type) {
	predicted = data[,2]
	actual = data[,1]
	tp = length(which(predicted == type & actual == type))
	fn = length(which(predicted != type & actual == type))
	print(data)
	if (tp+fn > 0) {
		return(tp/(tp+fn))
		} else {
			return(0)
		}
}

ltn.precision.collection <- function(data) {
	# types cantained in the actual test data
	types = sort(union(unique(data[,1]), unique(data[,2])))
	result = matrix(ncol=4, nrow=0)
	for (type in types) {
		p = ltn.precision(data, type)
		r = ltn.recall(data, type)
		print(p)
		print(r)
		if (p+r > 0) {
			f = 2*p*r/(p+r)
			} else {
				f = 0
			}

		result <- rbind (result, c(type,round(p,2),round(r,2),round(f,2)))
	}
	result <- as.data.frame(result)
	colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE')
	return(result)
}

# path = '/home/johannes/code/masterproject/data/data.csv'
path = '/Users/mariyaperchyk/Documents/python_hana/analysis/rData/data.csv'


################MULTIPLE ITERATIONS
ltn.iterations <- function(path) {
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

	return(aggregate(. ~ TYPE, data= result, FUN="mean"))
}

############LEARNING CURVE
ltn.learn <- function(path) {
	data <- read.csv(path)
	result = matrix(ncol=4, nrow=0)
	colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE')

	for(i in 1:10){

		print ("------------------------")
		splitData <- ltn.sampleN(data, i, 42)
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
		print(a)
		result = rbind(result,a)
	}
	return(result)
}

plot_f1 <- function(a) {
	plot(1:nrow(a[a$TYPE==-1,]),a[a$TYPE==-1,4],type="l",col="red",xlim=c(1,nrow(a[a$TYPE==-1,])),ylim=c(0,1))
	lines(1:nrow(a[a$TYPE==137,]),a[a$TYPE==137,4],col="green",xlim=c(1,nrow(a[a$TYPE==-1,])),ylim=c(0,1))
	lines(1:nrow(a[a$TYPE==138,]),a[a$TYPE==138,4],col="blue",xlim=c(1,nrow(a[a$TYPE==-1,])),ylim=c(0,1))
	lines(1:nrow(a[a$TYPE==139,]),a[a$TYPE==139,4],col="black",xlim=c(1,nrow(a[a$TYPE==-1,])),ylim=c(0,1))
	lines(1:nrow(a[a$TYPE==140,]),a[a$TYPE==140,4],col="yellow",xlim=c(1,nrow(a[a$TYPE==-1,])),ylim=c(0,1))
}

#a <- ltn.learn(path)
#plot_f1(a)