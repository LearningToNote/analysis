
library(RTextTools)

ltn.downsample <- function(data) {
	#### down sampling
	true_pairs <- data[data$DDI != -1,]
	false_pairs <- data[data$DDI == -1,]
	if (nrow(true_pairs) > 0) {
		false_downsampled_index <- sample(1:nrow(false_pairs), min(nrow(false_pairs), nrow(true_pairs)*1.5) )
		false_downsampled <- false_pairs[false_downsampled_index,]
		data <- rbind(true_pairs, false_downsampled)
		return(data)
	} else {
		return(false_pairs)
	}
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

ltn.sampleN <- function(data, n, seed, number_of_test_docs) {
	set.seed(seed)
	docs <- unique(data$DOC_ID)

	docs <- sample(docs, n+number_of_test_docs)
	train_docs <- docs[1:n]
	test_docs <- docs[(n+1):(n+number_of_test_docs)]

	train_rows <- which(data$DOC_ID %in% train_docs)
	test_rows <- which(data$DOC_ID %in% test_docs)
	train_data <- ltn.downsample(data[train_rows,-1])
	test_data <- data[test_rows,-1]

	return (list(train_data=train_data, test_data=test_data))
}

ltn.train_binary <- function(data) {
	ddi <- data$DDI

	if (length(unique(ddi)) == 1) {
		return(NULL)
	}
	#### binarize target values

	ddi[ddi != -1] <- 1
	ddi[ddi == -1] <- 0

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
	if (length(unique(data$DDI)) <= 1) {
		return(NULL)
	}

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
	if (is.null(svm.model.binary) || is.null(svm.model.classes)) {
		return(NULL)
	}
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
	if (tp+fn > 0) {
		return(tp/(tp+fn))
	} else {
		return(0.0)
	}
}

ltn.micro_average <- function(data,type) {
	predicted = data[,2]
	actual = data[,1]
	tp = length(which(predicted == type & actual == type))
	fp = length(which(predicted == type & actual != type))
	fn = length(which(predicted != type & actual == type))
	tn = length(which(predicted != type & actual != type))
	return (cbind(tp,fp,fn,tn))
}

ltn.precision.collection <- function(data) {
	# types cantained in the actual test data
	types = sort(unique(c(data[,1],data[,2])))
	result = matrix(ncol=8, nrow=0)
	for (type in types) {
		p = ltn.precision(data, type)
		r = ltn.recall(data, type)
		if (p+r > 0) {
			f = 2*p*r/(p+r)
<<<<<<< Updated upstream
		} else {
			f = 0/1
		}
		result <- rbind (result, c(type,round(p,2),round(r,2),round(f,2)))
=======
			} else {
				f = 0/1
			}
		micro = ltn.micro_average(data, type)
		result <- rbind (result, c(type,round(p,2),round(r,2),round(f,2), micro))
>>>>>>> Stashed changes
	}
	result <- as.data.frame(result)
	colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE', 'TP', 'FP', 'FN', 'TN')
	return(result)
}

ltn.micro.collection <- function(data) {

	precision = sum(data$TP) / (sum(data$TP)+sum(data$FP))
	recall = sum(data$TP) / (sum(data$TP)+sum(data$FN))
	return(cbind(precision, recall))
}

# path = '/home/johannes/code/masterproject/data/data.csv'
path = '/Users/mariyaperchyk/Documents/python_hana/analysis/rData/data.csv'
train_path = '/Users/mariyaperchyk/Documents/python_hana/analysis/rData/all_train_data.csv'
test_path = '/Users/mariyaperchyk/Documents/python_hana/analysis/rData/all_test_data.csv'


################MULTIPLE ITERATIONS
ltn.iterations <- function(train_path, test_path, use_testset=FALSE, iterations) {
	all_predictions = matrix(ncol=2, nrow=0)
	colnames(all_predictions) <- c('ACTUAL', 'PREDICTED')

	result = matrix(ncol=8, nrow=0)
	micro = matrix(ncol=2, nrow=0)
	colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE', 'TP', 'FP', 'FN', 'TN')

	for(i in 1:iterations){
		print ("------------------------")
		print (i)
		if(!use_testset) {
			splitData <- ltn.splitCopurs(train_path)
			train_data = splitData$train_data
			test_data = splitData$test_data
		} else {
			train_data = ltn.downsample(read.csv(train_path))
			test_data = read.csv(test_path)
		}

		print('training binary')
		binary <- ltn.train_binary(train_data)
		svm.model.binary = binary$model
		dictionaries = binary$dictionaries

		print('training multiclass')
		svm.model.classes <- ltn.train_multiclass(train_data, dictionaries)

		print('predicting')
		predicted <- ltn.predict(svm.model.binary, svm.model.classes, test_data[,-1], dictionaries)
		predicted <- as.numeric(levels(predicted[,1]))[predicted[,1]]
		actual <- test_data[,1]

		all_predictions <- rbind(all_predictions,cbind(actual, predicted))

		a <- ltn.precision.collection(cbind(actual,predicted))
		micro_collection = ltn.micro.collection(a[,c(5:8)])
		print(a)
		print(micro_collection)

		micro = rbind(micro, micro_collection)
		result = rbind(result,a)

	}
	aggr = aggregate(. ~ TYPE, data= result[,-c(5:8)], FUN="mean")
	aggr_micro = colMeans(micro)
	return(list(perClassAggregation=aggr,microAggregation=aggr_micro))
}

############LEARNING CURVE
ltn.learn <- function(data,number_of_iterations, seed, steps) {
	result = matrix(ncol=5, nrow=0)
	colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE','i')

	for(i in seq(2,number_of_iterations+1, by=steps)){

		print ("------------------------")
		print(i)
		splitData <- ltn.sampleN(data, i, seed, 20)
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
		if (is.null(predicted)) {
			result = rbind(result,c(rep(0,4),i))
			next
		}
		predicted <- predicted[,1]
		if (class(predicted) !='numeric') {
			predicted <- as.numeric(levels(predicted))[predicted]
		}
		actual <- test_data[,1]

		a <- ltn.precision.collection(cbind(actual, predicted))
		a <- cbind(a,i)
		result = rbind(result,a)
	}
	print(result)
	return(result)
}

ltn.aggregate_learn <- function(path, iterations_per_cycle, cycles, seed) {
	# iterations_per_cycle = how many docs at max we use to learn
	# cycles = how often should we execute the learning curve to aggregate
	data <- read.csv(path)
	result = matrix(ncol=5, nrow=0)
	colnames(result) <- c('TYPE', 'PRECISION', 'RECALL', 'F_MEASURE','i')

	for(i in 1:cycles){
		a <- ltn.learn(data,iterations_per_cycle,i+seed, 10)
		result <-rbind(result, a)
	}
	return(aggregate(. ~ TYPE+i, data= result, FUN="mean"))
}

plot_f1 <- function(a, column) {
	plot(a[a$TYPE==-1,2],a[a$TYPE==-1,column],type="l",col="red",xlim=c(min(a$i),max(a$i)),ylim=c(0,1))
	lines(a[a$TYPE==137,2],a[a$TYPE==137,column],col="green",xlim=c(min(a$i),max(a$i)),ylim=c(0,1))
	lines(a[a$TYPE==138,2],a[a$TYPE==138,column],col="blue",xlim=c(min(a$i),max(a$i)),ylim=c(0,1))
	lines(a[a$TYPE==139,2],a[a$TYPE==139,column],col="black",xlim=c(min(a$i),max(a$i)),ylim=c(0,1))
	lines(a[a$TYPE==140,2],a[a$TYPE==140,column],col="yellow",xlim=c(min(a$i),max(a$i)),ylim=c(0,1))
}

# collection = ltn.iterations(train_path, test_path, FALSE, 2)

#a <- ltn.learn(path,2,102)
#plot_f1(a)