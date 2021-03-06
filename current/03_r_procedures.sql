SET SCHEMA LTN_DEVELOP;


DROP PROCEDURE R_TRAIN_BINARY;
CREATE PROCEDURE R_TRAIN_BINARY(IN data T_TRAIN_DATA, IN task_id T_INT, OUT model T_MODELS)
LANGUAGE RLANG AS
BEGIN
	########################
	generateRobjColumn <- function(...){
        result <- as.data.frame(cbind(
            lapply(
                list(...),
                function(x) if (is.null(x)) NULL else serialize(x, NULL)
            )
        ))
        names(result) <- NULL
        names(result[[1]]) <- NULL
        result
    }
	########################

	library(RTextTools)

	########################
	ddi <- data$DDI

	if (length(unique(ddi)) > 1) {

		#### binarize target values

		ddi[ddi != -1] <- 1
		ddi[ddi == -1] <- 0

		#### feature extraction

		before_dtm <- create_matrix(
			data$BEFORE,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf)
		o_before_dtm <- before_dtm[1,]
		colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
		between_dtm <- create_matrix(
			data$BETWEEN,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf)
		o_between_dtm <- between_dtm[1,]
		colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
		after_dtm <- create_matrix(
			data$AFTER,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf)
		o_after_dtm <- after_dtm[1,]
		colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

		p_before_dtm <- create_matrix(
			data$P_BEFORE,
			minWordLength=1,
			removeStopwords=FALSE)
		o_p_before_dtm <- p_before_dtm[1,]
		colnames(p_before_dtm) <- paste("pb", colnames(p_before_dtm), sep = "_")

		p_between_dtm <- create_matrix(
			data$P_BETWEEN,
			minWordLength=1,
			removeStopwords=FALSE)
		o_p_between_dtm <- p_between_dtm[1,]
		colnames(p_between_dtm) <- paste("pi", colnames(p_between_dtm), sep = "_")

		p_after_dtm <- create_matrix(
			data$P_AFTER,
			minWordLength=1,
			removeStopwords=FALSE)
		o_p_after_dtm <- p_after_dtm[1,]
		colnames(p_after_dtm) <- paste("pa", colnames(p_after_dtm), sep = "_")

		features <- cbind(
			before_dtm, between_dtm, after_dtm,
			p_before_dtm, p_between_dtm, p_after_dtm,
			data$E1_TYPE, data$E2_TYPE,
			data$CHAR_DIST, data$WORD_DIST)

		container <- create_container(features,ddi,trainSize=1:nrow(data),virgin=FALSE)

		svm.model.binary <- train_models(container, algorithms=c("SVM"), method = "C-classification")

		model <- data.frame(
			TASK_ID=task_id[1,1],
			ID = c(1),
			DESCRIPTION = c('binary classification'),
			MODEL = generateRobjColumn(svm.model.binary),
			MATRIX_BEFORE = generateRobjColumn(o_before_dtm),
			MATRIX_BETWEEN = generateRobjColumn(o_between_dtm),
			MATRIX_AFTER = generateRobjColumn(o_after_dtm),
			MATRIX_P_BEFORE = generateRobjColumn(o_p_before_dtm),
			MATRIX_P_BETWEEN = generateRobjColumn(o_p_between_dtm),
			MATRIX_P_AFTER = generateRobjColumn(o_p_after_dtm),
			CREATED_AT = generateRobjColumn(strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"))
		)
	} else {
		model <- data.frame(
			TASK_ID=task_id[1,1],
			ID = c(1),
			DESCRIPTION = c('empty binary model'),
			MODEL = generateRobjColumn(NULL),
			MATRIX_BEFORE = generateRobjColumn(NULL),
			MATRIX_BETWEEN = generateRobjColumn(NULL),
			MATRIX_AFTER = generateRobjColumn(NULL),
			MATRIX_P_BEFORE = generateRobjColumn(NULL),
			MATRIX_P_BETWEEN = generateRobjColumn(NULL),
			MATRIX_P_AFTER = generateRobjColumn(NULL),
			CREATED_AT = generateRobjColumn(strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"))
		)
	}

END;


DROP PROCEDURE R_TRAIN_CLASSES;
CREATE PROCEDURE R_TRAIN_CLASSES(IN data T_TRAIN_DATA, IN modeltable T_MODELS, IN task_id T_INT, OUT model T_MODELS)
LANGUAGE RLANG AS
BEGIN
	########################
	generateRobjColumn <- function(...){
        result <- as.data.frame(cbind(
            lapply(
                list(...),
                function(x) if (is.null(x)) NULL else serialize(x, NULL)
            )
        ))
        names(result) <- NULL
        names(result[[1]]) <- NULL
        result
    }
	########################

	if (is.null(modeltable$MODEL[[1]])) {
		model <- data.frame(
			TASK_ID=task_id[1,1],
			ID = c(2),
			DESCRIPTION = c('empty multiclass model'),
			MODEL = generateRobjColumn(NULL),
			MATRIX_BEFORE = generateRobjColumn(NULL),
			MATRIX_BETWEEN = generateRobjColumn(NULL),
			MATRIX_AFTER = generateRobjColumn(NULL),
			MATRIX_P_BEFORE = generateRobjColumn(NULL),
			MATRIX_P_BETWEEN = generateRobjColumn(NULL),
			MATRIX_P_AFTER = generateRobjColumn(NULL),
			CREATED_AT = generateRobjColumn(strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"))
		)
	} else {

		library(RTextTools)

		#### retrieve dictionaries from binary classification
		o_before_dtm <- unserialize(modeltable$MATRIX_BEFORE[[1]])
		o_between_dtm <- unserialize(modeltable$MATRIX_BETWEEN[[1]])
		o_after_dtm <- unserialize(modeltable$MATRIX_AFTER[[1]])
		o_p_before_dtm <- unserialize(modeltable$MATRIX_P_BEFORE[[1]])
		o_p_between_dtm <- unserialize(modeltable$MATRIX_P_BETWEEN[[1]])
		o_p_after_dtm <- unserialize(modeltable$MATRIX_P_AFTER[[1]])

		#### feature extraction

		before_dtm <- create_matrix(
			data$BEFORE,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf, originalMatrix=o_before_dtm)
		colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
		between_dtm <- create_matrix(
			data$BETWEEN,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf,
			originalMatrix=o_between_dtm)
		colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
		after_dtm <- create_matrix(
			data$AFTER,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf,
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

		ddi <- data$DDI

		container <- create_container(features,ddi,trainSize=1:nrow(data),virgin=FALSE)

		svm.model.classes <- train_models(container, algorithms=c("SVM"), method = "C-classification")


		model <- data.frame(
			TASK_ID=task_id[1,1],
			ID = c(2),
			DESCRIPTION = c('multi class classification'),
			MODEL = generateRobjColumn(svm.model.classes),
			MATRIX_BEFORE = generateRobjColumn(NULL),
			MATRIX_BETWEEN = generateRobjColumn(NULL),
			MATRIX_AFTER = generateRobjColumn(NULL),
			MATRIX_P_BEFORE = generateRobjColumn(NULL),
			MATRIX_P_BETWEEN = generateRobjColumn(NULL),
			MATRIX_P_AFTER = generateRobjColumn(NULL),
			CREATED_AT = generateRobjColumn(strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"))
		)
	}
END;



DROP PROCEDURE R_PREDICT;
CREATE PROCEDURE R_PREDICT(IN data T_PREDICT_INPUT, IN modeltable T_MODELS, OUT result T_RESULTS)
LANGUAGE RLANG AS
BEGIN

	if(nrow(data) > 0 && !is.null(modeltable$MODEL[[1]])) {

		library(RTextTools)

		#### retrieve dictionaries
		o_before_dtm <- unserialize(modeltable$MATRIX_BEFORE[[1]])
		o_between_dtm <- unserialize(modeltable$MATRIX_BETWEEN[[1]])
		o_after_dtm <- unserialize(modeltable$MATRIX_AFTER[[1]])
		o_p_before_dtm <- unserialize(modeltable$MATRIX_P_BEFORE[[1]])
		o_p_between_dtm <- unserialize(modeltable$MATRIX_P_BETWEEN[[1]])
		o_p_after_dtm <- unserialize(modeltable$MATRIX_P_AFTER[[1]])

		#### retrieve models
		svm.model.binary <- unserialize(modeltable$MODEL[[1]])
		svm.model.classes <- unserialize(modeltable$MODEL[[2]])

		#### feature extraction
		before_dtm <- create_matrix(
			data$BEFORE,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf,
			originalMatrix=o_before_dtm)
		colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
		between_dtm <- create_matrix(
			data$BETWEEN,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf,
			originalMatrix=o_between_dtm)
		colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
		after_dtm <- create_matrix(
			data$AFTER,
			minWordLength=1,
			removePunctuation=FALSE,
			removeStopwords=FALSE,
			weighting=tm::weightTfIdf,
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

		#### binary classification
		## creates results 1: relation, 0: no relation

		container <- create_container(features,labels=rep(0, nrow(data)),testSize=1:nrow(data),virgin=FALSE)

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

		result <-as.data.frame(cbind(results[,1], data[,c(1,2)]))
		colnames(result) <- c("DDI", "E1_ID", "E2_ID")
	} else {
		result <- data.frame(DDI=integer(), E1_ID=character(), E2_ID=character())
	}

END;

