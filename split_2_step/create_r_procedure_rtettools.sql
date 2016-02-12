SET SCHEMA LTN_TRAIN;

DROP TYPE T_MODELS;
CREATE TYPE T_MODELS AS TABLE (
	ID INTEGER,
	DESCRIPTION VARCHAR(255),
	MODEL BLOB,
	DICT_BEFORE BLOB,
	DICT_BETWEEN BLOB,
	DICT_AFTER BLOB
);
DROP TABLE MODELS;
CREATE COLUMN TABLE MODELS LIKE T_MODELS;

DROP TYPE T_RESULTS;
CREATE TYPE T_RESULTS AS TABLE (DDI VARCHAR(255), E1_ID VARCHAR(255), E2_ID VARCHAR(255));

DROP TYPE T_R_STAT;
CREATE TYPE T_R_STAT AS TABLE (NAME VARCHAR(255), VALUE DOUBLE);


DROP PROCEDURE R_TRAIN_BINARY;
CREATE PROCEDURE R_TRAIN_BINARY(IN data T_TD_CLASSES, OUT model T_MODELS)
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

	#### down sampling
	true_pairs <- data[data$DDI != "NONE",]
	false_pairs <- data[data$DDI == "NONE",]

	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
	false_downsampled <- false_pairs[false_downsampled_index,]

	data<-rbind(true_pairs, false_downsampled)

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

	container <- create_container(features,as.numeric(as.factor(data$DDI)),trainSize=1:nrow(data),virgin=FALSE)

	models <- train_models(container, algorithms=c("SVM"), method = "C-classification")

	model <- data.frame(
		ID = c(1),
		DESCRIPTION = c('binary classification'),
		MODEL = generateRobjColumn(svm.model),
		DICT_BEFORE = generateRobjColumn(o_before_dtm),
		DICT_BETWEEN = generateRobjColumn(o_between_dtm),
		DICT_AFTER = generateRobjColumn(o_after_dtm)
	)
END;


DROP PROCEDURE R_PREDICT;
CREATE PROCEDURE R_PREDICT(IN data T_PREDICT_INPUT, IN modeltable T_MODELS, OUT result T_RESULTS)
LANGUAGE RLANG AS
BEGIN

	library(RTextTools)


	#### retrieve dictionaries
	o_before_dtm <- unserialize(modeltable$DICT_BEFORE[[1]])
	o_between_dtm <- unserialize(modeltable$DICT_BETWEEN[[1]])
	o_after_dtm <- unserialize(modeltable$DICT_AFTER[[1]])

	#### retrieve models
	models <- unserialize(modeltable$MODEL[[1]])

	#### feature extraction
	before_dtm <- create_matrix(data$BEFORE, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_before_dtm)
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")
	between_dtm <- create_matrix(data$BETWEEN, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_between_dtm)
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")
	after_dtm <- create_matrix(data$AFTER, minWordLength=2, removeStopwords=FALSE, weighting=tm::weightTfIdf, originalMatrix=o_after_dtm)
	colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

	features <- cbind(before_dtm, between_dtm, after_dtm)

	container <- create_container(features,labels=rep(0, nrow(data)),testSize=1:nrow(data),virgin=FALSE)

	results <- classify_models(container,models)

	result<-cbind(results, data[,c(1,2)])


	result <-as.data.frame(result)
	colnames(result) <- c("DDI", "E1_ID", "E2_ID")
END;

