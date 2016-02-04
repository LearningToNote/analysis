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

DROP TYPE T_TRAIN_RESULTS;
CREATE TYPE T_TRAIN_RESULTS AS TABLE (DDI VARCHAR(255), E1_ID VARCHAR(255), E2_ID VARCHAR(255));

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

	library(tm)
	library(e1071)
	library(SparseM)

	#### down sampling
	true_pairs <- data[data$DDI != "NONE",]
	false_pairs <- data[data$DDI == "NONE",]

	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
	false_downsampled <- false_pairs[false_downsampled_index,]

	data<-rbind(true_pairs, false_downsampled)

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

	svm.model <- svm(x = extracted_features, y=ddi, type="C-classification", cost = 8, gamma = 0.5)


	model <- data.frame(
		ID = c(1),
		DESCRIPTION = c('binary classification'),
		MODEL = generateRobjColumn(svm.model),
		DICT_BEFORE = generateRobjColumn(dict_before),
		DICT_BETWEEN = generateRobjColumn(dict_between),
		DICT_AFTER = generateRobjColumn(dict_after)
	)
END;




DROP PROCEDURE R_TRAIN_CLASSES;
CREATE PROCEDURE R_TRAIN_CLASSES(IN data T_TD_CLASSES, IN modeltable T_MODELS, OUT model T_MODELS)
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

	library(tm)
	library(e1071)
	library(SparseM)


	#### remove remaining NONE pairs

	data <- data[data$DDI != "NONE",]

	#### retrieve dictionaries from binary classification
	dict_before <- unserialize(modeltable$DICT_BEFORE[[1]])
	dict_between <- unserialize(modeltable$DICT_BETWEEN[[1]])
	dict_after <- unserialize(modeltable$DICT_AFTER[[1]])

	#### feature extraction

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

	ddi <- data$DDI
	ddi <- as.factor(ddi)

	svm.model <- svm(x = extracted_features, y=ddi, type="C-classification", cost = 8, gamma = 0.5)


	model <- data.frame(
		ID = c(2),
		DESCRIPTION = c('multi class classification'),
		MODEL = generateRobjColumn(svm.model),
		DICT_BEFORE = generateRobjColumn(dict_before),
		DICT_BETWEEN = generateRobjColumn(dict_between),
		DICT_AFTER = generateRobjColumn(dict_after)
	)
END;



DROP PROCEDURE R_PREDICT;
CREATE PROCEDURE R_PREDICT(IN data T_PREDICT_INPUT, IN modeltable T_MODELS, OUT result T_TRAIN_RESULTS, OUT stat T_R_STAT)
LANGUAGE RLANG AS
BEGIN
	library(tm)
	library(e1071)
	library(SparseM)


	#### retrieve dictionaries
	dict_before <- unserialize(modeltable$DICT_BEFORE[[1]])
	dict_between <- unserialize(modeltable$DICT_BETWEEN[[1]])
	dict_after <- unserialize(modeltable$DICT_AFTER[[1]])

	#### retrieve models
	svm.model.binary <- unserialize(modeltable$MODEL[[1]])
	svm.model.classes <- unserialize(modeltable$MODEL[[2]])

	#### feature extraction
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


	#### binary classification
	svm.pred.binary <- predict(svm.model.binary, extracted_features)
	result<-cbind(svm.pred.binary, data[,c(1,2)])

	colnames(result)[1] <- "DDI"
	levels(result$DDI) <- c(levels(result$DDI), 'advise','int','mechanism','effect', 'NONE')

	#### multi classification where binary classified as true
	true_index <- which(svm.pred.binary == TRUE)
	svm.pred.classes <- predict(svm.model.classes, extracted_features[true_index,])

	#### rename classes
	result$DDI[result$DDI == FALSE] <- 'NONE'
	result[true_index,]$DDI <- svm.pred.classes

	result <- droplevels(result)



	result <-as.data.frame(result)
	colnames(result) <- c("DDI", "E1_ID", "E2_ID")

	stat <-as.data.frame(matrix(c("example", 1.0), nrow=1, ncol=2) )
	colnames(stat) <- c("NAME", "VALUE")
END;

