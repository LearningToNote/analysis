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


DROP PROCEDURE R_TRAIN;
CREATE PROCEDURE R_TRAIN(IN data T_TD_CLASSES, OUT model T_MODELS)
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

	true_pairs <- data[data$DDI != "NONE",]
	false_pairs <- data[data$DDI == "NONE",]

	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
	false_downsampled <- false_pairs[false_downsampled_index,]

	data<-rbind(true_pairs, false_downsampled)

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

	svm.model <- svm(x = extracted_features, y=ddi, type="C-classification", cost = 8, gamma = 0.5)



	model <- data.frame(
		ID = c(1),
		DESCRIPTION = c('DEINE MUDDA'),
		MODEL = generateRobjColumn(svm.model),
		DICT_BEFORE = generateRobjColumn(dict_before),
		DICT_BETWEEN = generateRobjColumn(dict_between),
		DICT_AFTER = generateRobjColumn(dict_after)
	)
END;


DROP PROCEDURE R_PREDICT;
CREATE PROCEDURE R_PREDICT(IN data T_TD_CLASSES, IN modeltable T_MODELS, OUT result T_TRAIN_RESULTS, OUT stat T_R_STAT)
LANGUAGE RLANG AS
BEGIN
	library(tm)
	library(e1071)
	library(SparseM)

	true_pairs <- data[data$DDI != "NONE",]
	false_pairs <- data[data$DDI == "NONE",]
	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
	false_downsampled <- false_pairs[false_downsampled_index,]
	data<-rbind(true_pairs, false_downsampled)

	index <- 1:nrow(data)
	testindex <- sample(index, trunc(length(index)/10))
	data <- data[testindex,]



	start_time <- proc.time()


	dict_before <- unserialize(modeltable$DICT_BEFORE[[1]])
	dict_between <- unserialize(modeltable$DICT_BETWEEN[[1]])
	dict_after <- unserialize(modeltable$DICT_AFTER[[1]])

	print('serialize:')
	print(proc.time() - start_time)
	start_time <- proc.time()


	before_Corpus = Corpus(VectorSource(data$BEFORE))
	before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_before))
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

	between_Corpus = Corpus(VectorSource(data$BETWEEN))
	between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_between))
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

	after_Corpus = Corpus(VectorSource(data$AFTER))
	after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE, dictionary=dict_after))
	colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

	print('feature extraction:')
	print(proc.time() - start_time)
	start_time <- proc.time()


	extracted_features <- cbind(before_dtm, between_dtm, after_dtm)

	svm.model <- unserialize(modeltable$MODEL[[1]])

	print('model unpacking:')
	print(proc.time() - start_time)
	start_time <- proc.time()

	svm.pred <- predict(svm.model, extracted_features)
	print('prediction:')
	print(proc.time() - start_time)
	start_time <- proc.time()
	-- result<-cbind(svm.pred, data[,c(1,2)])
	result<-cbind(svm.pred, data[,c(2,3)])


	result <-as.data.frame(as.matrix(result))
	colnames(result) <- c("DDI", "E1_ID", "E2_ID")

	#conf <- table(svm.pred,ddi[testindex])
	#accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)
	#stat <-as.data.frame(matrix(c("accuracy", accuracy), nrow=1, ncol=2) )
	stat <-as.data.frame(matrix(c("example", 1.0), nrow=1, ncol=2) )
	colnames(stat) <- c("NAME", "VALUE")
END;

