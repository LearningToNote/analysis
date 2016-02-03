SET SCHEMA LTN_TRAIN;

DROP TYPE T_MODELS;
CREATE TYPE T_MODELS AS TABLE (ID INTEGER, DESCRIPTION VARCHAR(255), MODEL BLOB);
DROP TABLE MODELS;
CREATE COLUMN TABLE MODELS LIKE T_MODELS;

DROP TYPE T_TRAIN_RESULTS;
CREATE TYPE T_TRAIN_RESULTS AS TABLE (DDI VARCHAR(255), E1_ID VARCHAR(255), E2_ID VARCHAR(255));

DROP TYPE T_R_STAT;
CREATE TYPE T_R_STAT AS TABLE (NAME VARCHAR(255), VALUE DOUBLE);


DROP PROCEDURE R_TRAIN;
CREATE PROCEDURE R_TRAIN(IN data T_TD_CLASSES, OUT result T_MODELS)
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
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

	between_Corpus = Corpus(VectorSource(data$BETWEEN))
	between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE))
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

	after_Corpus = Corpus(VectorSource(data$AFTER))
	after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE))
	colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

	extracted_features <- cbind(before_dtm, between_dtm, after_dtm)

	ddi <- data$DDI
	ddi <- as.factor(ddi)

	svm.model <- svm(x = extracted_features, y=ddi, type="C-classification", cost = 8, gamma = 0.5)

	result <- data.frame(ID = c(1), DESCRIPTION = c('DEINE MUDDA'), MODEL = generateRobjColumn(svm.model))
END;



DROP PROCEDURE R_PREDICT;
CREATE PROCEDURE R_PREDICT(IN data T_PREDICT_INPUT, IN modeltable T_MODELS, OUT result T_TRAIN_RESULTS, OUT stat T_R_STAT)
LANGUAGE RLANG AS
BEGIN
	library(tm)
	library(e1071)
	library(SparseM)

	before_Corpus = Corpus(VectorSource(data$BEFORE))
	before_dtm <- DocumentTermMatrix(before_Corpus, control = list(removeStopwords=FALSE))
	colnames(before_dtm) <- paste("b", colnames(before_dtm), sep = "_")

	between_Corpus = Corpus(VectorSource(data$BETWEEN))
	between_dtm <- DocumentTermMatrix(between_Corpus, control = list(removeStopwords=FALSE))
	colnames(between_dtm) <- paste("i", colnames(between_dtm), sep = "_")

	after_Corpus = Corpus(VectorSource(data$AFTER))
	after_dtm <- DocumentTermMatrix(after_Corpus, control = list(removeStopwords=FALSE))
	colnames(after_dtm) <- paste("a", colnames(after_dtm), sep = "_")

	extracted_features <- cbind(before_dtm, between_dtm, after_dtm)

	print(as.matrix(extracted_features[1,1:5]))

	svm.model <- unserialize(modeltable$MODEL[[1]])

	print(summary(svm.model))


	svm.pred <- predict(svm.model, extracted_features)
	result<-cbind(svm.pred, data[,c(1,2)])
	print(dim(result))

	result <-as.data.frame(as.matrix(result))
	print(dim(result))
	colnames(result) <- c("DDI", "E1_ID", "E2_ID")

	#conf <- table(svm.pred,ddi[testindex])
	#accuracy <- (conf[1,1] + conf[2,2] + conf[3,3] + conf[4,4] + conf[5,5]) / sum(conf)
	#stat <-as.data.frame(matrix(c("accuracy", accuracy), nrow=1, ncol=2) )
	stat <-as.data.frame(matrix(c("example", 1.0), nrow=1, ncol=2) )
	colnames(stat) <- c("NAME", "VALUE")
END;

