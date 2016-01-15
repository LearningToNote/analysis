SET SCHEMA LEARNING_TO_NOTE;

DROP PROCEDURE R_TEST;
DROP PROCEDURE CALL_R;

DROP TYPE T_DDI_DATA;
DROP TABLE TRAIN_DATA;
DROP TABLE TRAIN_RESULT;


CREATE TYPE T_DDI_DATA AS TABLE (DDI TINYINT, BEFORE TEXT, BETWEEN TEXT, AFTER TEXT);

CREATE COLUMN TABLE TRAIN_RESULT LIKE T_DDI_DATA;
CREATE COLUMN TABLE TRAIN_DATA LIKE T_DDI_DATA;

INSERT INTO LEARNING_TO_NOTE.TRAIN_DATA
SELECT DDI,
STRING_AGG(T1, ' ') AS BEFORE,
STRING_AGG(T2, ' ') AS BETWEEN,
STRING_AGG(T3, ' ') AS AFTER
FROM (
	SELECT E1_ID, E2_ID, DDI, 
	STRING_AGG(T1, ' ' ORDER BY COUNTER) AS T1,
	STRING_AGG(T2, ' ' ORDER BY COUNTER) AS T2,
	STRING_AGG(T3, ' ' ORDER BY COUNTER) AS T3
	FROM(
	
	SELECT E1_ID, E2_ID, DDI,
	CASE WHEN POSITION < 0 THEN TOKEN ELSE NULL END AS T1,
	CASE WHEN POSITION = 0 THEN TOKEN ELSE NULL END AS T2,
	CASE WHEN POSITION > 0 THEN TOKEN ELSE NULL END AS T3,
	COUNTER,
	POSITION
	FROM (
		SELECT E1_ID, E2_ID, DDI,
		CASE WHEN FTI.TA_STEM IS NULL THEN FTI.TA_NORMALIZED ELSE FTI.TA_STEM END AS TOKEN,
		CASE
		    WHEN FTI.TA_COUNTER < FTI1.TA_COUNTER THEN -1
		    WHEN (FTI.TA_COUNTER > FTI1.TA_COUNTER AND FTI.TA_COUNTER < FTI2.TA_COUNTER) THEN 0
		    WHEN FTI.TA_COUNTER > FTI2.TA_COUNTER THEN 1
		END AS POSITION,
		FTI.TA_COUNTER AS COUNTER
		FROM LEARNING_TO_NOTE.PAIRS P
		JOIN LEARNING_TO_NOTE.ENTITIES E1 ON P.E1_ID = E1.ID
		JOIN LEARNING_TO_NOTE.ENTITIES E2 ON P.E2_ID = E2.ID
		JOIN LEARNING_TO_NOTE.USER_DOCUMENTS UD ON E1.USER_DOC_ID = UD.ID AND E2.USER_DOC_ID = UD.ID
		JOIN LEARNING_TO_NOTE.OFFSETS O1 ON O1.ENTITY_ID = E1.ID
		JOIN LEARNING_TO_NOTE.OFFSETS O2 ON O2.ENTITY_ID = E2.ID
		JOIN LEARNING_TO_NOTE."$TA_FTI" FTI1 ON FTI1.ID = UD.DOCUMENT_ID AND FTI1.TA_OFFSET = O1."START" AND FTI1.TA_TOKEN = E1.TEXT
		JOIN LEARNING_TO_NOTE."$TA_FTI" FTI2 ON FTI2.ID = UD.DOCUMENT_ID AND FTI2.TA_OFFSET = O2."START" AND FTI2.TA_TOKEN = E2.TEXT
		JOIN LEARNING_TO_NOTE."$TA_FTI" FTI ON FTI.ID = UD.DOCUMENT_ID
		WHERE UD.USER_ID = 'DDI-IMPORTER'
		AND FTI.TA_TYPE <> 'punctuation'
		AND FTI1.TA_SENTENCE = FTI2.TA_SENTENCE
		AND FTI.TA_SENTENCE = FTI1.TA_SENTENCE
		AND FTI.TA_COUNTER <> FTI1.TA_COUNTER
		AND FTI.TA_COUNTER <> FTI2.TA_COUNTER
		AND FTI1.TA_COUNTER < FTI2.TA_COUNTER
		
		ORDER BY FTI.TA_COUNTER
		)
	)
	
	GROUP BY E1_ID, E2_ID, DDI, POSITION
	ORDER BY E1_ID, E2_ID, POSITION
)
GROUP BY E1_ID, E2_ID, DDI
;


CREATE PROCEDURE R_TEST(IN train_data TRAIN_DATA, OUT result T_DDI_DATA)
LANGUAGE RLANG AS
BEGIN

	library(e1071)
	library(qdap)
	
	bag.make = function(x, n = 100000000000){
	
	    bag = bag_o_words(x)
	    bag = sort(table(bag), dec = TRUE)
	    n = min(n, length(bag))
	    counts = bag[1:n]
	    bag = names(bag)[1:n]
	
	    b = list(items = bag, counts = counts,
	
	        apply = function(targ){
	            target = apply(data.frame(targ), 1, bag_o_words)
	
	            #idx = apply(target, 2, match, bag)
	            m = matrix(0, nrow = length(targ), ncol=length(bag))
	            #i ~ row, j ~ column
	            for(i in 1:length(targ)) {
	                for (j in 1:length(bag)) {
	                    if (bag[j] %in% unlist(target[i])) {
	                        m[i,j] = 1
	                    } else {
	                        m[i,j] = 0
	                    }
	
	                }
	            }
	            #sapply(1:nrow(m), function(r){
	            #    m[r, idx[r,]] <<- 1
	            #})
	            colnames(m) = bag
	            m
	        })
	    class(b) = 'bag'
	    b
	}
	
	
	train_data = read.csv("~/Downloads/coolData.csv")
	
	true_pairs <- train_data[train_data$DDI == 1,]
	false_pairs <- train_data[train_data$DDI == 0,]
	
	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
	false_downsampled <- false_pairs[false_downsampled_index,]
	
	all<-rbind(true_pairs, false_downsampled)
	
	bag <- bag.make(all$BEFORE)
	
	extracted_features <- bag$apply(all$BEFORE)
	
	index <- 1:nrow(all)
	testindex <- sample(index, trunc(length(index)/3))
	testset <- all[testindex,]
	trainset <- all[-testindex,]
	
	svm.model <- svm(DDI ~ ., data = trainset, type="C-classification")
	svm.pred <- predict(svm.model, testset[,-1])
	
	pred <-as.data.frame(svm.pred)
	result<-cbind(pred[,1], testset[,-1])

    colnames(result) <- c("DDI", "BEFORE", "BETWEEN", "AFTER")

END;

CREATE PROCEDURE CALL_R()
LANGUAGE SQLSCRIPT AS
BEGIN
    train_data = SELECT * FROM TRAIN_DATA;
    CALL R_TEST(:train_data, T_DDI_DATA);
    INSERT INTO TRAIN_RESULT SELECT * FROM :T_DDI_DATA;
END;
 
CALL CALL_R();
SELECT ddi, count(*) FROM TRAIN_RESULT group by ddi;

