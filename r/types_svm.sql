SET SCHEMA LEARNING_TO_NOTE;

DROP PROCEDURE R_TEST;
DROP PROCEDURE CALL_R;

DROP TYPE T_DDI_DATA;
DROP TABLE TRAIN_DATA;
DROP TABLE TRAIN_RESULT;


CREATE TYPE T_DDI_DATA AS TABLE (DDI TINYINT, E1_TYPE INT, E2_TYPE INT);

CREATE COLUMN TABLE TRAIN_DATA LIKE T_DDI_DATA;
CREATE COLUMN TABLE TRAIN_RESULT LIKE T_DDI_DATA;

INSERT INTO TRAIN_DATA
SELECT 
  DDI, E1.TYPE_ID AS E1_TYPE, E2.TYPE_ID AS E2_TYPE
  FROM LEARNING_TO_NOTE.PAIRS P
  JOIN LEARNING_TO_NOTE.ENTITIES E1 ON P.E1_ID = E1.ID
  JOIN LEARNING_TO_NOTE.ENTITIES E2 ON P.E2_ID = E2.ID
;


CREATE PROCEDURE R_TEST(IN train_data TRAIN_DATA, OUT result T_DDI_DATA)
LANGUAGE RLANG AS
BEGIN
    library(e1071)
    
    true_pairs <- train_data[train_data$DDI == 1,]
    false_pairs <- train_data[train_data$DDI == 0,]
    
    false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
    false_downsampled<-false_pairs[false_downsampled_index,]
    
    all<-rbind(true_pairs, false_downsampled)
    
    index <- 1:nrow(all)
    testindex <- sample(index, trunc(length(index)/3))
    testset <- all[testindex,]
    trainset <- all[-testindex,]

    svm.model <- svm(DDI ~ ., data = trainset, type="C-classification")
    svm.pred <- predict(svm.model, testset[,-1])
    
    pred <-as.data.frame(svm.pred)
    result<-cbind(pred[,1], testset[,-1])    
    colnames(result) <- c("DDI", "E1_TYPE", "E2_TYPE")
   
    

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
