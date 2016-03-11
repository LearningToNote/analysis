SET SCHEMA LTN_DEVELOP;


DROP PROCEDURE LTN_TRAIN;
CREATE PROCEDURE LTN_TRAIN(IN task_id INT)
LANGUAGE SQLSCRIPT AS
BEGIN
DECLARE binary_model LTN_DEVELOP.T_MODELS;
DECLARE multiclass_model LTN_DEVELOP.T_MODELS;

    CALL LTN_FEATURES(:task_id, -1, :train_data);
    task_id_table = select :task_id AS NUMBER from DUMMY;

    CALL R_TRAIN_BINARY(:train_data, :task_id_table, :binary_model);
    CALL R_TRAIN_CLASSES(:train_data, :binary_model, :task_id_table, :multiclass_model);

    DELETE FROM MODELS WHERE TASK_ID=:task_id;
    INSERT INTO MODELS SELECT * FROM :binary_model;
    INSERT INTO MODELS SELECT * FROM :multiclass_model;

END;


DROP PROCEDURE LTN_PREDICT;
CREATE PROCEDURE LTN_PREDICT(IN task_id INT)
LANGUAGE SQLSCRIPT AS
BEGIN
    CALL LTN_FEATURES_BY_USER(:task_id, 'DDI-TEST_DATA', -1, :data);

    test_data = select
        E1_ID,
        E2_ID,
        E1_TYPE,
        E2_TYPE,
        CHAR_DIST,
        WORD_DIST,
        "BEFORE",
        BETWEEN,
        AFTER,
        P_BEFORE,
        P_BETWEEN,
        P_AFTER
    FROM :data;

    models = SELECT * FROM MODELS;
    CALL R_PREDICT(:test_data, :models, T_RESULTS);

    DELETE FROM RESULTS;
    INSERT INTO RESULTS SELECT * FROM :T_RESULTS;
END;

CALL LTN_TRAIN(1);
SELECT * FROM MODELS;

CALL LTN_PREDICT(1);
SELECT * FROM RESULTS;



----------------------------------------------------------
----------------------------------------------------------



DROP PROCEDURE LTN_PRF;
CREATE PROCEDURE LTN_PRF(IN data COMPARISON_T, OUT evaluation EVALUATION_T)
LANGUAGE RLANG AS
BEGIN
    precision <- function(data, type) {
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

    recall <- function(data, type) {
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

    precision.collection <- function(data) {
        types = sort(unique(data[,1]))
        result = matrix(ncol=4, nrow=0)
        for (type in types) {
            p = precision(data, type)
            r = recall(data, type)
            if (p+r > 0) {
                f = 2*p*r/(p+r)
            } else {
                f = 0/1
            }
            result <- rbind (result, c(type,round(p,2),round(r,2),round(f,2)))
        }
        result <- as.data.frame(result)
        colnames(result) <- c('T', 'PRECISION', 'RECALL', 'F_MEASURE')
        return(result)
    }

    evaluation = precision.collection(data)
END;



DROP PROCEDURE LTN_EVALUATION;
CREATE PROCEDURE LTN_EVALUATION(IN task_id INT, OUT evaluation EVALUATION_T)
LANGUAGE SQLSCRIPT AS
BEGIN

    CALL CREATE_TRAINING_DATA(:task_id, 'DDI-TEST_DATA', -1, :test_data);

    comparison = SELECT TD.DDI AS ACTUAL, R.DDI AS PREDICTED
        FROM :test_data TD
        JOIN RESULTS R ON TD.E1_ID = R.E1_ID AND TD.E2_ID = R.E2_ID;

    CALL LTN_PRF(:comparison, :evaluation);
END;

CALL LTN_EVALUATION(1, ?)

