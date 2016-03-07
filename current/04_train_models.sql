SET SCHEMA LTN_DEVELOP;

DROP TABLE RESULTS;
CREATE COLUMN TABLE RESULTS LIKE T_RESULTS;


DROP PROCEDURE LTN_TRAIN;
CREATE PROCEDURE LTN_TRAIN()
LANGUAGE SQLSCRIPT AS
BEGIN

	DELETE FROM MODELS;
    CALL CREATE_TRAINING_DATA(1, :train_data); -- TODO: replace task number

    CALL R_TRAIN_BINARY(:train_data, T_MODELS);
    INSERT INTO MODELS SELECT * FROM :T_MODELS;

	model = SELECT * FROM MODELS WHERE ID=1;
    CALL R_TRAIN_CLASSES(:train_data, :model, T_MODELS);
    INSERT INTO MODELS SELECT * FROM :T_MODELS;

END;
CALL LTN_TRAIN();
SELECT * FROM MODELS;




-- DROP PROCEDURE LTN_PREDICT;
-- CREATE PROCEDURE LTN_PREDICT()
-- LANGUAGE SQLSCRIPT AS
-- BEGIN
--     data = SELECT E1_ID,E2_ID,E1_TYPE,E2_TYPE,CHAR_DIST,WORD_DIST,"BEFORE",BETWEEN,AFTER,P_BEFORE,P_BETWEEN,P_AFTER 
--     FROM TD_CLASSES_TEST

--     ;

--     models = SELECT * FROM MODELS;
--     CALL R_PREDICT(:data, :models, T_RESULTS);
--     INSERT INTO RESULTS SELECT * FROM :T_RESULTS;
-- END;


-- CALL CALL_R_PREDICT();
-- SELECT * FROM RESULTS;


-- SELECT ACTUAL, PREDICTED, COUNT(*)
-- FROM (
--     SELECT TD.DDI AS ACTUAL, R.DDI AS PREDICTED
--     FROM TD_CLASSES_TEST TD
--     JOIN RESULTS R ON TD.E1_ID = R.E1_ID AND TD.E2_ID = R.E2_ID
-- )
-- GROUP BY ACTUAL, PREDICTED
-- ORDER BY ACTUAL, PREDICTED;


-- DROP TYPE COMPARISON_T;
-- CREATE TYPE COMPARISON_T AS TABLE (ACTUAL INT, PREDICTED INT);

-- DROP TABLE COMPARISON;
-- CREATE TABLE COMPARISON LIKE COMPARISON_T;
-- INSERT INTO COMPARISON
--     SELECT TD.DDI AS ACTUAL, R.DDI AS PREDICTED
--     FROM TD_CLASSES_TEST TD
--     JOIN RESULTS R ON TD.E1_ID = R.E1_ID AND TD.E2_ID = R.E2_ID;



-- CREATE TYPE PRF_TABLE_T AS TABLE (T INT, PRECISION DOUBLE, RECALL DOUBLE, F_MEASURE DOUBLE);
-- DROP PROCEDURE PRF;
-- CREATE PROCEDURE PRF(IN data COMPARISON_T, OUT prf PRF_TABLE_T)
-- LANGUAGE RLANG AS
-- BEGIN
--     precision <- function(data, type) {
--         predicted = data[,2]
--         actual = data[,1]
--         tp = length(which(predicted == type & actual == type))
--         fp = length(which(predicted == type & actual != type))
--         return(tp/(tp+fp))
--     }

--     recall <- function(data, type) {
--         predicted = data[,2]
--         actual = data[,1]
--         tp = length(which(predicted == type & actual == type))
--         fn = length(which(predicted != type & actual == type))
--         return(tp/(tp+fn))
--     }

--     precision.collection <- function(data) {
--         types = sort(unique(data[,1]))
--         result = matrix(ncol=4, nrow=0)
--         for (type in types) {
--             p = precision(data, type)
--             r = recall(data, type)
--             f = 2*p*r/(p+r)

--             result <- rbind (result, c(type,round(p,2),round(r,2),round(f,2)))
--         }
--         result <- as.data.frame(result)
--         colnames(result) <- c('T', 'PRECISION', 'RECALL', 'F_MEASURE')
--         return(result)
--     }

--     prf = precision.collection(data)
-- END;

-- CALL PRF(COMPARISON, ?)



-- SELECT MATCHES/TOTAL AS ACCURACY
-- FROM (
--     SELECT
--     (
--         SELECT COUNT(*)
--         FROM TD_CLASSES_TEST TD
--         JOIN RESULTS R ON TD.E1_ID = R.E1_ID AND TD.E2_ID = R.E2_ID
--         WHERE TD.DDI = R.DDI
--     ) AS MATCHES,
--     (
--         SELECT COUNT(*)
--         FROM RESULTS
--     ) AS TOTAL
--     FROM DUMMY
-- );
