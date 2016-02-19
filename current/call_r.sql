SET SCHEMA LTN_TRAIN;

DROP TABLE RESULTS;
CREATE COLUMN TABLE RESULTS LIKE T_RESULTS;

DROP PROCEDURE CALL_R_TRAIN_BINARY;
CREATE PROCEDURE CALL_R_TRAIN_BINARY()
LANGUAGE SQLSCRIPT AS
BEGIN
    DELETE FROM MODELS WHERE ID=1;
    train_data = SELECT * FROM TD_CLASSES_TRAIN;
    CALL R_TRAIN_BINARY(:train_data, T_MODELS);
    INSERT INTO MODELS SELECT * FROM :T_MODELS;
END;

DROP PROCEDURE CALL_R_TRAIN_CLASSES;
CREATE PROCEDURE CALL_R_TRAIN_CLASSES()
LANGUAGE SQLSCRIPT AS
BEGIN
    DELETE FROM MODELS WHERE ID=2;
    train_data = SELECT * FROM TD_CLASSES_TRAIN WHERE DDI <> -1;
    model = SELECT * FROM MODELS WHERE ID=1;
    CALL R_TRAIN_CLASSES(:train_data, :model, T_MODELS);
    INSERT INTO MODELS SELECT * FROM :T_MODELS;
END;


DROP PROCEDURE CALL_R_PREDICT;
CREATE PROCEDURE CALL_R_PREDICT()
LANGUAGE SQLSCRIPT AS
BEGIN
    data = SELECT E1_ID,E2_ID,"BEFORE",BETWEEN,AFTER,P_BEFORE,P_BETWEEN,P_AFTER FROM TD_CLASSES_TEST;

    models = SELECT * FROM LTN_TRAIN.MODELS;
    CALL LTN_TRAIN.R_PREDICT(:data, :models, T_RESULTS);
    INSERT INTO LTN_TRAIN.RESULTS SELECT * FROM :T_RESULTS;
END;

CALL CALL_R_TRAIN_BINARY();
CALL CALL_R_TRAIN_CLASSES();
SELECT * FROM MODELS;
CALL CALL_R_PREDICT();
SELECT * FROM RESULTS;

SELECT MATCHES/TOTAL AS ACCURACY
FROM (
    SELECT
    (
        SELECT COUNT(*)
        FROM TD_CLASSES_TEST TD
        JOIN RESULTS R ON TD.E1_ID = R.E1_ID AND TD.E2_ID = R.E2_ID
        WHERE TD.DDI = R.DDI
    ) AS MATCHES,
    (
        SELECT COUNT(*)
        FROM RESULTS
    ) AS TOTAL
    FROM DUMMY
);
