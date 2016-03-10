SET SCHEMA LTN_DEVELOP;

DROP TYPE T_TRAIN_DATA_DOCS;
CREATE TYPE T_TRAIN_DATA_DOCS AS TABLE (DOC_ID VARCHAR(255), DDI INT, E1_ID VARCHAR(255), E2_ID VARCHAR(255), E1_TYPE INT, E2_TYPE INT, CHAR_DIST INT, WORD_DIST INT, "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);
DROP TYPE T_TRAIN_DATA;
CREATE TYPE T_TRAIN_DATA AS TABLE (DDI INT, E1_ID VARCHAR(255), E2_ID VARCHAR(255), E1_TYPE INT, E2_TYPE INT, CHAR_DIST INT, WORD_DIST INT, "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);
DROP TYPE T_TASK_ID;
CREATE TYPE T_TASK_ID AS TABLE (TASK_ID INT);


DROP TYPE T_PREDICT_INPUT;
CREATE TYPE T_PREDICT_INPUT AS TABLE (E1_ID VARCHAR(255), E2_ID VARCHAR(255), E1_TYPE INT, E2_TYPE INT, CHAR_DIST INT, WORD_DIST INT, "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);

DROP TYPE T_MODELS;
CREATE TYPE T_MODELS AS TABLE (
	TASK_ID INTEGER,
	ID INTEGER,
	DESCRIPTION VARCHAR(255),
	MODEL BLOB,
	MATRIX_BEFORE BLOB,
	MATRIX_BETWEEN BLOB,
	MATRIX_AFTER BLOB,
	MATRIX_P_BEFORE BLOB,
	MATRIX_P_BETWEEN BLOB,
	MATRIX_P_AFTER BLOB
);
DROP TABLE MODELS;
CREATE COLUMN TABLE MODELS LIKE T_MODELS;

DROP TYPE T_RESULTS;
CREATE TYPE T_RESULTS AS TABLE (DDI INT, E1_ID VARCHAR(255), E2_ID VARCHAR(255));
