SET SCHEMA LTN_TRAIN;

DROP TYPE T_TD_CLASSES;
CREATE TYPE T_TD_CLASSES AS TABLE (DDI VARCHAR(255), E1_ID VARCHAR(255), E2_ID VARCHAR(255), "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);

DROP TYPE T_PREDICT_INPUT;
CREATE TYPE T_PREDICT_INPUT AS TABLE (E1_ID VARCHAR(255), E2_ID VARCHAR(255), "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);

DROP TABLE TD_CLASSES;
CREATE COLUMN TABLE TD_CLASSES LIKE T_TD_CLASSES;

DROP TABLE TD_CLASSES_TRAIN;
DROP TABLE TD_CLASSES_TEST;
CREATE COLUMN TABLE TD_CLASSES_TRAIN LIKE T_TD_CLASSES;
CREATE COLUMN TABLE TD_CLASSES_TEST LIKE T_TD_CLASSES;


INSERT INTO TD_CLASSES
SELECT DDI, E1_ID, E2_ID,
STRING_AGG(T1, ' ') AS "BEFORE",
STRING_AGG(T2, ' ') AS BETWEEN,
STRING_AGG(T3, ' ') AS AFTER,
STRING_AGG(P1, ' ') AS P_BEFORE,
STRING_AGG(P2, ' ') AS P_BETWEEN,
STRING_AGG(P3, ' ') AS P_AFTER
FROM (
	SELECT E1_ID, E2_ID, DDI,
	STRING_AGG(T1, ' ' ORDER BY COUNTER) AS T1,
	STRING_AGG(T2, ' ' ORDER BY COUNTER) AS T2,
	STRING_AGG(T3, ' ' ORDER BY COUNTER) AS T3,
	STRING_AGG(P1, ' ' ORDER BY COUNTER) AS P1,
	STRING_AGG(P2, ' ' ORDER BY COUNTER) AS P2,
	STRING_AGG(P3, ' ' ORDER BY COUNTER) AS P3
	FROM(
		SELECT E1_ID, E2_ID, DDI,
		CASE WHEN POSITION < 0 THEN TOKEN ELSE NULL END AS T1,
		CASE WHEN POSITION = 0 THEN TOKEN ELSE NULL END AS T2,
		CASE WHEN POSITION > 0 THEN TOKEN ELSE NULL END AS T3,

		CASE WHEN POSITION < 0 THEN POS ELSE NULL END AS P1,
		CASE WHEN POSITION = 0 THEN POS ELSE NULL END AS P2,
		CASE WHEN POSITION > 0 THEN POS ELSE NULL END AS P3,

		COUNTER,
		POSITION
		FROM (
			SELECT E1_ID, E2_ID,
			CASE WHEN P.LABEL IS NULL THEN 'NONE' ELSE P.LABEL END AS DDI,
			CASE WHEN FTI.TA_STEM IS NULL THEN FTI.TA_NORMALIZED ELSE FTI.TA_STEM END AS TOKEN,
			POSTAGS.ID as POS,
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
			JOIN LEARNING_TO_NOTE.POS_TAGS POSTAGS ON FTI.TA_TYPE = POSTAGS.POS
			WHERE UD.USER_ID = 'DDI-IMPORTER'
			AND FTI.TA_TYPE <> 'punctuation'
			AND FTI1.TA_SENTENCE = FTI2.TA_SENTENCE
			AND FTI.TA_SENTENCE = FTI1.TA_SENTENCE
			AND FTI.TA_COUNTER <> FTI1.TA_COUNTER
			AND FTI.TA_COUNTER <> FTI2.TA_COUNTER
			AND FTI1.TA_COUNTER < FTI2.TA_COUNTER

			AND (
				(FTI1.TA_COUNTER -3 <= FTI.TA_COUNTER AND FTI.TA_COUNTER <= FTI1.TA_COUNTER +3)
				OR
				(FTI2.TA_COUNTER -3 <= FTI.TA_COUNTER AND FTI.TA_COUNTER <= FTI2.TA_COUNTER +3)
			)

			ORDER BY FTI.TA_COUNTER
		)
	)
	GROUP BY E1_ID, E2_ID, DDI, POSITION
	ORDER BY E1_ID, E2_ID, POSITION
)
GROUP BY E1_ID, E2_ID, DDI;





DROP PROCEDURE DOWNSAMPLE_R;
CREATE PROCEDURE DOWNSAMPLE_R(IN data T_TD_CLASSES, OUT train_data T_TD_CLASSES, OUT test_data T_TD_CLASSES)
LANGUAGE RLANG AS
BEGIN
	#### down sampling
	true_pairs <- data[data$DDI != "NONE",]
	false_pairs <- data[data$DDI == "NONE",]

	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs))
	false_downsampled <- false_pairs[false_downsampled_index,]

	sampled <- rbind(true_pairs, false_downsampled)

	index <- 1:nrow(sampled)
	testindex <- sample(index, trunc(length(index)/10))
	test_data <- sampled[testindex,]
	train_data <- sampled[-testindex,]
END;

DROP PROCEDURE DOWNSAMPLE;
CREATE PROCEDURE DOWNSAMPLE()
LANGUAGE SQLSCRIPT AS
BEGIN
    data = SELECT * FROM TD_CLASSES;
    CALL DOWNSAMPLE_R(:data, train_data, test_data);
    INSERT INTO TD_CLASSES_TRAIN SELECT * FROM :train_data;
    INSERT INTO TD_CLASSES_TEST SELECT * FROM :test_data;
END;

CALL DOWNSAMPLE();

SELECT COUNT(*) FROM TD_CLASSES_TRAIN;
SELECT COUNT(*) FROM TD_CLASSES_TEST;

