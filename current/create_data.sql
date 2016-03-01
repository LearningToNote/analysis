SET SCHEMA LTN_TRAIN;

DROP TYPE T_TD_CLASSES_DOCS;
CREATE TYPE T_TD_CLASSES_DOCS AS TABLE (DOC_ID VARCHAR(255), DDI INT, E1_ID VARCHAR(255), E2_ID VARCHAR(255), E1_TYPE INT, E2_TYPE INT, CHAR_DIST INT, WORD_DIST INT, "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);
DROP TYPE T_TD_CLASSES;
CREATE TYPE T_TD_CLASSES AS TABLE (DDI INT, E1_ID VARCHAR(255), E2_ID VARCHAR(255), E1_TYPE INT, E2_TYPE INT, CHAR_DIST INT, WORD_DIST INT, "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);

DROP TYPE T_PREDICT_INPUT;
CREATE TYPE T_PREDICT_INPUT AS TABLE (E1_ID VARCHAR(255), E2_ID VARCHAR(255), E1_TYPE INT, E2_TYPE INT, CHAR_DIST INT, WORD_DIST INT, "BEFORE" TEXT, BETWEEN TEXT, AFTER TEXT, P_BEFORE TEXT, P_BETWEEN TEXT, P_AFTER TEXT);

DROP TABLE TD_CLASSES;
CREATE COLUMN TABLE TD_CLASSES LIKE T_TD_CLASSES_DOCS;

DROP TABLE TD_CLASSES_TRAIN;
DROP TABLE TD_CLASSES_TEST;
CREATE COLUMN TABLE TD_CLASSES_TRAIN LIKE T_TD_CLASSES;
CREATE COLUMN TABLE TD_CLASSES_TEST LIKE T_TD_CLASSES;


INSERT INTO TD_CLASSES
SELECT DOC_ID, DDI, E1_ID, E2_ID, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST,
REPLACE_REGEXPR('.* (\w+ \w+ \w+)$' IN STRING_AGG(T1, ' ') WITH '\1' OCCURRENCE ALL) AS "BEFORE",
REPLACE_REGEXPR('^(\w+ \w+ \w+).* (\w+ \w+ \w+)$' IN STRING_AGG(T2, ' ') WITH '\1 \2' OCCURRENCE ALL) AS BETWEEN,
REPLACE_REGEXPR('^(\w+ \w+ \w+).*' IN STRING_AGG(T3, ' ') WITH '\1' OCCURRENCE ALL) AS AFTER,
REPLACE_REGEXPR('.* (\w+ \w+ \w+)$' IN STRING_AGG(P1, ' ') WITH '\1' OCCURRENCE ALL) AS P_BEFORE,
REPLACE_REGEXPR('^(\w+ \w+ \w+).* (\w+ \w+ \w+)$' IN STRING_AGG(P2, ' ') WITH '\1 \2' OCCURRENCE ALL) AS P_BETWEEN,
REPLACE_REGEXPR('^(\w+ \w+ \w+).*' IN STRING_AGG(P3, ' ') WITH '\1' OCCURRENCE ALL) AS P_AFTER
FROM (
	SELECT DOC_ID, E1_ID, E2_ID, DDI, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST,
	STRING_AGG(T1, ' ' ORDER BY COUNTER) AS T1,
	STRING_AGG(T2, ' ' ORDER BY COUNTER) AS T2,
	STRING_AGG(T3, ' ' ORDER BY COUNTER) AS T3,
	STRING_AGG(P1, ' ' ORDER BY COUNTER) AS P1,
	STRING_AGG(P2, ' ' ORDER BY COUNTER) AS P2,
	STRING_AGG(P3, ' ' ORDER BY COUNTER) AS P3
	FROM(
		SELECT DOC_ID, E1_ID, E2_ID, DDI, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST,
		CASE WHEN POSITION < 0 THEN TOKEN ELSE NULL END AS T1,
		CASE WHEN POSITION = 0 THEN TOKEN ELSE NULL END AS T2,
		CASE WHEN POSITION > 0 THEN TOKEN ELSE NULL END AS T3,

		CASE WHEN POSITION < 0 THEN POS ELSE NULL END AS P1,
		CASE WHEN POSITION = 0 THEN POS ELSE NULL END AS P2,
		CASE WHEN POSITION > 0 THEN POS ELSE NULL END AS P3,

		COUNTER,
		POSITION
		FROM (
			SELECT
			UD.ID AS DOC_ID,
			E1_ID,
			E2_ID,
			CASE WHEN P.TYPE_ID IS NULL THEN -1 ELSE P.TYPE_ID END AS DDI,
			E1.TYPE_ID AS E1_TYPE,
			E2.TYPE_ID AS E2_TYPE,
			O2."START" - O1."END" AS CHAR_DIST,
			FTI2.TA_COUNTER - FTI1.TA_COUNTER AS WORD_DIST,
			CASE
				WHEN FTI.TA_NORMALIZED IS NOT NULL THEN FTI.TA_NORMALIZED
				WHEN FTI.TA_STEM IS NOT NULL THEN FTI.TA_STEM
				ELSE FTI.TA_TOKEN
			END AS TOKEN,
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
			JOIN LEARNING_TO_NOTE.USER_DOCUMENTS UD ON E1.USER_DOC_ID = UD.ID AND E2.USER_DOC_ID = UD.ID AND P.USER_DOC_ID = UD.ID
			JOIN LEARNING_TO_NOTE.OFFSETS O1 ON O1.ENTITY_ID = E1.ID
			JOIN LEARNING_TO_NOTE.OFFSETS O2 ON O2.ENTITY_ID = E2.ID
			JOIN LEARNING_TO_NOTE."$TA_FTI" FTI1 ON FTI1.ID = UD.DOCUMENT_ID AND FTI1.TA_OFFSET = O1."START"
			JOIN LEARNING_TO_NOTE."$TA_FTI" FTI2 ON FTI2.ID = UD.DOCUMENT_ID AND FTI2.TA_OFFSET = O2."START"
			JOIN LEARNING_TO_NOTE."$TA_FTI" FTI ON FTI.ID = UD.DOCUMENT_ID
			JOIN LEARNING_TO_NOTE.POS_TAGS POSTAGS ON FTI.TA_TYPE = POSTAGS.POS
			WHERE UD.USER_ID = 'DDI-IMPORTER'
			AND (FTI.TA_TYPE <> 'punctuation' OR FTI.TA_TOKEN=':')
			AND FTI1.TA_SENTENCE = FTI2.TA_SENTENCE
			AND FTI.TA_SENTENCE = FTI1.TA_SENTENCE
			AND FTI.TA_COUNTER <> FTI1.TA_COUNTER
			AND FTI.TA_COUNTER <> FTI2.TA_COUNTER
			AND FTI1.TA_COUNTER < FTI2.TA_COUNTER

			ORDER BY FTI.TA_COUNTER
		)
		WHERE TOKEN NOT IN (SELECT STOPWORD FROM LEARNING_TO_NOTE.STOPWORDS)
	)
	GROUP BY DOC_ID, E1_ID, E2_ID, DDI, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST, POSITION
	ORDER BY DOC_ID, E1_ID, E2_ID, POSITION
)
GROUP BY DOC_ID, E1_ID, E2_ID, DDI, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST;


DROP PROCEDURE DOWNSAMPLE_R;
CREATE PROCEDURE DOWNSAMPLE_R(IN data T_TD_CLASSES_DOCS, OUT train_data T_TD_CLASSES, OUT test_data T_TD_CLASSES)
LANGUAGE RLANG AS
BEGIN
	#### down sampling
	true_pairs <- data[data$DDI != -1,]
	false_pairs <- data[data$DDI == -1,]

	false_downsampled_index <- sample(1:nrow(false_pairs), nrow(true_pairs) * 1.5)
	false_downsampled <- false_pairs[false_downsampled_index,]

	sampled <- rbind(true_pairs, false_downsampled)

	docs <- unique(sampled$DOC_ID)
	testdocs <- sample(docs, 20)

	testrows <- which(sampled$DOC_ID %in% testdocs)
	test_data <- sampled[testrows,-1]
	train_data <- sampled[-testrows,-1]
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
SELECT * FROM TD_CLASSES_TEST;

