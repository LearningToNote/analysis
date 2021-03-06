DROP TABLE LEARNING_TO_NOTE.TRAIN_DATA;
CREATE COLUMN TABLE LEARNING_TO_NOTE.TRAIN_DATA LIKE LEARNING_TO_NOTE.T_DDI_DATA;

INSERT INTO LEARNING_TO_NOTE.TRAIN_DATA
SELECT DDI,
STRING_AGG(T1, ' ') AS "BEFORE",
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

		AND (
			(FTI1.TA_COUNTER -3 <= FTI.TA_COUNTER AND FTI.TA_COUNTER <= FTI1.TA_COUNTER +3)
			OR
			(FTI2.TA_COUNTER -3 <= FTI.TA_COUNTER AND FTI.TA_COUNTER <= FTI2.TA_COUNTER +3)
		)


		--AND FTI.ID = 'DDI-DrugBank.d0' OR FTI.ID = 'DDI-DrugBank.d1' OR FTI.ID = 'DDI-DrugBank.d3'


		ORDER BY FTI.TA_COUNTER
		)
		WHERE TOKEN NOT IN (SELECT STOPWORD FROM LEARNING_TO_NOTE.STOPWORDS)

	)

	GROUP BY E1_ID, E2_ID, DDI, POSITION
	ORDER BY E1_ID, E2_ID, POSITION
)
GROUP BY E1_ID, E2_ID, DDI


;


SELECT * FROM LEARNING_TO_NOTE.TRAIN_DATA
