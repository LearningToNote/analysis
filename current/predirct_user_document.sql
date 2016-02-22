SET SCHEMA LTN_TRAIN;

DROP TABLE RESULTS;
CREATE COLUMN TABLE RESULTS LIKE T_RESULTS;

DROP PROCEDURE PREDICT_UD;
CREATE PROCEDURE PREDICT_UD(IN ud_id VARCHAR(255), OUT results T_RESULTS)
LANGUAGE SQLSCRIPT AS
BEGIN

	data =
		SELECT E1_ID, E2_ID, E1_TYPE, E2_TYPE, DIST,
		REPLACE_REGEXPR('.* (\w+ \w+ \w+)$' IN STRING_AGG(T1, ' ') WITH '\1' OCCURRENCE ALL) AS "BEFORE",
		REPLACE_REGEXPR('^(\w+ \w+ \w+).* (\w+ \w+ \w+)$' IN STRING_AGG(T2, ' ') WITH '\1 \2' OCCURRENCE ALL) AS BETWEEN,
		REPLACE_REGEXPR('^(\w+ \w+ \w+).*' IN STRING_AGG(T3, ' ') WITH '\1' OCCURRENCE ALL) AS AFTER,
		REPLACE_REGEXPR('.* (\w+ \w+ \w+)$' IN STRING_AGG(P1, ' ') WITH '\1' OCCURRENCE ALL) AS P_BEFORE,
		REPLACE_REGEXPR('^(\w+ \w+ \w+).* (\w+ \w+ \w+)$' IN STRING_AGG(P2, ' ') WITH '\1 \2' OCCURRENCE ALL) AS P_BETWEEN,
		REPLACE_REGEXPR('^(\w+ \w+ \w+).*' IN STRING_AGG(P3, ' ') WITH '\1' OCCURRENCE ALL) AS P_AFTER
		FROM (
			SELECT E1_ID, E2_ID, E1_TYPE, E2_TYPE, DIST,
			STRING_AGG(T1, ' ' ORDER BY COUNTER) AS T1,
			STRING_AGG(T2, ' ' ORDER BY COUNTER) AS T2,
			STRING_AGG(T3, ' ' ORDER BY COUNTER) AS T3,
			STRING_AGG(P1, ' ' ORDER BY COUNTER) AS P1,
			STRING_AGG(P2, ' ' ORDER BY COUNTER) AS P2,
			STRING_AGG(P3, ' ' ORDER BY COUNTER) AS P3
			FROM(
				SELECT E1_ID, E2_ID, E1_TYPE, E2_TYPE, DIST,
				CASE WHEN POSITION < 0 THEN TOKEN ELSE NULL END AS T1,
				CASE WHEN POSITION = 0 THEN TOKEN ELSE NULL END AS T2,
				CASE WHEN POSITION > 0 THEN TOKEN ELSE NULL END AS T3,

				CASE WHEN POSITION < 0 THEN POS ELSE NULL END AS P1,
				CASE WHEN POSITION = 0 THEN POS ELSE NULL END AS P2,
				CASE WHEN POSITION > 0 THEN POS ELSE NULL END AS P3,

				COUNTER,
				POSITION
				FROM (
					SELECT E1.ID E1_ID, E2.ID E2_ID,
					E1.TYPE_ID AS E1_TYPE,
					E2.TYPE_ID AS E2_TYPE,
					FTI2.TA_COUNTER - FTI1.TA_COUNTER AS DIST,
					CASE WHEN FTI.TA_STEM IS NULL THEN FTI.TA_NORMALIZED ELSE FTI.TA_STEM END AS TOKEN,
					POSTAGS.ID as POS,
					CASE
					    WHEN FTI.TA_COUNTER < FTI1.TA_COUNTER THEN -1
					    WHEN (FTI.TA_COUNTER > FTI1.TA_COUNTER AND FTI.TA_COUNTER < FTI2.TA_COUNTER) THEN 0
					    WHEN FTI.TA_COUNTER > FTI2.TA_COUNTER THEN 1
					END AS POSITION,
					FTI.TA_COUNTER AS COUNTER

					FROM LEARNING_TO_NOTE.USER_DOCUMENTS UD
					JOIN LEARNING_TO_NOTE.ENTITIES E1 ON E1.USER_DOC_ID = UD.ID
					JOIN LEARNING_TO_NOTE.ENTITIES E2 ON E2.USER_DOC_ID = UD.ID
					JOIN LEARNING_TO_NOTE.OFFSETS O1 ON O1.ENTITY_ID = E1.ID AND O1.USER_DOC_ID = UD.ID
					JOIN LEARNING_TO_NOTE.OFFSETS O2 ON O2.ENTITY_ID = E2.ID AND O2.USER_DOC_ID = UD.ID
					JOIN LEARNING_TO_NOTE."$TA_FTI" FTI1 ON FTI1.ID = UD.DOCUMENT_ID AND FTI1.TA_OFFSET = O1."START"
					JOIN LEARNING_TO_NOTE."$TA_FTI" FTI2 ON FTI2.ID = UD.DOCUMENT_ID AND FTI2.TA_OFFSET = O2."START"
					JOIN LEARNING_TO_NOTE."$TA_FTI" FTI ON FTI.ID = UD.DOCUMENT_ID
					JOIN LEARNING_TO_NOTE.POS_TAGS POSTAGS ON FTI.TA_TYPE = POSTAGS.POS
					WHERE UD.ID = :ud_id
					AND FTI.TA_TYPE <> 'punctuation'
					AND E1.ID < E2.ID
					AND FTI1.TA_SENTENCE = FTI2.TA_SENTENCE
					AND FTI.TA_SENTENCE = FTI1.TA_SENTENCE
					AND FTI.TA_COUNTER <> FTI1.TA_COUNTER
					AND FTI.TA_COUNTER <> FTI2.TA_COUNTER
					AND FTI1.TA_COUNTER < FTI2.TA_COUNTER

					ORDER BY FTI.TA_COUNTER
				)
			)
			GROUP BY E1_ID, E2_ID, E1_TYPE, E2_TYPE, DIST, POSITION
			ORDER BY E1_ID, E2_ID, POSITION
		)
		GROUP BY E1_ID, E2_ID, E1_TYPE, E2_TYPE, DIST;


    models = SELECT * FROM LTN_TRAIN.MODELS;
    CALL LTN_TRAIN.R_PREDICT(:data, :models, T_RESULTS);
    results = SELECT * FROM :T_RESULTS;
END;

CALL PREDICT_UD('DDI-IMPORTER_DDI-MedLine.d99', ?);
CALL PREDICT_UD('DDI-IMPORTER_DDI-DrugBank.d0', ?);


