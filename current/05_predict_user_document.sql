SET SCHEMA LTN_DEVELOP;

DROP TABLE RESULTS;
CREATE COLUMN TABLE RESULTS LIKE T_RESULTS;

DROP PROCEDURE PREDICT_UD;
CREATE PROCEDURE PREDICT_UD(IN ud_id VARCHAR(255), IN task_id INT, OUT results T_RESULTS)
LANGUAGE SQLSCRIPT AS
BEGIN

DECLARE FTI LTN_DEVELOP.T_INDEX;
CALL get_fulltext_index_for_task(:task_id, :FTI);
data =
	SELECT E1_ID, E2_ID, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST,
	REPLACE_REGEXPR('.* (\w+[ \:\,\;]* \w+[ \:\,\;]* \w+[ \:\,\;]*)$' IN STRING_AGG(T1, ' ') WITH '\1' OCCURRENCE ALL) AS "BEFORE",
	REPLACE_REGEXPR('^([\:\,\; ]*\w+[ \:\,\;]* \w+[ \:\,\;]* \w+[ \:\,\;]*).* (\w+[ \:\,\;]* \w+[ \:\,\;]* \w+[ \:\,\;]*)$' IN STRING_AGG(T2, ' ') WITH '\1 \2' OCCURRENCE ALL) AS BETWEEN,
	REPLACE_REGEXPR('^([\:\,\; ]*\w+[ \:\,\;]* \w+[ \:\,\;]* \w+[ \:\,\;]*).*' IN STRING_AGG(T3, ' ') WITH '\1' OCCURRENCE ALL) AS AFTER,
	REPLACE_REGEXPR('.* (\w+ \w+ \w+)$' IN STRING_AGG(P1, ' ') WITH '\1' OCCURRENCE ALL) AS P_BEFORE,
	REPLACE_REGEXPR('^(\w+ \w+ \w+).* (\w+ \w+ \w+)$' IN STRING_AGG(P2, ' ') WITH '\1 \2' OCCURRENCE ALL) AS P_BETWEEN,
	REPLACE_REGEXPR('^(\w+ \w+ \w+).*' IN STRING_AGG(P3, ' ') WITH '\1' OCCURRENCE ALL) AS P_AFTER
	FROM (
		SELECT E1_ID, E2_ID, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST,
		STRING_AGG(T1, ' ' ORDER BY COUNTER) AS T1,
		STRING_AGG(T2, ' ' ORDER BY COUNTER) AS T2,
		STRING_AGG(T3, ' ' ORDER BY COUNTER) AS T3,
		STRING_AGG(P1, ' ' ORDER BY COUNTER) AS P1,
		STRING_AGG(P2, ' ' ORDER BY COUNTER) AS P2,
		STRING_AGG(P3, ' ' ORDER BY COUNTER) AS P3
		FROM(
			SELECT E1_ID, E2_ID, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST,
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

				FROM USER_DOCUMENTS UD
				JOIN ENTITIES E1 ON E1.USER_DOC_ID = UD.ID
				JOIN ENTITIES E2 ON E2.USER_DOC_ID = UD.ID
				JOIN OFFSETS O1 ON O1.ENTITY_ID = E1.ID AND O1.USER_DOC_ID = UD.ID
				JOIN OFFSETS O2 ON O2.ENTITY_ID = E2.ID AND O2.USER_DOC_ID = UD.ID
				JOIN :FTI FTI1 ON FTI1.DOCUMENT_ID = UD.DOCUMENT_ID AND FTI1.TA_OFFSET = O1."START"
				JOIN :FTI FTI2 ON FTI2.DOCUMENT_ID = UD.DOCUMENT_ID AND FTI2.TA_OFFSET = O2."START"
				JOIN :FTI FTI ON FTI.DOCUMENT_ID = UD.DOCUMENT_ID
				JOIN POS_TAGS POSTAGS ON FTI.TA_TYPE = POSTAGS.POS
				WHERE UD.ID = :ud_id
				AND (FTI.TA_TYPE <> 'punctuation' OR FTI.TA_TOKEN = ':' OR FTI.TA_TOKEN = ',' OR FTI.TA_TOKEN = ';')
				AND FTI.TA_TYPE <> 'number'
				AND FTI.TA_TOKEN NOT LIKE '%!%%' ESCAPE '!'
				AND E1.ID < E2.ID
				AND FTI1.TA_SENTENCE = FTI2.TA_SENTENCE
				AND FTI.TA_SENTENCE = FTI1.TA_SENTENCE
				AND FTI.TA_COUNTER <> FTI1.TA_COUNTER
				AND FTI.TA_COUNTER <> FTI2.TA_COUNTER
				AND FTI1.TA_COUNTER < FTI2.TA_COUNTER

				ORDER BY FTI.TA_COUNTER
			)
		)
		GROUP BY E1_ID, E2_ID, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST, POSITION
		ORDER BY E1_ID, E2_ID, POSITION
	)
	GROUP BY E1_ID, E2_ID, E1_TYPE, E2_TYPE, CHAR_DIST, WORD_DIST;


    models = SELECT * FROM MODELS;
    CALL R_PREDICT(:data, :models, T_RESULTS);
    results = SELECT * FROM :T_RESULTS;
END;

CALL PREDICT_UD('DDI-IMPORTER_DDI-DrugBank.d0', 1, ?);


