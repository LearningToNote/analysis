SET SCHEMA LEARNING_TO_NOTE;


-- this counts words between two entities
-- believe me

SELECT FTI.ID, FTI.TA_SENTENCE, E1.TEXT, E2.TEXT, COUNT(*)
FROM PAIRS P
JOIN ENTITIES E1 ON P.E1_ID = E1.ID
JOIN ENTITIES E2 ON P.E2_ID = E2.ID
JOIN USER_DOCUMENTS UD ON E1.USER_DOC_ID = UD.ID AND E2.USER_DOC_ID = UD.ID
JOIN "$TA_FTI" FTI1 ON FTI1.ID = UD.DOCUMENT_ID AND FTI1.TA_TOKEN = E1.TEXT
JOIN "$TA_FTI" FTI2 ON FTI2.ID = UD.DOCUMENT_ID AND FTI2.TA_TOKEN = E2.TEXT
JOIN "$TA_FTI" FTI ON FTI.ID = UD.DOCUMENT_ID
WHERE UD.USER_ID = 'DDI-IMPORTER'
AND FTI1.TA_SENTENCE = FTI2.TA_SENTENCE
AND FTI.TA_SENTENCE = FTI1.TA_SENTENCE
AND FTI.TA_COUNTER > FTI1.TA_COUNTER
AND FTI.TA_COUNTER < FTI2.TA_COUNTER
AND FTI.TA_TYPE <> 'punctuation'
--AND UD.DOCUMENT_ID = 'DDI-DrugBank.d532'
GROUP BY FTI.ID, FTI.TA_SENTENCE, E1.TEXT, E2.TEXT
;
