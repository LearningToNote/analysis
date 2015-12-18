/*
SELECT E1_ID, E2_ID,
SUM(use) AS use,
SUM(increase) AS increase
FROM(
  SELECT ID, TA_SENTENCE, E1_ID, E2_ID,
  CASE WHEN TOKEN = 'use' THEN 1 ELSE 0 END AS use,
  CASE WHEN TOKEN = 'increase' THEN 1 ELSE 0 END AS increase
  FROM "#verbsInBetween"
)
GROUP BY ID, TA_SENTENCE, E1_ID, E2_ID
*/

CREATE PROCEDURE "LEARNING_TO_NOTE"."LTN::DynamicWordsInBetween"
  LANGUAGE SQLSCRIPT
  SQL SECURITY INVOKER
  DEFAULT SCHEMA LEARNING_TO_NOTE AS
  CURSOR C_WORDS FOR SELECT TOKEN FROM "COMMON_WORDS";
  i int default 0;

  select_clause NVARCHAR(2000000) := 'SELECT E1_ID, E2_ID,';
  from_clause NVARCHAR(2000000) := 'FROM( SELECT ID, TA_SENTENCE, E1_ID, E2_ID,';
  query NVARCHAR(2000000);
BEGIN

  CREATE LOCAL TEMPORARY TABLE "#verbsInBetween" (
    ID VARCHAR(25),
    TA_SENTENCE INT,
    E1_ID VARCHAR(25),
    E2_ID VARCHAR(25),
    TOKEN VARCHAR(25));

  INSERT INTO "#verbsInBetween" (
    SELECT ID, TA_SENTENCE, E1_ID, E2_ID, TOKEN FROM (
        SELECT FTI.ID, FTI.TA_SENTENCE, E1_ID, E2_ID,
      CASE WHEN FTI.TA_STEM IS NULL THEN FTI.TA_NORMALIZED ELSE FTI.TA_STEM END AS TOKEN
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
      AND (FTI.TA_TYPE = 'verb' OR FTI.TA_TYPE = 'auxiliary verb')
      ---AND UD.DOCUMENT_ID = 'DDI-DrugBank.d522')
      )
    GROUP BY ID, TA_SENTENCE, E1_ID, E2_ID, TOKEN);

  FOR CUR_ROW AS C_WORDS DO
    if i>0 then
    select_clause := select_clause || ',' ;
    from_clause := from_clause || ',';
    end if;

    select_clause := select_clause || 'SUM(' || CUR_ROW.TOKEN || ') AS ' || CUR_ROW.TOKEN;
    from_clause := from_clause || 'CASE WHEN TOKEN = ''' || CUR_ROW.TOKEN || ''' THEN 1 ELSE 0 END AS ' || CUR_ROW.TOKEN;
    i := i+1;
  END FOR;

  from_clause := from_clause || ' FROM "#verbsInBetween")';
  from_clause := from_clause || ' GROUP BY ID, TA_SENTENCE, E1_ID, E2_ID';

  query := select_clause ||' '|| from_clause;

  EXECUTE IMMEDIATE :query;

  DROP TABLE "#verbsInBetween";
END;


CALL "LEARNING_TO_NOTE"."LTN::DynamicWordsInBetween"();
