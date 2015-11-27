SET SCHEMA DM_PAL;

DROP TABLE SAMPLES;
DROP TABLE TRUE_VALUES;
DROP TABLE FALSE_VALUES;
DROP TABLE DOWNSAMPLED_VALUES;
DROP TABLE TRAIN_DATA;
DROP TABLE TEST_DATA;


CREATE COLUMN TABLE SAMPLES AS (
  SELECT P.DDI AS DDI, ET1.ID AS E1_TYPE, ET2.ID AS E2_TYPE
  FROM LEARNING_TO_NOTE.PAIRS P
  JOIN LEARNING_TO_NOTE.ENTITIES E1 ON P.E1_ID = E1.ID
  JOIN LEARNING_TO_NOTE.ENTITIES E2 ON P.E2_ID = E2.ID
  JOIN LEARNING_TO_NOTE.ENTITY_TYPES ET1 ON ET1."TYPE" = E1."TYPE"
  JOIN LEARNING_TO_NOTE.ENTITY_TYPES ET2 ON ET2."TYPE" = E2."TYPE"
);

CREATE COLUMN TABLE TRUE_VALUES AS (
  SELECT *
  FROM SAMPLES
  WHERE DDI = 1
);
-- 3,789

CREATE COLUMN TABLE FALSE_VALUES AS (
  SELECT *
  FROM SAMPLES
  WHERE DDI = 0
);


CREATE COLUMN TABLE DOWNSAMPLED_VALUES AS (
  SELECT ROW_NUMBER() OVER (ORDER BY DDI) AS ID, DDI, E1_TYPE, E2_TYPE
  FROM (
    SELECT * FROM TRUE_VALUES
    UNION ALL
    SELECT * FROM FALSE_VALUES TABLESAMPLE SYSTEM (19)
  )
);


CREATE COLUMN TABLE TRAIN_DATA AS (SELECT * FROM DOWNSAMPLED_VALUES TABLESAMPLE BERNOULLI(70));
CREATE COLUMN TABLE TEST_DATA AS (SELECT * FROM DOWNSAMPLED_VALUES WHERE ID NOT IN (SELECT ID FROM TRAIN_DATA));










  
