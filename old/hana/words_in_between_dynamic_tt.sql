
DROP TYPE TT_COMMON_WORDS_INBETWEEN;
DROP PROCEDURE "LEARNING_TO_NOTE"."LTN::DynamicWordsTableType";

CREATE PROCEDURE "LEARNING_TO_NOTE"."LTN::DynamicWordsTableType" ()
  LANGUAGE SQLSCRIPT
  SQL SECURITY INVOKER
  DEFAULT SCHEMA LEARNING_TO_NOTE AS
  CURSOR C_WORDS FOR SELECT TOKEN FROM "COMMON_WORDS";

  query NVARCHAR(2000000) := 'CREATE TYPE TT_COMMON_WORDS_INBETWEEN AS TABLE( ID INT, DDI TINYINT';
BEGIN

  FOR CUR_ROW AS C_WORDS DO
    query := query || ', ' ;

    query := query || CUR_ROW.TOKEN || ' INT';
  END FOR;
  query := query || ');';

  EXECUTE IMMEDIATE :query;

END;

CALL "LEARNING_TO_NOTE"."LTN::DynamicWordsTableType"();