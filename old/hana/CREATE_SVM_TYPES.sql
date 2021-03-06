
DROP TYPE DM_PAL.PAL_SVM_MODELPART2_T;
DROP TYPE DM_PAL.PAL_SVM_TESTINGSET_T;
DROP PROCEDURE "LEARNING_TO_NOTE"."LTN::CREATE_SVM_TYPES";

CREATE PROCEDURE "LEARNING_TO_NOTE"."LTN::CREATE_SVM_TYPES" ()
  LANGUAGE SQLSCRIPT
  SQL SECURITY INVOKER
  DEFAULT SCHEMA "LEARNING_TO_NOTE" AS
  CURSOR C_WORDS FOR SELECT TOKEN FROM "COMMON_WORDS";

  query1 NVARCHAR(2000000) := 'CREATE TYPE DM_PAL.PAL_SVM_MODELPART2_T AS TABLE( ID integer, ALPHA double';
  query2 NVARCHAR(2000000) := 'CREATE TYPE DM_PAL.PAL_SVM_TESTINGSET_T AS TABLE( ID integer';
BEGIN

  FOR CUR_ROW AS C_WORDS DO
    query1 := query1 || ', ' || CUR_ROW.TOKEN || ' INT';
    query2 := query2 || ', ' || CUR_ROW.TOKEN || ' INT';
  END FOR;
  query1 := query1 || ');';
  query2 := query2 || ');';

  EXECUTE IMMEDIATE :query1;
  EXECUTE IMMEDIATE :query2;

END;

CALL "LEARNING_TO_NOTE"."LTN::CREATE_SVM_TYPES"();
