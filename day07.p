
/*------------------------------------------------------------------------
    File        : day07.p
    Purpose     : Solve Day 7 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 7 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Sun Dec 08 11:29:49 CET 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL       AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/&1/day/&2".
DEFINE VARIABLE cCommand   AS CHARACTER NO-UNDO.
/* Variables for input handling */
DEFINE VARIABLE lDownload  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSession   AS CHARACTER NO-UNDO INITIAL "53616c7465645f5f0aa2b48889c4ecb0a71e7086a3ce378be60c9c62fff2ce2f0a803b3cf401a90e48d12df95cfd2383f2923a50c7378e392a1b5d4ce4438c7e".
DEFINE VARIABLE iYear      AS INTEGER   NO-UNDO INITIAL 2024.
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 07.
DEFINE VARIABLE hPLIP      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInput    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOpenURL   AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lPart      AS LOGICAL   NO-UNDO EXTENT 2.
/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution  AS INT64     NO-UNDO.
DEFINE VARIABLE lOk        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug   AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow    AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart      AS INTEGER   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine     AS INTEGER 
   FIELD cInputLine AS CHARACTER FORMAT "X(80)"
   
   INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttCalculate
   FIELD IDLine      AS INTEGER  
   FIELD iResult     AS INT64 FORMAT "zzz,zzz,zzz,zzz,zz9"
   FIELD iTerms      AS INTEGER 
   FIELD cOperations AS CHARACTER FORMAT "X(15)"
   FIELD lTrue       AS LOGICAL 
   FIELD iTerm       AS INTEGER EXTENT 13
   FIELD iOperation  AS INTEGER EXTENT 12
INDEX indLine IS UNIQUE IDLine.

DEFINE VARIABLE iTerm  AS INTEGER NO-UNDO.
DEFINE VARIABLE cTerms AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getBinary RETURNS CHARACTER 
   (INPUT ipiInt64 AS INT64, INPUT ipiLength AS INTEGER) FORWARD.

FUNCTION getTrinary RETURNS CHARACTER 
   (INPUT ipiInt64  AS INT64, INPUT ipiLength AS INTEGER) FORWARD.

{AOC_session.i}

DISPLAY
   SUBSTITUTE ("Year &1 Day &2", iYear, iDay) FORMAT "X(16)" NO-LABELS SKIP
   lOpenURL  LABEL "Open URL?"       VIEW-AS TOGGLE-BOX SKIP
   lDownload LABEL "Download Input?" VIEW-AS TOGGLE-BOX SKIP   
   lPart[1]  LABEL "Solve Part 1?"   VIEW-AS TOGGLE-BOX SKIP
   lPart[2]  LABEL "Solve Part 2?"   VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug  LABEL "Debug?"          VIEW-AS TOGGLE-BOX SKIP 
   lvlShow   LABEL "Show?"           VIEW-AS TOGGLE-BOX SKIP
   WITH FRAME fr-Parameters SIDE-LABELS ROW 3 CENTERED TITLE " Parameters ".
ASSIGN 
   lDownload  = FALSE
   cInputfile = SUBSTITUTE ("C:\OpenEdge\WRK\AOC&1\input\&2.txt", iYear, STRING (iDay, "99"))
   cURL       = SUBSTITUTE (cURL, iYear, iDay)
   .
FILE-INFO:FILE-NAME = cInputFile.
IF FILE-INFO:FILE-TYPE EQ ? THEN 
DO:
   lDownload = TRUE.
END.

UPDATE
   lOpenURL
   lDownload
   lPart
   lvlDebug
   lvlShow
   WITH FRAME fr-Parameters.

RUN plip_aoc.p PERSISTENT SET hPLIP.

IF lOpenURL THEN 
DO:
   RUN chkURL IN hPLIP
      (INPUT  iYear,
      INPUT  iDay,
      OUTPUT lOk,
      OUTPUT cMessage).
   IF lOk EQ FALSE THEN 
   DO:
      MESSAGE cMessage
         VIEW-AS ALERT-BOX WARNING.
      RETURN.
   END.
   cCommand = SUBSTITUTE ("start &1", cURL).
   OS-COMMAND SILENT VALUE (cCommand).
END.

IF lDownload THEN 
DO:
   RUN getInput IN hPLIP
      (INPUT  cSession,
      INPUT  iYear,
      INPUT  iDay,
      INPUT  cInputFile,
      OUTPUT lOk,
      OUTPUT cMessage).
   IF lOk EQ FALSE THEN 
   DO:
      MESSAGE cMessage
         VIEW-AS ALERT-BOX WARNING.
      RETURN.
   END.
END.

/* Start Processing */
iSolution = 0.

ETIME (YES).
COPY-LOB FROM FILE cInputfile TO OBJECT lcInput.

IF lvlDebug THEN 
DO:
   lcInput = "190: 10 19~n3267: 81 40 27~n83: 17 5~n156: 15 6~n7290: 6 8 6 15~n161011: 16 10 13~n192: 17 8 14~n21037: 9 7 18 13~n292: 11 6 16 20".
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN 
      NEXT.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
      .

   // Specific Parsing
   cTerms = TRIM (ENTRY (2, ttLine.cInputLine, ":")).
   CREATE ttCalculate.
   ASSIGN 
      ttCalculate.IDLine     = ttLine.IDLine
      ttCalculate.iResult    = INT64 (ENTRY (1, ttLine.cInputLine, ":"))
      ttCalculate.iTerms     = NUM-ENTRIES(cTerms, " ") 
   .
   DO iTerm = 1 TO ttCalculate.iTerms:
      ASSIGN 
         ttCalculate.iTerm[iTerm] = INTEGER (ENTRY (iTerm, cTerms, " "))
      .
   END.
   
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttCalculate:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */

   FOR EACH ttCalculate:
      RUN Try2Calculate
         (INPUT  ttCalculate.iResult,
          INPUT  ttCalculate.iTerms,
          INPUT  ttCalculate.iTerm,
          OUTPUT ttCalculate.cOperations,
          OUTPUT ttCalculate.lTrue).
      IF ttCalculate.lTrue THEN 
         iSolution = iSolution + ttCalculate.iResult.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 07 - Part One".
END. /* Process Part One */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttCalculate:HANDLE).
END.
   
IF lPart[2] THEN 
DO:

   iSolution = 0.

   FOR EACH ttCalculate:
      IF lvlDebug THEN DO:
         MESSAGE SUBSTITUTE ("Line: &1, Result: &2", ttCalculate.IDLine, ttCalculate.iResult)
         VIEW-AS ALERT-BOX.
      END.
      RUN Try2Calculate
         (INPUT  ttCalculate.iResult,
          INPUT  ttCalculate.iTerms,
          INPUT  ttCalculate.iTerm,
          OUTPUT ttCalculate.cOperations,
          OUTPUT ttCalculate.lTrue).
      IF ttCalculate.lTrue EQ FALSE THEN           
         RUN Try3Calculate
            (INPUT  ttCalculate.iResult,
             INPUT  ttCalculate.iTerms,
             INPUT  ttCalculate.iTerm,
             OUTPUT ttCalculate.cOperations,
             OUTPUT ttCalculate.lTrue).
      IF ttCalculate.lTrue THEN 
         iSolution = iSolution + ttCalculate.iResult.
      IF lvlDebug THEN DO:
         MESSAGE SUBSTITUTE ("operations &1, OK? &2. Total: &3", 
                              ttCalculate.cOperations, 
                              ttCalculate.lTrue,
                              iSolution)
         VIEW-AS ALERT-BOX.
      END.
   END.
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 07 - Part Two".
END. /* Process Part Two */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttCalculate:HANDLE).
END.

CATCH oError AS Progress.Lang.Error :
   DEFINE VARIABLE iMessage      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
   cErrorMessage = oError:GetMessage(1).
   iMessage = 2.
   DO WHILE iMessage LT oError:NumMessages:
      cErrorMessage = SUBSTITUTE ("&1~n&2", cErrorMessage, oError:GetMessage(iMessage)).
      iMessage = iMessage + 1.
   END.
   IF oError:CallStack NE ? THEN 
   DO:
      cErrorMessage = SUBSTITUTE ("&1~n~nCall Stack:~n&2", cErrorMessage, oError:CallStack).
   END.
   MESSAGE "Error!" SKIP (1)
      SUBSTITUTE ("At line #: &1: &2", iLine, cLine) SKIP
      cErrorMessage SKIP(1) 
      VIEW-AS ALERT-BOX ERROR.
   IF lvlShow THEN 
   DO:
   END.
   RETURN.      
END CATCH.


/* **********************  Internal Procedures  *********************** */

PROCEDURE Try2Calculate:
/*------------------------------------------------------------------------------
 Purpose: Try to calculate with two operators
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiResult     AS INT64     NO-UNDO.
DEFINE INPUT  PARAMETER ipiTerms      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiTerm       AS INTEGER   NO-UNDO EXTENT 13.
DEFINE OUTPUT PARAMETER opcOperations AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplTrue       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE iTry        AS INT64     NO-UNDO.
DEFINE VARIABLE cOperations AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTerm       AS INTEGER NO-UNDO.
DEFINE VARIABLE iResult     AS INT64     NO-UNDO.
   
   TryBlock:
   DO iTry = 0 TO EXP (2, ipiTerms - 1):
      /* All the possibile combinations of TWO operators */
      cOperations = getBinary(iTry, ipiTerms - 1).
      DO iTerm = 1 TO ipiTerms - 1:
         CASE SUBSTRING (cOperations, iTerm, 1):
            WHEN "0" THEN DO:
               /* Multiply */
               IF iTerm EQ 1 THEN 
                  iResult = ipiTerm[iTerm] * ipiTerm[iTerm + 1].
               ELSE 
                  iResult = iResult * ipiTerm[iTerm + 1].
            END.
            WHEN "1" THEN DO:
               /* Add */
               IF iTerm EQ 1 THEN 
                  iResult = ipiTerm[iTerm] + ipiTerm[iTerm + 1].
               ELSE 
                  iResult = iResult + ipiTerm[iTerm + 1].
                   
            END.
         END.
         IF iResult GT ipiResult THEN DO:
            oplTrue = FALSE.
            NEXT TryBlock.
         END.
      END.
      IF iResult EQ ipiResult THEN DO:
         oplTrue = TRUE.
         opcOperations = cOperations.
         RETURN.
      END.
   END.

END PROCEDURE.

PROCEDURE Try3Calculate:
   /*------------------------------------------------------------------------------
    Purpose: Try to calculate with three operators
    Notes:
   ------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ipiResult     AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER ipiTerms      AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER ipiTerm       AS INTEGER   NO-UNDO EXTENT 13.
   DEFINE OUTPUT PARAMETER opcOperations AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oplTrue       AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE iTry        AS INT64     NO-UNDO.
   DEFINE VARIABLE cOperations AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iTerm       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iResult     AS INT64     NO-UNDO.
   
   TryBlock:
   DO iTry = 0 TO EXP (3, ipiTerms - 1):
      /* All the possibile combinations of THREE operators */
      cOperations = getTrinary(iTry, ipiTerms - 1).
      DO iTerm = 1 TO ipiTerms - 1:
         CASE SUBSTRING (cOperations, iTerm, 1):
            WHEN "0" THEN 
            DO:
               /* Multiply */
               IF iTerm EQ 1 THEN 
                  iResult = ipiTerm[iTerm] * ipiTerm[iTerm + 1].
               ELSE 
                  iResult = iResult * ipiTerm[iTerm + 1].
            END.
            WHEN "1" THEN 
            DO:
               /* Add */
               IF iTerm EQ 1 THEN 
                  iResult = ipiTerm[iTerm] + ipiTerm[iTerm + 1].
               ELSE 
                  iResult = iResult + ipiTerm[iTerm + 1].
                
            END.
            WHEN "2" THEN 
            DO:
               /* Concatenate */
               IF iTerm EQ 1 THEN
                  iResult = INT64 (STRING (ipiTerm[iTerm]) + STRING (ipiTerm[iTerm + 1])).
               ELSE 
                  iResult = INT64 (STRING (iResult) + STRING (ipiTerm[iTerm + 1])).
            END.                     
         END.
         IF iResult GT ipiResult THEN 
         DO:
            oplTrue = FALSE.
            NEXT TryBlock.
         END.
      END.
      IF iResult EQ ipiResult THEN 
      DO:
         oplTrue = TRUE.
         opcOperations = cOperations.
         RETURN.
      END.
   END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION getBinary RETURNS CHARACTER 
(INPUT ipiInt64  AS INT64, 
 INPUT ipiLength AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Converts an int64 into binary 01010101
 Notes:
------------------------------------------------------------------------------*/   
   DEFINE VARIABLE iBit    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iValue  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cBinary AS CHARACTER NO-UNDO.

   iValue = 1.
   DO iBit = 1 TO ipiLength:
      IF ipiInt64 MOD (iValue * 2) NE 0 THEN 
         cBinary = "1" + cBinary.
      ELSE 
         cBinary = "0" + cBinary.
      iValue = iValue * 2.
      ipiInt64 = ipiInt64 - (ipiInt64 MOD iValue).
   END.
   
   RETURN cBinary.
                     
END FUNCTION.

FUNCTION getTrinary RETURNS CHARACTER 
(INPUT ipiInt64  AS INT64, 
 INPUT ipiLength AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Converts an int64 into base 3 012012
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iValue   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBit     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMod     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDigit   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTrinary AS CHARACTER NO-UNDO.

   iValue = 1.
   DO iBit = 1 TO ipiLength:
      iMod = ipiInt64 MOD (iValue * 3).
      iDigit = iMod / iValue.
      cTrinary = STRING (iDigit) + cTrinary.
      ipiInt64 = ipiInt64 - (iDigit * iValue).
      iValue = iValue * 3.
   END.
   
   RETURN cTrinary.
                     
END FUNCTION.

