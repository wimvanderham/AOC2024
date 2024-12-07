
/*------------------------------------------------------------------------
    File        : day02.p
    Purpose     : Solve Day 2 of Advent of Code 2024

    Syntax      :

    Description : Solution to Day 2 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Mon Dec 02 05:55:04 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 2.
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
   FIELD cInputLine AS CHARACTER FORMAT "X(60)"
   FIELD cInputList AS CHARACTER FORMAT "X(60)"

   FIELD IsSave     AS LOGICAL 
   FIELD iFailed    AS INTEGER 
   INDEX indLine IS UNIQUE IDLine.


DEFINE VARIABLE iReport     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNumber     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrev       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsSave     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDecreasing AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iDelta      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNext       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSkip       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewReport  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNewReports AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION IsSave RETURNS LOGICAL 
   (INPUT cReports    AS CHARACTER) FORWARD.

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
   lcInput = "68 70 71 70 72 75 76 76".
   MESSAGE STRING (lcInput)
   VIEW-AS ALERT-BOX.
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

   ASSIGN 
      ttLine.cInputList = SUBSTITUTE ("[&1]", REPLACE(ttLine.cInputLine, " ", ", "))
   . 
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttLine:
      ttLine.IsSave = IsSave(ttLine.cInputLine).
      IF ttLine.IsSave THEN 
         iSolution = iSolution + 1.
   END.

   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 01 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:

   iSolution = 0.
   
   FOR EACH ttLine:
      
      ttLine.IsSave = IsSave(ttLine.cInputLine).
      
      IF ttLine.IsSave EQ FALSE THEN DO:
         /* Try alternative lists with a single skip */
         SkipBlock:
         DO iSkip = 1 TO NUM-ENTRIES (ttLine.cInputLine, " "):
            cNewReports = "".
            DO iNewReport = 1 TO NUM-ENTRIES (ttLine.cInputLine, " "):
               IF iNewReport NE iSkip THEN
                  cNewReports = SUBSTITUTE ("&1&2&3",
                                            cNewReports, 
                                            (IF cNewReports NE "" THEN " " ELSE ""), 
                                            ENTRY (iNewReport, ttLine.cInputLine, " ")).
            END.
            IF IsSave(cNewReports) THEN DO: 
               ttLine.IsSave = TRUE.
               LEAVE SkipBlock.
            END.
         END.
      END. /* Try alternative lists with a single skip */
      IF ttLine.IsSave EQ TRUE THEN 
         iSolution = iSolution + 1.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 01 - Part Two".
END. /* Process Part Two */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
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


/* ************************  Function Implementations ***************** */

FUNCTION IsSave RETURNS LOGICAL 
   (INPUT cReports    AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Determines if a Report is Save
 Notes:   A Report is save if all numbers are 
            increasing with at least one or at most three
            decreasing by at least one and at most three
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iNumber     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNumber     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrev       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNext       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDelta      AS INTEGER   NO-UNDO.

   DO iReport = 1 TO NUM-ENTRIES (cReports, " "):
      cNumber = ENTRY (iReport, cReports, " ").
      iDelta = INTEGER (cNumber) - INTEGER (cPrev).
      IF iReport EQ 2 THEN 
      DO:
         IF iDelta EQ 0 THEN
            RETURN FALSE.               
         lDecreasing = iDelta LT 0.
      END.
      IF iReport GE 2 THEN 
      DO:
         
         IF lDecreasing EQ TRUE AND (iDelta GE 0 OR iDelta LT -3) THEN 
         DO:
            RETURN FALSE.               
         END.
         
         IF lDecreasing EQ FALSE AND (iDelta LE 0 OR iDelta GT 3) THEN 
         DO:
            RETURN FALSE.               
         END.
      END.
      cPrev = cNumber.   
   END.
   
   RETURN TRUE.
      
END FUNCTION.
