
/*------------------------------------------------------------------------
    File        : day05.p
    Purpose     : Solve Day 5 of Advent of Code 2024

    Syntax      :

    Description : Solution of Day 5 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Thu Dec 05 01:46:43 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 5.
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

   /* Specific fields */
   FIELD cSection       AS CHARACTER  
   FIELD lCorrect       AS LOGICAL 
   FIELD iMiddleNumber  AS INTEGER
   FIELD cCorrectedList AS CHARACTER FORMAT "X(80)"
          
   INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttBefore
   FIELD ThisNumber   AS INTEGER 
   FIELD BeforeNumber AS INTEGER 
INDEX indNumbers ThisNumber BeforeNumber.

DEFINE VARIABLE cSection    AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION checkList RETURNS LOGICAL 
   (INPUT ipcOrderedList   AS CHARACTER) FORWARD.

FUNCTION fixList RETURNS CHARACTER 
   (INPUT ipcUnorderedList AS CHARACTER) FORWARD.

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
   lcInput = "47|53~n97|13~n97|61~n97|47~n75|29~n61|13~n75|53~n29|13~n97|29~n53|29~n61|53~n97|53~n61|29~n47|13~n75|47~n97|75~n47|61~n75|61~n47|29~n75|13~n53|13~n~n75,47,61,53,29~n97,61,53,29,13~n75,29,13~n75,97,47,61,53~n61,13,29~n97,13,75,29,47".
END.

cSection = "before".
/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN DO:
      cSection = "pages". 
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
      .
   ttLine.cSection = cSection.

   CASE cSection:
      WHEN "before" THEN DO:
         CREATE ttBefore.
         ASSIGN 
            ttBefore.ThisNumber   = INTEGER (ENTRY (1, ttLine.cInputLine, "|"))
            ttBefore.BeforeNumber = INTEGER (ENTRY (2, ttLine.cInputLine, "|"))
         .
      END.
   END CASE.         
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttLine 
   WHERE ttLine.cSection EQ "pages":
      /* Check the ordering of pages */
      ttLine.lCorrect = checkList(ttLine.cInputLine).
      IF ttLine.lCorrect THEN DO:
         ASSIGN 
            ttLine.iMiddleNumber = INTEGER (ENTRY (INTEGER (NUM-ENTRIES (ttLine.cInputLine) / 2), ttLine.cInputLine))
            .
         iSolution = iSolution + ttLine.iMiddleNumber. 
      END.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 05 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:

   iSolution = 0.
   IF lPart[1] EQ FALSE THEN DO:
      /* First determine the unordered lists */
      FOR EACH ttLine
      WHERE ttLine.cSection EQ "pages":
         ttLine.lCorrect = checkList(ttLine.cInputLine).
      END.
   END.
   
   FOR EACH ttLine
   WHERE ttLine.cSection EQ "pages" 
   AND   ttLine.lCorrect EQ FALSE:
      /* Only consider the unordered lists */
      ttLine.cCorrectedList = ttLine.cInputLine.
      DO WHILE ttLine.lCorrect EQ FALSE:
         ttLine.cCorrectedList = fixList(ttLine.cCorrectedList).
         ttLine.lCorrect = checkList(ttLine.cCorrectedList).
      END.
      
      ASSIGN 
         ttLine.iMiddleNumber = INTEGER (ENTRY (INTEGER (NUM-ENTRIES (ttLine.cCorrectedList) / 2), ttLine.cCorrectedList))
      .
      iSolution = iSolution + ttLine.iMiddleNumber. 
   END.
   OUTPUT CLOSE.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 05 - Part Two".
END. /* Process Part Two */

IF lvlShow THEN DO:
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

FUNCTION checkList RETURNS LOGICAL 
(INPUT ipcOrderedList AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Checks if the input List is ordered
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iPage1      AS INTEGER NO-UNDO.
DEFINE VARIABLE iPage2      AS INTEGER NO-UNDO.
DEFINE VARIABLE lOrdered    AS LOGICAL NO-UNDO.
DEFINE VARIABLE iPageNumber AS INTEGER NO-UNDO EXTENT 2.

DEFINE BUFFER ttBefore FOR ttBefore.

   DO iPage1 = 1 TO NUM-ENTRIES (ipcOrderedList) - 1:
      DO iPage2 = iPage1 + 1 TO NUM-ENTRIES (ipcOrderedList):
         ASSIGN 
            ipageNumber[1] = INTEGER (ENTRY (iPage1, ipcOrderedList))
            ipageNumber[2] = INTEGER (ENTRY (iPage2, ipcOrderedList))
         .
         FIND FIRST ttBefore
         WHERE ttBefore.ThisNumber   EQ ipageNumber[1]
         AND   ttBefore.BeforeNumber EQ ipageNumber[2] NO-ERROR.
         IF NOT AVAILABLE ttBefore THEN 
         DO: 
            RETURN FALSE.
         END.
      END.
   END.

   RETURN TRUE.
   
END FUNCTION.

FUNCTION fixList RETURNS CHARACTER 
(INPUT ipcUnorderedList AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Order an unordered list
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iNumber1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iNumber2 AS INTEGER NO-UNDO.

DEFINE VARIABLE iPage       AS INTEGER.
DEFINE VARIABLE iNextPage   AS INTEGER.
DEFINE VARIABLE iPageNumber AS INTEGER NO-UNDO EXTENT 2.

DEFINE VARIABLE iFrom        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTo          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSwap        AS CHARACTER NO-UNDO.

   PageBlock:
   DO iPage = 1 TO NUM-ENTRIES (ipcUnorderedList) - 1:
      NextPageBlock:
      DO iNextPage = iPage + 1 TO NUM-ENTRIES (ipcUnorderedList):
         iPageNumber[1] = INTEGER (ENTRY (iPage, ipcUnorderedList)).
         iPageNumber[2] = INTEGER (ENTRY (iNextPage, ipcUnorderedList)).
         FIND FIRST ttBefore
         WHERE ttBefore.ThisNumber   EQ iPageNumber[1]
         AND   ttBefore.BeforeNumber EQ iPageNumber[2] NO-ERROR.
         IF NOT AVAILABLE ttBefore THEN DO:
            iTo = iPage.
            FOR EACH ttBefore
            WHERE ttBefore.BeforeNumber EQ iPageNumber[1]:
               iFrom = LOOKUP (STRING (ttBefore.ThisNumber), ipcUnorderedList). 
               IF iFrom GT iPage THEN DO:
                  /* Found a substiute */
                  cSwap = ENTRY (iFrom, ipcUnorderedList).
                  ENTRY (iFrom, ipcUnorderedList) = ENTRY (iTo, ipcUnorderedList).
                  ENTRY (iTo, ipcUnorderedList) = cSwap.
               END.
            END.
         END.
      END.
   END. 
   
   RETURN ipcUnorderedList.

END FUNCTION.
