
/*------------------------------------------------------------------------
    File        : day13.p
    Purpose     : Solve Day 23 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 23 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Sat Dec 14 12:50:17 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 13.
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

DEFINE TEMP-TABLE ttMachine
   FIELD IDLine   AS INTEGER 
   FIELD iAX      AS INT64 
   FIELD iAY      AS INT64 
   FIELD iBX      AS INT64 
   FIELD iBY      AS INT64
   FIELD iPriceX  AS INT64 
   FIELD iPriceY  AS INT64 
   FIELD deTimesA AS DECIMAL 
   FIELD deTimesB AS DECIMAL 
   FIELD iTimesA  AS INT64 
   FIELD iTimesB  AS INT64 
   FIELD lWin     AS LOGICAL 
   FIELD iPrice   AS INT64 
INDEX indLine IS UNIQUE IDLine.
DEFINE VARIABLE iIDLine AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

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
   lcInput = "Button A: X+94, Y+34~nButton B: X+22, Y+67~nPrize: X=8400, Y=5400~n~nButton A: X+26, Y+66~nButton B: X+67, Y+21~nPrize: X=12748, Y=12176~n~nButton A: X+17, Y+86~nButton B: X+84, Y+37~nPrize: X=7870, Y=6450~n~nButton A: X+69, Y+23~nButton B: X+27, Y+71~nPrize: X=18641, Y=10279".
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

   IF ttLine.cInputLine BEGINS "Button A:" THEN DO:
      CREATE ttMachine.
      ASSIGN
         /* Button A: X+94, Y+34 */
         ttMachine.IDLine = ttLine.IDLine 
         ttMachine.iAX    = INT64 (ENTRY (2, ENTRY (1, ttLine.cInputLine), "X"))
         ttMachine.iAY    = INT64 (ENTRY (2, TRIM (ENTRY (2, ttLine.cInputLine)), "Y"))
      .
      iIDLine = ttMachine.IDLine.
   END.
   
   IF ttLine.cInputLine BEGINS "Button B:" THEN DO:
      FIND  ttMachine
      WHERE ttMachine.IDLine EQ iIDLine.
      ASSIGN 
         ttMachine.iBX = INT64 (ENTRY (2, ENTRY (1, ttLine.cInputLine), "X"))
         ttMachine.iBY = INT64 (ENTRY (2, TRIM (ENTRY (2, ttLine.cInputLine)), "Y"))
      .
   END.
   
   IF ttLine.cInputLine BEGINS "Prize:" THEN DO:
      FIND  ttMachine
      WHERE ttMachine.IDLine EQ iIDLine.
      ASSIGN 
         /* Prize: X=8400, Y=5400*/
         ttMachine.iPriceX = INT64 (ENTRY (2, ENTRY (1, TRIM (ENTRY (2, ttLine.cInputLine, ":"))), "="))
         ttMachine.iPriceY = INT64 (ENTRY (2, TRIM (ENTRY (2, ttLine.cInputLine)), "="))
      .
   END.
   
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).

   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttMachine:HANDLE).      
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttMachine:
      ASSIGN 
         ttMachine.deTimesB = (ttMachine.iPriceY * ttMachine.iAX - ttMachine.iAY * ttMachine.iPriceX) / (ttMachine.iBY * ttMachine.iAX - ttMachine.iAY * ttMachine.iBX)
         ttMachine.deTimesA = (ttMachine.iPriceX - ttMachine.iBX * ttMachine.deTimesB) / ttMachine.iAX
         ttMachine.iTimesA = INT64 (ttMachine.deTimesA)
         ttMachine.iTimesB = INT64 (ttMachine.deTimesB)
      .
      ttMachine.lWin = DECIMAL (ttMachine.iTimesA) EQ ttMachine.deTimesA AND DECIMAL (ttMachine.iTimesB) EQ ttMachine.deTimesB.
      
      IF ttMachine.lWin EQ TRUE THEN DO:
         ttMachine.iPrice = ttMachine.iTimesA * 3 + ttMachine.iTimesB.
         iSolution = iSolution + ttMachine.iPrice.
      END.
   END.
  
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 13 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   
   iSolution = 0.
   FOR EACH ttMachine:
      ASSIGN  
         ttMachine.iPriceX = ttMachine.iPriceX + 10000000000000
         ttMachine.iPriceY = ttMachine.iPriceY + 10000000000000
         ttMachine.lWin    = FALSE 
         ttMachine.iPrice  = 0
      .
      
      ASSIGN 
         ttMachine.deTimesB = (ttMachine.iPriceY * ttMachine.iAX - ttMachine.iAY * ttMachine.iPriceX) / (ttMachine.iBY * ttMachine.iAX - ttMachine.iAY * ttMachine.iBX)
         ttMachine.deTimesA = (ttMachine.iPriceX - ttMachine.iBX * ttMachine.deTimesB) / ttMachine.iAX
         ttMachine.iTimesA  = INT64 (ttMachine.deTimesA)
         ttMachine.iTimesB  = INT64 (ttMachine.deTimesB)
      .
      ttMachine.lWin = DECIMAL (ttMachine.iTimesA) EQ ttMachine.deTimesA AND DECIMAL (ttMachine.iTimesB) EQ ttMachine.deTimesB.
      
      IF ttMachine.lWin EQ TRUE THEN 
      DO:
         ttMachine.iPrice = ttMachine.iTimesA * 3 + ttMachine.iTimesB.
         iSolution = iSolution + ttMachine.iPrice.
      END.
      
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 13 - Part Two".
END. /* Process Part Two */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttMachine:HANDLE).
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
