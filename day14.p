
/*------------------------------------------------------------------------
    File        : day14.p
    Purpose     : Solve Day 14 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 14 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Sat Dec 14 19:06:11 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 14.
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
DEFINE VARIABLE edProgress AS CHARACTER NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine     AS INTEGER 
   FIELD cInputLine AS CHARACTER FORMAT "X(80)"

INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttRobot
   FIELD IDRobot  AS INTEGER 
   FIELD RowPos   AS INTEGER 
   FIELD ColPos   AS INTEGER 
   FIELD deltaRow AS INTEGER 
   FIELD deltaCol AS INTEGER
   FIELD Quadrant AS INTEGER  
   /* Fields for Part Two */
   FIELD startRow AS INTEGER 
   FIELD startCol AS INTEGER 
INDEX indID IS UNIQUE IDRobot
INDEX indRow RowPos
INDEX indCol ColPos
INDEX indRowCol RowPos ColPos.
   
DEFINE VARIABLE lviMaxCol   AS INTEGER NO-UNDO INIT 101.
DEFINE VARIABLE lviMaxRow   AS INTEGER NO-UNDO INIT 103.
DEFINE VARIABLE iSecond     AS INTEGER NO-UNDO.
DEFINE VARIABLE iCenterCol  AS INTEGER NO-UNDO.
DEFINE VARIABLE iCenterRow  AS INTEGER NO-UNDO.
DEFINE VARIABLE iQuadrant   AS INTEGER NO-UNDO EXTENT 4.
DEFINE VARIABLE iMaxSeconds AS INTEGER NO-UNDO.
DEFINE VARIABLE lFound      AS LOGICAL NO-UNDO.
DEFINE VARIABLE iMinLength  AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttGridLine
   FIELD iSeconds AS INTEGER 
   FIELD iRow     AS INTEGER 
   FIELD cLine    AS CHARACTER FORMAT "X(101)"
INDEX indSeconds IS UNIQUE iSeconds iRow.

/* ********************  Preprocessor Definitions  ******************** */

{AOC_session.i}

  DISPLAY
   SUBSTITUTE ("Year &1 Day &2", iYear, iDay) FORMAT "X(16)" NO-LABELS SKIP
   lOpenURL  LABEL "Open URL?"       VIEW-AS TOGGLE-BOX SKIP
   lDownload LABEL "Download Input?" VIEW-AS TOGGLE-BOX SKIP   
   lPart[1]  LABEL "Solve Part 1?"   VIEW-AS TOGGLE-BOX SKIP
   lPart[2]  LABEL "Solve Part 2?"   VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug  LABEL "Debug?"          VIEW-AS TOGGLE-BOX SKIP 
   lvlShow   LABEL "Show?"           VIEW-AS TOGGLE-BOX SKIP(2)
   edProgress LABEL "Progress"       VIEW-AS EDITOR SIZE-CHARS 60 BY 10
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
   lcInput = "p=0,4 v=3,-3~np=6,3 v=-1,-3~np=10,3 v=-1,2~np=2,0 v=2,-1~np=0,0 v=1,3~np=3,0 v=-2,-2~np=7,6 v=-1,-3~np=3,0 v=-1,-2~np=9,3 v=2,3~np=7,3 v=-1,2~np=2,4 v=2,-3~np=9,5 v=-3,-3".
   ASSIGN 
      lviMaxCol = 11
      lviMaxRow = 7
   .
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

   CREATE ttRobot.
   ASSIGN
      ttRobot.IDRobot  = ttLine.IDLine
      ttRobot.ColPos   = INTEGER (ENTRY (1, ENTRY (2, ENTRY (1, ttLine.cInputLine, " "), "=")))
      ttRobot.RowPos   = INTEGER (ENTRY (2, ENTRY (2, ENTRY (1, ttLine.cInputLine, " "), "=")))
      ttRobot.deltaCol = INTEGER (ENTRY (1, ENTRY (2, ENTRY (2, ttLine.cInputLine, " "), "=")))
      ttRobot.deltaRow = INTEGER (ENTRY (2, ENTRY (2, ENTRY (2, ttLine.cInputLine, " "), "=")))
   .
   ASSIGN 
      ttRobot.startCol = ttRobot.ColPos
      ttRobot.startRow = ttRobot.RowPos
   .
   
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRobot:HANDLE).      
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   IF lvlShow THEN 
   DO:
      OS-DELETE VALUE ("output/14.txt").
      IF lvlDebug THEN 
         RUN outputGrid
            (INPUT SUBSTITUTE ("output/14_&1.txt", STRING (iSecond, "999")),
             INPUT SUBSTITUTE ("Start Grid", iSecond),
             INPUT TRUE).
      ELSE 
         RUN outputGrid
            (INPUT SUBSTITUTE ("output/14_&1.txt", STRING (iSecond, "999")),
             INPUT SUBSTITUTE ("Start Grid", iSecond),
             INPUT FALSE).
   END.
   DO iSecond = 1 TO 100:
      FOR EACH ttRobot:
         RUN moveRobot
            (INPUT  ttRobot.ColPos,
             INPUT  ttRobot.RowPos,
             INPUT  ttRobot.deltaCol,
             INPUT  ttRobot.deltaRow,
             OUTPUT ttRobot.ColPos,
             OUTPUT ttRobot.RowPos).
      END.
      IF lvlShow THEN 
      DO:
         IF lvlDebug THEN 
            RUN outputGrid
               (INPUT SUBSTITUTE ("output/14_&1.txt", STRING (iSecond, "999")),
                INPUT SUBSTITUTE ("After &1 Second&2:", iSecond, (IF iSecond GT 1 THEN "s" ELSE "")),
                INPUT TRUE).
         ELSE 
            RUN outputGrid
               (INPUT SUBSTITUTE ("output/14.txt", STRING (iSecond, "999")),
                INPUT SUBSTITUTE ("After &1 Second&2:", iSecond, (IF iSecond GT 1 THEN "s" ELSE "")),
                INPUT FALSE).
      END.
   END.             
  
   ASSIGN
      iCenterCol = (lviMaxCol - 1) / 2
      iCenterRow = (lviMaxRow - 1) / 2
   .

   FOR EACH ttRobot
   WHERE ttRobot.ColPos NE iCenterCol
   AND   ttRobot.RowPos NE iCenterRow:
      IF ttRobot.ColPos LT iCenterCol THEN 
         IF ttRobot.RowPos LT iCenterRow THEN 
            ttRobot.Quadrant = 1.
         ELSE
            ttRobot.Quadrant = 3.
      ELSE 
         IF ttRobot.RowPos LT iCenterRow THEN 
            ttRobot.Quadrant = 2.
         ELSE 
            ttRobot.Quadrant = 4.
   END.
   
   iSolution = 1.
   FOR EACH ttRobot
   WHERE ttRobot.Quadrant GT 0:
      ASSIGN 
         iQuadrant[ttRobot.Quadrant] = iQuadrant[ttRobot.Quadrant] + 1
      .
   END.

   IF lvlShow THEN DO:
      OUTPUT TO "output/14.txt" APPEND.
      FOR EACH ttRobot:
         PUT UNFORMATTED 
            SUBSTITUTE ("&1 &2",
                        ttRobot.IDRobot - 1,
                        ttRobot.Quadrant) SKIP.
      END.                     
      PUT UNFORMATTED 
         SUBSTITUTE ("&1 &2 &3 &4", 
                     iQuadrant[1],
                     iQuadrant[2],
                     iQuadrant[3],
                     iQuadrant[4]) SKIP.
      OUTPUT CLOSE.
   END.
               
   iSolution = iQuadrant[1] * iQuadrant[2] * iQuadrant[3] * iQuadrant[4].   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 14 - Part One".
END. /* Process Part One */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRobot:HANDLE).
END.

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   ASSIGN 
      iSolution   = 0
      iMaxSeconds = 10000.
      iMinLength  = 5
   .

   SecondBlock:
   REPEAT:
      iSecond = iSecond + 1.
      FOR EACH ttRobot:
         RUN moveRobot
            (INPUT  ttRobot.ColPos,
             INPUT  ttRobot.RowPos,
             INPUT  ttRobot.deltaCol,
             INPUT  ttRobot.deltaRow,
             OUTPUT ttRobot.ColPos,
             OUTPUT ttRobot.RowPos).
      END.
      IF iSecond MOD 500 EQ 0 THEN 
         edProgress:INSERT-STRING (SUBSTITUTE ("&1: Start &2~n", STRING (NOW, "99-99-9999 HH:MM:SS"), iSecond)).
         RUN checkGrid
            (INPUT iSecond,
             INPUT iMinLength,       /* Minimum length */
             OUTPUT lFound).
      IF lFound THEN DO:
         RUN outputGrid
            (INPUT SUBSTITUTE ("output/14_&1.txt", iSecond),
             INPUT SUBSTITUTE ("Grid after &1 seconds.", iSecond),
             INPUT TRUE).
         RUN sy\win\show-file.w
            (INPUT SUBSTITUTE ("output/14_&1.txt", iSecond))).
         MESSAGE "Found Christmas Tree Easter Egg?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lFound.
         IF lFound EQ TRUE THEN 
            LEAVE SecondBlock.
         IF lFound EQ ? THEN
            RETURN.
         IF lFound EQ FALSE THEN DO:
            MESSAGE "Update minimum length:" UPDATE iMinLength.
         END.
      END.
      IF iSecond GE iMaxSeconds THEN 
         LEAVE SecondBlock.
   END.
         
   iSolution = iSecond.
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 14 - Part Two".
END. /* Process Part Two */

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

PROCEDURE moveRobot:
/*------------------------------------------------------------------------------
 Purpose: Move a Robot from input Pos to output Pos 
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiColPos    AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiRowPos    AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipideltaCol  AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipideltaRow  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiNewColPos AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiNewRowPos AS INTEGER NO-UNDO.

   ASSIGN 
      opiNewColPos = ipiColPos + ipideltaCol
      opiNewRowPos = ipiRowPos + ipideltaRow
   .
   IF opiNewColPos GE lviMaxCol THEN 
      opiNewColPos = opiNewColPos - lviMaxCol.
   IF opiNewColPos LT 0 THEN 
      opiNewColPos = opiNewColPos + lviMaxCol.
      
   IF opiNewRowPos GE lviMaxRow THEN 
      opiNewRowPos = opiNewRowPos - lviMaxRow.
   IF opiNewRowPos LT 0 THEN 
      opiNewRowPos = opiNewRowPos + lviMaxRow.

END PROCEDURE.

PROCEDURE outputGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTitle    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplGrid     AS LOGICAL NO-UNDO.

DEFINE BUFFER ttRobot FOR ttRobot.

DEFINE VARIABLE iRow AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol AS INTEGER NO-UNDO.

   IF iplGrid EQ TRUE THEN 
      OUTPUT TO VALUE (ipcFileName).
   ELSE 
      OUTPUT TO VALUE (ipcFileName) APPEND.
   IF iplGrid THEN DO:
      PUT UNFORMATTED 
         ipcTitle SKIP.
      
      DO iRow = 0 TO lviMaxRow - 1:
         DO iCol = 0 TO lviMaxCol - 1:
            FOR EACH ttRobot
            WHERE ttRobot.RowPos EQ iRow
            AND   ttRobot.ColPos EQ iCol:
               ACCUM "" (COUNT).
            END.
            IF (ACCUM COUNT "") EQ 0 THEN DO:
               PUT UNFORMATTED 
                  ".".
            END.
            ELSE DO:
               PUT UNFORMATTED 
                  (ACCUM COUNT "") MOD 10.
            END.
         END.
         PUT UNFORMATTED SKIP.
      END.
      FOR EACH ttRobot
         BY ttRobot.RowPos
         BY ttRobot.ColPos:
         PUT UNFORMATTED 
            SUBSTITUTE ("Robot &1 (&2,&3)", ttRobot.IDRobot, ttRobot.ColPos, ttRobot.RowPos) SKIP.
      END.
   END.
   ELSE DO:
      FOR EACH ttRobot:
         PUT UNFORMATTED 
            SUBSTITUTE ("&1 &2 &3", ttRobot.IDRobot - 1, ttRobot.ColPos, ttRobot.RowPos) SKIP.
      END.
   END.
   OUTPUT CLOSE.  

END PROCEDURE.

PROCEDURE checkGrid:
/*------------------------------------------------------------------------------
 Purpose: Checks if the current grid has a numer of consecutive robots
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiSeconds   AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiMinlength AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER oplFound     AS LOGICAL NO-UNDO.

DEFINE BUFFER ttRobot    FOR ttRobot.
DEFINE BUFFER ttGridLine FOR ttGridLine.

DEFINE VARIABLE iRow    AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol    AS INTEGER NO-UNDO.
DEFINE VARIABLE cLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
   
   DO iRow = 0 TO lviMaxRow:
      cLine = "".
      DO iCol = 0 TO lviMaxCol:
         FOR EACH ttRobot
         WHERE ttRobot.RowPos EQ iRow
         AND   ttRobot.ColPos EQ iCol:
            ACCUM "" (COUNT).
         END.
         IF (ACCUM COUNT "") EQ 0 THEN
            ASSIGN 
               cLine = cLine + "."
               iLength = 0
            .
         ELSE 
            ASSIGN 
               cLine = cLine + STRING ((ACCUM COUNT "") MOD 10)
               iLength = iLength + 1
            .
         IF iLength GE ipiMinLength THEN DO:
            CREATE ttGridLine.
            ASSIGN
               ttGridLine.iSeconds = ipiSeconds
               ttGridLine.iRow     = iRow
               ttGridLine.cLine    = cLine
            .
            oplFound = TRUE.
            RETURN.
         END.
      END.
   END.

END PROCEDURE.
