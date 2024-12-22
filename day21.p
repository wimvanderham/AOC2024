
/*------------------------------------------------------------------------
    File        : day21.p
    Purpose     : Solve Day 21 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 21 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Sat Dec 21 13:12:12 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 21.
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

   FIELD cPressPath1 AS CHARACTER FORMAT "X(20)"
   FIELD cPressPath2 AS CHARACTER FORMAT "X(20)"
   FIELD cPressPath3 AS CHARACTER FORMAT "X(20)"
   FIELD iLength     AS INTEGER
   FIELD iNumeric    AS INTEGER
   FIELD iComplexity AS INTEGER  
   
   INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttGrid
   FIELD iRow     AS INTEGER 
   FIELD iCol     AS INTEGER 
   FIELD cChar    AS CHARACTER FORMAT "X"
   FIELD iSteps   AS INTEGER 
   INDEX indRowCol IS UNIQUE iRow iCol.
DEFINE BUFFER ttNextGrid FOR ttGrid.

DEFINE TEMP-TABLE ttNumericGrid LIKE ttGrid.

DEFINE TEMP-TABLE ttNumericPath
   FIELD cFromChar  AS CHARACTER 
   FIELD cToChar    AS CHARACTER 
   FIELD cPath      AS CHARACTER 
   FIELD cOptimized AS CHARACTER 
INDEX indPath IS UNIQUE cFromChar cToChar.
   
DEFINE TEMP-TABLE ttDirectionalGrid LIKE ttGrid.

DEFINE TEMP-TABLE ttDirectionalPath LIKE ttNumericPath.   
   
DEFINE TEMP-TABLE ttDirection
   FIELD cDirection     AS CHARACTER 
   FIELD iDeltaRow      AS INTEGER 
   FIELD iDeltaCol      AS INTEGER 
   INDEX indDirection IS UNIQUE cDirection.

DEFINE VARIABLE iRow       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCol       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPosRow    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPosCol    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxRow    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxCol    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDirection AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFoundStep AS INTEGER   NO-UNDO.
   
  
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getOptimized RETURNS CHARACTER 
   (INPUT ipcPath AS CHARACTER) FORWARD.

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
   lcInput = "029A~n980A~n179A~n456A~n379A".
   lcInput = "379A".
   MESSAGE STRING (lcInput)
      VIEW-AS ALERT-BOX TITLE " Debug Input ".
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

END. /* ReadBlock: */

RUN fillDirections.

RUN createNumericKeyboard.
RUN createNumericPaths.

RUN createDirectionalKeyboard.
RUN createDirectionalPaths.

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttNumericGrid:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttNumericPath:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttDirectionalGrid:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttDirectionalPath:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttLine:
      RUN pressNumericPath
         (INPUT  ttLine.cInputLine,
          OUTPUT ttLine.cPressPath1).
      RUN pressDirectionalPath
         (INPUT  ttLine.cPressPath1,
          OUTPUT ttLine.cPressPath2).  
      RUN pressDirectionalPath
         (INPUT  ttLine.cPressPath2,
          OUTPUT ttLine.cPressPath3).

      ASSIGN 
         ttLine.iLength  = LENGTH (ttLine.cPressPath3)
         ttLine.iNumeric = INTEGER (TRIM (ttLine.cInputLine, "A"))
         ttLine.iComplexity = ttLine.iLength * ttLine.iNumeric
      .
      iSolution = iSolution + ttLine.iComplexity.                                    
   END.
       
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 21 - Part One".
END. /* Process Part One */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).      
END.

IF lPart[2] THEN 
DO:

   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 21 - Part Two".
   IF lvlShow THEN 
   DO:
   END.
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

PROCEDURE createDirectionalKeyboard:
/*------------------------------------------------------------------------------
 Purpose: Create Directional Keyboard in a grid (Row, Col) = Direction
 Notes:
 Layout Directional Keyboard:
       +---+---+
       | ^ | A |
   +---+---+---+
   | < | v | > |
   +---+---+---+        
------------------------------------------------------------------------------*/
DEFINE BUFFER ttDirectionalGrid FOR ttDirectionalGrid.

DEFINE VARIABLE cDirectionalKeyboard AS CHARACTER NO-UNDO INITIAL " ^A~n<v>".

DEFINE VARIABLE iLine            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine            AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCol             AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar            AS CHARACTER NO-UNDO.

   DO iLine = 1 TO NUM-ENTRIES (cDirectionalKeyboard, "~n"):
      cLine = ENTRY (iLine, cDirectionalKeyboard, "~n").
      DO iCol = 1 TO LENGTH (cLine):
         cChar = TRIM (SUBSTRING (cLine, iCol, 1)).
         IF cChar NE "" THEN 
         DO:
            CREATE ttDirectionalGrid.
            ASSIGN 
               ttDirectionalGrid.iRow  = iLine
               ttDirectionalGrid.iCol  = iCol
               ttDirectionalGrid.cChar = cChar
               .
         END.
      END.
   END.

END PROCEDURE.

PROCEDURE createDirectionalPaths:
/*------------------------------------------------------------------------------
 Purpose: Create Pats from any button to any other button on the Directional Keyboard
 Notes:
------------------------------------------------------------------------------*/
DEFINE BUFFER ttDirectionalGrid     FOR ttDirectionalGrid.
DEFINE BUFFER ttDirectionalGridFrom FOR ttDirectionalGrid.
DEFINE BUFFER tTDirectionalGridTo   FOR ttDirectionalGrid.

   DEFINE VARIABLE iStep     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lNewStep  AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE cPrevPath AS CHARACTER NO-UNDO.

   FOR EACH ttDirectionalGridFrom:
      /* From all Starting Positions */
      FOR EACH ttDirectionalGridTo:
         /* Reset Steps counter */
         IF  ttDirectionalGridTo.iRow EQ ttDirectionalGridFrom.iRow
         AND ttDirectionalGridTo.iCol EQ ttDirectionalGridFrom.iCol THEN DO:
            ttDirectionalGridTo.iSteps = 0.
            CREATE ttDirectionalPath.
            ASSIGN
               ttDirectionalPath.cFromChar = ttDirectionalGridFrom.cChar
               ttDirectionalPath.cToChar   = ttDirectionalGridTo.cChar
               ttDirectionalPath.cPath     = ""
            .
         END.                   
         ELSE 
            ttDirectionalGridTo.iSteps = ?.
      END. /* Reset Steps counter */
      iStep = 0.
      RepeatBlock:
      REPEAT:
         lNewStep = FALSE.
         FOR EACH ttDirectionalGrid
            WHERE ttDirectionalGrid.iSteps EQ iStep:
            /* From each start position */
            FOR EACH ttDirection,
               FIRST ttDirectionalGridTo
               WHERE ttDirectionalGridTo.iRow   EQ ttDirectionalGrid.iRow + ttDirection.iDeltaRow
               AND   ttDirectionalGridTo.iCol   EQ ttDirectionalGrid.iCol + ttDirection.iDeltaCol
               AND   ttDirectionalGridTo.iSteps EQ ?:
               /* Search all unreached next positions and save path */
               ttDirectionalGridTo.iSteps = ttDirectionalGrid.iSteps + 1.
               FIND  ttDirectionalPath
               WHERE ttDirectionalPath.cFromChar EQ ttDirectionalGridFrom.cChar
               AND   ttDirectionalPath.cToChar   EQ ttDirectionalGrid.cChar NO-ERROR.
               IF AVAILABLE ttDirectionalPath THEN 
                  cPrevPath = ttDirectionalPath.cPath.
               ELSE 
                  cPrevPath = "".
               lNewStep = TRUE.
               CREATE ttDirectionalPath.
               ASSIGN 
                  ttDirectionalPath.cFromChar  = ttDirectionalGridFrom.cChar
                  ttDirectionalPath.cToChar    = ttDirectionalGridTo.cChar
                  ttDirectionalPath.cPath      = cPrevPath + ttDirection.cDirection
                  ttDirectionalPath.cOptimized = getOptimized(ttDirectionalPath.cPath)
                  .
            END. /* Search all unreached next positions and save path */
         END. /* From each start position */
         IF lNewStep EQ FALSE THEN 
            LEAVE.
         iStep = iStep + 1.
      END. /* RepeatBlock */
   END.

END PROCEDURE.

PROCEDURE createNumericKeyboard:
/*------------------------------------------------------------------------------
 Purpose: Create Numeric Keyboard in a grid (Row,Col) = Char
 Notes:
 Layout Numeric Keyboard:
 +---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
------------------------------------------------------------------------------*/
DEFINE BUFFER ttNumericGrid FOR ttNumericGrid.

DEFINE VARIABLE cNumericKeyboard AS CHARACTER NO-UNDO INITIAL "789~n456~n123~n 0A".

DEFINE VARIABLE iLine AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCol  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.

   DO iLine = 1 TO NUM-ENTRIES (cNumericKeyboard, "~n"):
      cLine = ENTRY (iLine, cNumericKeyboard, "~n").
      DO iCol = 1 TO LENGTH (cLine):
         cChar = TRIM (SUBSTRING (cLine, iCol, 1)).
         IF cChar NE "" THEN DO:
            CREATE ttNumericGrid.
            ASSIGN 
               ttNumericGrid.iRow  = iLine
               ttNumericGrid.iCol  = iCol
               ttNumericGrid.cChar = cChar
            .
         END.
      END.
   END.
   
   
END PROCEDURE.

PROCEDURE createNumericPaths:
/*------------------------------------------------------------------------------
 Purpose: Create Paths from any button to any other button on the Numeric Keyboard
 Notes:
------------------------------------------------------------------------------*/
DEFINE BUFFER ttNumericGrid     FOR ttNumericGrid.
DEFINE BUFFER ttNumericGridFrom FOR ttNumericGrid.
DEFINE BUFFER tTNumericGridTo   FOR ttNumericGrid.

DEFINE VARIABLE iStep     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lNewStep  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPrevPath AS CHARACTER NO-UNDO.

   FOR EACH ttNumericGridFrom:
      /* From all Starting Positions */
      FOR EACH ttNumericGridTo:
         /* Reset Steps counter */
         IF  ttNumericGridTo.iRow EQ ttNumericGridFrom.iRow
         AND ttNumericGridTo.iCol EQ ttNumericGridFrom.iCol THEN DO:
            ttNumericGridTo.iSteps = 0.
            CREATE ttNumericPath.
            ASSIGN
               ttNumericPath.cFromChar = ttNumericGridFrom.cChar
               ttNumericPath.cToChar   = ttNumericGridTo.cChar
               ttNumericPath.cPath     = ""
            .
         END.
         ELSE 
            ttNumericGridTo.iSteps = ?.
      END. /* Reset Steps counter */
      iStep = 0.
      RepeatBlock:
      REPEAT:
         lNewStep = FALSE.
         FOR EACH ttNumericGrid
         WHERE ttNumericGrid.iSteps EQ iStep:
            /* From each start position */
            FOR EACH ttDirection,
            FIRST ttNumericGridTo
            WHERE ttNumericGridTo.iRow   EQ ttNumericGrid.iRow + ttDirection.iDeltaRow
            AND   ttNumericGridTo.iCol   EQ ttNumericGrid.iCol + ttDirection.iDeltaCol
            AND   ttNumericGridTo.iSteps EQ ?:
               /* Search all unreached next positions and save path */
               ttNumericGridTo.iSteps = ttNumericGrid.iSteps + 1.
               FIND  ttNumericPath
               WHERE ttNumericPath.cFromChar EQ ttNumericGridFrom.cChar
               AND   ttNumericPath.cToChar   EQ ttNumericGrid.cChar NO-ERROR.
               IF AVAILABLE ttNumericPath THEN 
                  cPrevPath = ttNumericPath.cPath.
               ELSE 
                  cPrevPath = "".
               lNewStep = TRUE.
               CREATE ttNumericPath.
               ASSIGN 
                  ttNumericPath.cFromChar  = ttNumericGridFrom.cChar
                  ttNumericPath.cToChar    = ttNumericGridTo.cChar
                  ttNumericPath.cPath      = cPrevPath + ttDirection.cDirection
                  ttNumericPath.cOptimized = getOptimized(ttNumericPath.cPath)
               .
            END. /* Search all unreached next positions and save path */
         END. /* From each start position */
         IF lNewStep EQ FALSE THEN 
            LEAVE.
         iStep = iStep + 1.
      END. /* RepeatBlock */
   END.

END PROCEDURE.

PROCEDURE fillDirections:
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/

   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection     = "^"
      ttDirection.iDeltaRow      = -1
      ttDirection.iDeltaCol      = 0
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection     = ">"
      ttDirection.iDeltaRow      = 0
      ttDirection.iDeltaCol      = +1
   .

   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection     = "v"
      ttDirection.iDeltaRow      = +1
      ttDirection.iDeltaCol      = 0
   .
      
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection     = "<"
      ttDirection.iDeltaRow      = 0
      ttDirection.iDeltaCol      = -1
   .

END PROCEDURE.

PROCEDURE pressDirectionalPath:
/*------------------------------------------------------------------------------
 Purpose: Determines the Path to press a Code on the Directional Keyboard
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCode AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.

DEFINE BUFFER ttDirectionalPath FOR ttDirectionalPath.

DEFINE VARIABLE iCode       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentPos AS CHARACTER NO-UNDO.

   cCurrentPos = "A".
   
   DO iCode = 1 TO LENGTH (ipcCode):
      cCode = SUBSTRING (ipcCode, iCode, 1).
      FIND  ttDirectionalPath
      WHERE ttDirectionalPath.cFromChar EQ cCurrentPos
      AND   ttDirectionalPath.cToChar   EQ cCode NO-ERROR.
      IF NOT AVAILABLE ttDirectionalPath THEN DO:
         MESSAGE SUBSTITUTE ("No Directional Path from &1 to &2.", cCurrentPos, cCode)
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         opcPath = opcPath + ttDirectionalPath.cOptimized + "A".
      END.
      cCurrentPos = cCode.
   END.

END PROCEDURE.

PROCEDURE pressNumericPath:
/*------------------------------------------------------------------------------
 Purpose: Determines the Path to press a Code on the Numeric Keyboard
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCode AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.

DEFINE BUFFER ttNumericPath FOR ttNumericPath.

DEFINE VARIABLE iCode       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCode       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentPos AS CHARACTER NO-UNDO.

   cCurrentPos = "A".
   
   DO iCode = 1 TO LENGTH (ipcCode):
      cCode = SUBSTRING (ipcCode, iCode, 1).
      FIND  ttNumericPath
      WHERE ttNumericPath.cFromChar EQ cCurrentPos
      AND   ttNumericPath.cToChar   EQ cCode.
      opcPath = opcPath + ttNumericPath.cOptimized + "A".
      cCurrentPos = cCode.
   END.
   
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION getOptimized RETURNS CHARACTER 
(INPUT ipcPath AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Optimize Path to limit steps
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE cOptimized AS CHARACTER NO-UNDO.

   CASE ipcPath:
      WHEN "<v<" THEN 
         cOptimized = "v<<".
      WHEN ">^>" THEN 
         cOptimized = ">>^".
      OTHERWISE
         cOptimized = ipcPath.
   END CASE.
   
   RETURN cOptimized.
      
END FUNCTION.

