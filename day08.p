
/*------------------------------------------------------------------------
    File        : day08.p
    Purpose     : Solve Day 8 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 8 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Tue Dec 10 00:49:09 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 08.
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

DEFINE TEMP-TABLE ttGrid
   FIELD iRow       AS INTEGER 
   FIELD iCol       AS INTEGER 
   FIELD cChar      AS CHARACTER 
   FIELD iASC       AS INTEGER /* ASCII value of Antenna */
   FIELD iAntiNode  AS INTEGER 
   /* Counter for Part Two */
   FIELD iAntiNodes AS INTEGER 
INDEX indRowCol IS UNIQUE iRow iCol
INDEX indASC    IS PRIMARY iASC iRow iCol
INDEX indAnti   iAntiNode.
DEFINE BUFFER ttNextGrid FOR ttGrid.
DEFINE BUFFER ttAntiGrid FOR ttGrid.

DEFINE VARIABLE iRow      AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol      AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaRow AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaCol AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewRow   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewCol   AS INTEGER NO-UNDO.

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
   lcInput = "............~n........0...~n.....0......~n.......0....~n....0.......~n......A.....~n............~n............~n........A...~n.........A..~n............~n............".
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

   iRow = iLine.
   DO iCol = 1 TO LENGTH (ttLine.cInputLine):
      CREATE ttGrid.
      ASSIGN 
         ttGrid.iRow  = iRow
         ttGrid.iCol  = iCol
         ttGrid.cChar = SUBSTRING (ttLine.cInput, iCol, 1)
      .
      IF ttGrid.cChar NE "." THEN
         ttGrid.iASC = ASC (ttGrid.cChar).
   END.   
   
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttGrid
      WHERE ttGrid.iASC NE 0:
      /* Check all Antenna's */
      FOR EACH ttNextGrid
         WHERE ttNextGrid.iASC EQ ttGrid.iASC
         AND  (ttNextGrid.iRow NE ttGrid.iRow OR ttNextGrid.iCol NE ttGrid.iCol):
         /* FOR EACH Equal Frequency Antenna */
         ASSIGN 
            iDeltaRow = ttNextGrid.iRow - ttGrid.iRow
            iDeltaCol = ttNextGrid.iCol - ttGrid.iCol
            .
         FIND  ttAntiGrid
            WHERE ttAntiGrid.iRow EQ ttNextGrid.iRow + iDeltaRow
            AND   ttAntiGrid.iCol EQ ttNextGrid.iCol + iDeltaCol NO-ERROR.
         IF AVAILABLE ttAntiGrid THEN 
            ttAntiGrid.iAntiNode = ttAntiGrid.iAntiNode  + 1. 
      END.
   END.
   
   FOR EACH ttGrid
      WHERE ttGrid.iAntiNode GT 0:
      ACCUM "" (COUNT).
   END.
   iSolution = (ACCUM COUNT "").

   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 08 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:

   iSolution = 0.

   FOR EACH ttGrid
   WHERE ttGrid.iASC NE 0:
      /* Check all Antenna's */
      ttGrid.iAntiNodes = 1.
      FOR EACH ttNextGrid
      WHERE ttNextGrid.iASC EQ ttGrid.iASC
      AND  (ttNextGrid.iRow NE ttGrid.iRow OR ttNextGrid.iCol NE ttGrid.iCol):
         /* FOR EACH Equal Frequency Antenna */
         ASSIGN 
            iDeltaRow = ttNextGrid.iRow - ttGrid.iRow
            iDeltaCol = ttNextGrid.iCol - ttGrid.iCol
            iNewRow   = ttNextGrid.iRow + iDeltaRow
            iNewCol   = ttNextGrid.iCol + iDeltaCol
            .
         FIND  ttAntiGrid
            WHERE ttAntiGrid.iRow EQ iNewRow
            AND   ttAntiGrid.iCol EQ iNewCol NO-ERROR.
         DO WHILE AVAILABLE ttAntiGrid:
            ttAntiGrid.iAntiNodes = ttAntiGrid.iAntiNodes + 1.
            ASSIGN 
               iNewRow = iNewRow + iDeltaRow
               iNewCol = iNewCol + iDeltaCol
               .
            FIND  ttAntiGrid
               WHERE ttAntiGrid.iRow EQ iNewRow
               AND   ttAntiGrid.iCol EQ iNewCol NO-ERROR.
         END.               
      END.
   END.
   
   FOR EACH ttGrid
      WHERE ttGrid.iAntiNodes GT 0:
      ACCUM "" (COUNT).
   END.
   iSolution = (ACCUM COUNT "").
    
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 08 - Part Two".
END. /* Process Part Two */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE).
   RUN exportGrid
      (INPUT "output\08_2.txt").
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

PROCEDURE exportGrid:
/*------------------------------------------------------------------------------
 Purpose: Export a Grid in a text file
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.

DEFINE BUFFER ttGrid FOR ttGrid.

DEFINE VARIABLE iMaxRow AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxCol AS INTEGER NO-UNDO.

   FOR EACH ttGrid:
      ACCUM ttGrid.iRow (MAX).
      ACCUM ttGrid.iCol (MAX).
   END.
   ASSIGN 
      iMaxRow = (ACCUM MAX ttGrid.iRow)
      iMaxCol = (ACCUM MAX ttGrid.iCol)
   .
   
   OUTPUT TO VALUE (ipcOutputFile).
   DO iRow = 1 TO iMaxRow:
      DO iCol = 1 TO iMaxCol:
         FIND  ttGrid
         WHERE ttGrid.iRow EQ iRow
         AND   ttGrid.iCol EQ iCol.
         IF ttGrid.iASC NE 0 THEN 
            PUT UNFORMATTED 
               ttGrid.cChar.
         ELSE DO:         
            IF ttGrid.iAntiNodes GT 0 THEN 
               PUT UNFORMATTED 
                  "#".
            ELSE 
               PUT UNFORMATTED 
                  ttGrid.cChar.
         END.
      END.
      PUT UNFORMATTED SKIP.
   END.
      
   OUTPUT CLOSE.
END PROCEDURE.
