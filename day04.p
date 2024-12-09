
/*------------------------------------------------------------------------
    File        : day04.p
    Purpose     : Solve Day 4 of Advent of code 2024

    Syntax      :

    Description : Solution for Day 4 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Wed Dec 04 00:36:40 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 4.
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
   FIELD cInputLine AS CHARACTER 

   INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttRow
   FIELD iRow AS INTEGER 
   FIELD cText AS CHARACTER FORMAT "X(60)"
INDEX indRow IS UNIQUE iRow.

DEFINE TEMP-TABLE ttColumn
   FIELD iCol AS INTEGER 
   FIELD cText AS CHARACTER FORMAT "X(60)"
INDEX indCol IS UNIQUE iCol.

DEFINE TEMP-TABLE ttDiagonal
   FIELD cDirection AS CHARACTER 
   FIELD iRow       AS INTEGER 
   FIELD iCol       AS INTEGER 
   FIELD cText      AS CHARACTER FORMAT "X(60)"
INDEX indDiagonal IS UNIQUE cDirection iRow iCol.

DEFINE TEMP-TABLE ttMAS
   FIELD cDirection AS CHARACTER 
   FIELD iRow_A     AS INTEGER 
   FIELD iCol_A     AS INTEGER 
   FIELD withX      AS LOGICAL 
INDEX indMAS IS UNIQUE cDirection iRow_A iCol_A.
DEFINE BUFFER ttSAM FOR ttMAS.

DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

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
   lcInput = "....XXMAS.~n.SAMXMS...~n...S..A...~n..A.A.MS.X~nXMASAMX.MM~nX.....XA.A~nS.S.S.S.SS~n.A.A.A.A.A~n..M.M.M.MM~n.X.X.XMASX".
   lcInput = ".M.S......~n..A..MSMS.~n.M.S.MAA..~n..A.ASMSM.~n.M.S.M....~n..........~nS.S.S.S.S.~n.A.A.A.A..~nM.M.M.M.M.~n..........".
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

IF lvlShow THEN DO:
   RUN outputGrid
      (INPUT "output\04.csv").
END.      

RUN fillTempTables
   (INPUT lcInput).
   
IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
      
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRow:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttColumn:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttDiagonal:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   RUN searchString 
      (INPUT "XMAS",
       OUTPUT iCount).
   iSolution = iSolution + iCount.
   RUN searchString 
      (INPUT "SAMX",
       OUTPUT iCount).
   iSolution = iSolution + iCount.
          
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 04 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:

   iSolution = 0.
   RUN searchSafeString
      (INPUT "MAS").
   RUN searchSafeString
      (INPUT "SAM").
      
   FOR EACH ttMAS
   WHERE ttMAS.cDirection EQ "UL->LR",
   FIRST ttSAM
   WHERE ttSAM.cDirection NE ttMAS.cDirection
   AND   ttSAM.iRow_A     EQ ttMAS.iRow_A
   AND   ttSAM.iCol_A     EQ ttMAS.iCol_A:
      ttMAS.withX = TRUE.
      iSolution = iSolution + 1.
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 04 - Part Two".
END. /* Process Part Two */

IF lvlShow THEN DO:
   IF lPart[2] THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttDiagonal:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttMAS:HANDLE).
   END.
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

PROCEDURE fillTempTables:
/*------------------------------------------------------------------------------
 Purpose: Fills the Temp-Tables for ttRow, ttColumn and ttDiagonal
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iplcInput AS LONGCHAR NO-UNDO.

DEFINE VARIABLE iRow       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCol       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxRow    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxCol    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLine      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCharRow   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCharCol   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDirection AS CHARACTER NO-UNDO.

   cLine = ENTRY (1, iplcInput, "~n").
   ASSIGN 
      iMaxRow = NUM-ENTRIES (iplcInput, "~n")
      iMaxCol = LENGTH (cLine)
   .
   
   /* Fill ttRow */
   DO iRow = 1 TO iMaxRow:
      CREATE ttRow.
      ASSIGN 
         ttRow.iRow = iRow
         ttRow.cText = ENTRY (iRow, iplcInput, "~n")
      .
   END.
   
   /* Fill ttColumn */
   DO iCol = 1 TO iMaxCol:
      CREATE ttColumn.
      ASSIGN 
         ttColumn.iCol = iCol
      .
      FOR EACH ttRow:
         ASSIGN 
           ttColumn.cText = ttColumn.cText + SUBSTRING (ttRow.cText, iCol, 1)
         .
      END.
   END.
     
   /* Fill ttDiagonal - Upper-Left to Lower-Right UL->LR*/
   cDirection = "UL->LR".
   DO iCol = 1 TO iMaxCol:
      /* All diagonals starting at Row 1 */
      ASSIGN
         iCharRow = 1
         iCharCol = iCol
      .
      CREATE ttDiagonal.
      ASSIGN 
         ttDiagonal.cDirection = cDirection
         ttDiagonal.iRow       = iCharRow
         ttDiagonal.iCol       = iCharCol
      .
      DO WHILE iCharRow LE iMaxRow AND iCharCol LE iMaxCol:
         cLine = ENTRY (iCharRow, iplcInput, "~n").
         ASSIGN 
            ttDiagonal.cText = ttDiagonal.cText + 
               SUBSTRING (cLine, iCharCol, 1)
         .
         ASSIGN 
            iCharRow = iCharRow + 1
            iCharCol = iCharCol + 1
         .
      END.
   END.
                  
   DO iRow = 2 TO iMaxRow:
      /* All diagonals start at Col 1 and Rows 2 to MaxRow */
      ASSIGN 
         iCharRow = iRow
         iCharCol = 1
      .
      CREATE ttDiagonal.
      ASSIGN 
         ttDiagonal.cDirection = cDirection 
         ttDiagonal.iRow       = iCharRow
         ttDiagonal.iCol       = iCharCol
      .
      DO WHILE iCharRow LE iMaxRow AND iCharCol LE iMaxCol:
         cLine = ENTRY (iCharRow, iplcInput, "~n").
         ASSIGN 
            ttDiagonal.cText = ttDiagonal.cText + 
               SUBSTRING (cLine, iCharCol, 1)
         .
         ASSIGN 
            iCharRow = iCharRow + 1
            iCharCol = iCharCol + 1
         .
      END.
   END.

   /* Fill ttDiagonal - Lower-Left to Upper-Right LL->UR */
   cDirection = "LL->UR".
   DO iCol = 1 TO iMaxCol:
      /* All diagonals starting at Row 1 */
      ASSIGN
         iCharRow = iMaxRow
         iCharCol = iCol
         .
      CREATE ttDiagonal.
      ASSIGN 
         ttDiagonal.cDirection = cDirection
         ttDiagonal.iRow       = iCharRow
         ttDiagonal.iCol       = iCharCol
         .
      DO WHILE iCharRow GE 1 AND iCharCol LE iMaxCol:
         cLine = ENTRY (iCharRow, iplcInput, "~n").
         ASSIGN 
            ttDiagonal.cText = ttDiagonal.cText + 
               SUBSTRING (cLine, iCharCol, 1)
            .
         ASSIGN 
            iCharRow = iCharRow - 1
            iCharCol = iCharCol + 1
            .
      END.
   END.
                  
   DO iRow = 1 TO iMaxRow - 1:
      /* All diagonals start at Col 1 and MaxRow - 1 */
      ASSIGN 
         iCharRow = iRow
         iCharCol = 1
         .
      CREATE ttDiagonal.
      ASSIGN 
        ttDiagonal.cDirection = cDirection
         ttDiagonal.iRow       = iCharRow
         ttDiagonal.iCol       = iCharCol
         .
      DO WHILE iCharRow GE 1 AND iCharCol LE iMaxCol:
         cLine = ENTRY (iCharRow, iplcInput, "~n").
         ASSIGN 
            ttDiagonal.cText = ttDiagonal.cText + 
               SUBSTRING (cLine, iCharCol, 1)
            .
         ASSIGN 
            iCharRow = iCharRow - 1
            iCharCol = iCharCol + 1
            .
      END.
   END.
END PROCEDURE.

PROCEDURE outputGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar AS INTEGER NO-UNDO.

   OUTPUT TO VALUE (ipcOutputFile).
   FOR EACH ttLine:
      DO iChar = 1 TO LENGTH (ttLine.cInputLine):
         PUT UNFORMATTED 
            SUBSTRING (ttLine.cInputLine, iChar, 1)
         .
         IF iChar LT LENGTH (ttLine.cInputLine) THEN 
            PUT UNFORMATTED ";".
      END.
      PUT UNFORMATTED SKIP.
   END.
   OUTPUT CLOSE.    

END PROCEDURE.

PROCEDURE searchString:
/*------------------------------------------------------------------------------
 Purpose: Searches for a string in the text of the 
          temp-tables ttRow, ttColumn and ttDiagonal
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcSearchString AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiStringCount  AS INTEGER NO-UNDO.

DEFINE VARIABLE iStart AS INTEGER NO-UNDO.
DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

   FOR EACH ttRow:
      iStart = 1.
      iIndex = INDEX (ttRow.cText, ipcSearchString, iStart).
      DO WHILE iIndex NE 0:
         opiStringCount = opiStringCount + 1.
         iStart = iIndex + 1.
         iIndex = INDEX (ttRow.cText, ipcSearchString, iStart).
      END.
   END.
   
   FOR EACH ttColumn:
      iStart = 1.
      iIndex = INDEX (ttColumn.cText, ipcSearchString, iStart).
      DO WHILE iIndex NE 0:
         opiStringCount = opiStringCount + 1.
         iStart = iIndex + 1.
         iIndex = INDEX (ttColumn.cText, ipcSearchString, iStart).
      END.
   END.

   FOR EACH ttDiagonal:
      iStart = 1.
      iIndex = INDEX (ttDiagonal.cText, ipcSearchString, iStart).
      DO WHILE iIndex NE 0:
         opiStringCount = opiStringCount + 1.
         iStart = iIndex + 1.
         iIndex = INDEX (ttDiagonal.cText, ipcSearchString, iStart).
      END.
   END.

END PROCEDURE.

PROCEDURE searchSafeString:
   /*------------------------------------------------------------------------------
    Purpose: Searches for a string in the text of the 
             temp-table ttDiagonal and saves it in the ttMAS temp-table
    Notes:
   ------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ipcSearchString AS CHARACTER NO-UNDO.

   DEFINE VARIABLE iStart AS INTEGER NO-UNDO.
   DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

   FOR EACH ttDiagonal:
      iStart = 1.
      iIndex = INDEX (ttDiagonal.cText, ipcSearchString, iStart).
      DO WHILE iIndex NE 0:
         CREATE ttMAS.
         ASSIGN
            ttMAS.cDirection = ttDiagonal.cDirection
         .
         IF ttMAS.cDirection EQ "UL->LR" THEN 
            ASSIGN 
               ttMAS.iRow_A = ttDiagonal.iRow + (iIndex - 1) + 1
               ttMAS.iCol_A = ttDiagonal.iCol + (iIndex - 1) + 1
            .
         ELSE 
            ASSIGN 
               ttMAS.iRow_A = ttDiagonal.iRow - (iIndex - 1) - 1
               ttMAS.iCol_A = ttDiagonal.iCol + (iIndex - 1) + 1
            .
         iStart = iIndex + 1.
         iIndex = INDEX (ttDiagonal.cText, ipcSearchString, iStart).
      END.
   END.

END PROCEDURE.
