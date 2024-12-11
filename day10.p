
/*------------------------------------------------------------------------
    File        : day10.p
    Purpose     : Solve Day 10 of Advent of Code 2024

    Syntax      :

    Description : Solution of Day 10 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Tue Dec 10 21:56:37 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 10.
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
   FIELD iRow   AS INTEGER 
   FIELD iCol   AS INTEGER 
   FIELD iValue AS INTEGER
   FIELD iScore AS INTEGER  
INDEX indRowCol IS UNIQUE iRow iCol
INDEX indValue  iValue iRow iCol.

DEFINE TEMP-TABLE ttStep
   FIELD iStep       AS INTEGER 
   FIELD iValue      AS INTEGER
   FIELD iRow        AS INTEGER 
   FIELD iCol        AS INTEGER 
   FIELD iScore      AS INTEGER 
   FIELD iStartRow   AS INTEGER 
   FIELD iStartCol   AS INTEGER
   FIELD cFromPath   AS CHARACTER FORMAT "X(100)"
   FIELD cDirections AS CHARACTER FORMAT "X(20)" 
INDEX indStep IS UNIQUE iStep iRow iCol iValue iStartRow iStartCol cFromPath.
DEFINE BUFFER ttNextStep FOR ttStep.

DEFINE VARIABLE iRow AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttDirection
   FIELD cDirection AS CHARACTER 
   FIELD iDeltaRow  AS INTEGER 
   FIELD iDeltaCol  AS INTEGER 
INDEX indDirection IS UNIQUE cDirection.

DEFINE VARIABLE iStep       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lNewStep    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFromPath   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDirections AS CHARACTER NO-UNDO.

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
   lcInput = "...0...~n...1...~n...2...~n6543456~n7.....7~n8.....8~n9.....9".
   lcInput = "89010123~n78121874~n87430965~n96549874~n45678903~n32019012~n01329801~n10456732".
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
         ttGrid.iRow   = iRow
         ttGrid.iCol   = iCol
         ttGrid.iValue = INTEGER (SUBSTRING (ttLine.cInput, iCol, 1))
      .
   END.   
   
END. /* ReadBlock: */

RUN fillDirections.

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

   // Process Part1
   iStep = 0.
   FOR EACH ttGrid
   WHERE ttGrid.iValue EQ 0:
      /* Find the starting points */
      CREATE ttStep.
      ASSIGN 
         ttStep.iStep     = iStep
         ttStep.iValue    = ttGrid.iValue
         ttStep.iRow      = ttGrid.iRow
         ttStep.iCol      = ttGrid.iCol
         ttStep.iScore    = 0
      .
   END.
   
   RepeatBlock:
   REPEAT:
      lNewStep = FALSE.
      FOR EACH ttStep
      WHERE ttStep.iStep EQ iStep:
         FOR EACH ttDirection,
         FIRST ttGrid
         WHERE ttGrid.iRow   EQ ttStep.iRow + ttDirection.iDeltaRow
         AND   ttGrid.iCol   EQ ttStep.iCol + ttDirection.iDeltaCol
         AND   ttGrid.iValue EQ ttStep.iValue + 1:
            FIND FIRST ttNextStep
            WHERE ttNextStep.iStep     EQ ttStep.iStep + 1
            AND   ttNextStep.iRow      EQ ttGrid.iRow
            AND   ttNextStep.iCol      EQ ttGrid.iCol
            AND   ttNextStep.iValue    EQ ttGrid.iValue
            AND   ttNextStep.iStartRow EQ ttStep.iStartRow
            AND   ttNextStep.iStartCol EQ ttStep.iStartCol NO-ERROR.
            IF NOT AVAILABLE ttNextStep THEN DO:
               lNewStep = TRUE.
               CREATE ttNextStep.
               ASSIGN 
                  ttNextStep.iStep     = ttStep.iStep + 1
                  ttNextStep.iRow      = ttGrid.iRow
                  ttNextStep.iCol      = ttGrid.iCol
                  ttNextStep.iValue    = ttGrid.iValue
                  ttNextStep.iStartRow = ttStep.iStartRow
                  ttNextStep.iStartCol = ttStep.iStartCol
               .
               IF ttNextStep.iValue EQ 9 THEN 
                  ttNextStep.iScore = 1.
            END.
         END.
      END.
      IF lNewStep EQ FALSE THEN 
         LEAVE RepeatBlock.
      iStep = iStep + 1.
   END.
   
   FOR EACH ttGrid
   WHERE ttGrid.iValue EQ 0:
      FOR EACH ttStep
      WHERE ttStep.iStartRow EQ ttGrid.iRow
      AND   ttStep.iStartCol EQ ttGrid.iCol
      AND   ttStep.iValue    EQ 9:
         ttGrid.iScore = ttGrid.iScore + 1.
      END.
      iSolution = iSolution + ttGrid.iScore.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 10 - Part One".

   IF lvlShow THEN 
   DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttStep:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
   END.


END. /* Process Part One */


IF lPart[2] THEN 
DO:
   /* Process Part Two */

   iSolution = 0.

   // Process Part 2
   iStep = 0.
   FOR EACH ttGrid
      WHERE ttGrid.iValue EQ 0:
      /* Find the starting points */
      CREATE ttStep.
      ASSIGN 
         ttStep.iStep       = iStep
         ttStep.iValue      = ttGrid.iValue
         ttStep.iRow        = ttGrid.iRow
         ttStep.iCol        = ttGrid.iCol
         ttStep.iScore      = 0
         ttStep.iStartRow   = ttGrid.iRow
         ttStep.iStartCol   = ttGrid.iCol
         ttStep.cFromPath   = SUBSTITUTE ("(&1,&2)", ttGrid.iRow, ttGrid.iCol)
         ttStep.cDirections = "0"
      .
   END.
   
   RepeatBlock:
   REPEAT:
      lNewStep = FALSE.
      FOR EACH ttStep
         WHERE ttStep.iStep EQ iStep:
         FOR EACH ttDirection,
         FIRST ttGrid
         WHERE ttGrid.iRow   EQ ttStep.iRow + ttDirection.iDeltaRow
         AND   ttGrid.iCol   EQ ttStep.iCol + ttDirection.iDeltaCol
         AND   ttGrid.iValue EQ ttStep.iValue + 1:
            cFromPath = SUBSTITUTE ("(&1,&2)<--&3", ttGrid.iRow, ttGrid.iCol, ttStep.cFromPath). 
            cDirections = SUBSTITUTE ("&1&2&3", ttStep.cDirections, ttDirection.cDirection, ttGrid.iValue).             
            FIND FIRST ttNextStep
               WHERE ttNextStep.iStep       EQ ttStep.iStep + 1
               AND   ttNextStep.iRow        EQ ttGrid.iRow
               AND   ttNextStep.iCol        EQ ttGrid.iCol
               AND   ttNextStep.iValue      EQ ttGrid.iValue
               AND   ttNextStep.iStartRow   EQ ttStep.iStartRow
               AND   ttNextStep.iStartCol   EQ ttStep.iStartCol 
               AND   ttNextStep.cFromPath   EQ cFromPath
               AND   ttNextStep.cDirections EQ cDirections
               NO-ERROR.
            IF NOT AVAILABLE ttNextStep THEN 
            DO:
               lNewStep = TRUE.
               CREATE ttNextStep.
               ASSIGN 
                  ttNextStep.iStep       = ttStep.iStep + 1
                  ttNextStep.iRow        = ttGrid.iRow
                  ttNextStep.iCol        = ttGrid.iCol
                  ttNextStep.iValue      = ttGrid.iValue
                  ttNextStep.iStartRow   = ttStep.iStartRow
                  ttNextStep.iStartCol   = ttStep.iStartCol
                  ttNextStep.cFromPath   = cFromPath
                  ttNextStep.cDirections = cDirections
               .
               IF ttNextStep.iValue EQ 9 THEN 
                  ttNextStep.iScore = 1.
            END.
         END.
      END.
      IF lNewStep EQ FALSE THEN 
         LEAVE RepeatBlock.
      iStep = iStep + 1.
   END.
   
   FOR EACH ttGrid
      WHERE ttGrid.iValue EQ 0:
      FOR EACH ttStep
         WHERE ttStep.iStartRow EQ ttGrid.iRow
         AND   ttStep.iStartCol EQ ttGrid.iCol
         AND   ttStep.iValue    EQ 9:
         ttGrid.iScore = ttGrid.iScore + 1.
      END.
      iSolution = iSolution + ttGrid.iScore.
   END.
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 10 - Part Two".
      
   IF lvlShow THEN 
   DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttStep:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
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

PROCEDURE fillDirections:
/*------------------------------------------------------------------------------
 Purpose: Fills all possible directions with deltaRow and deltaCol
 Notes:   ^ Up, v Down, > Right, < Left (no diagonals)
------------------------------------------------------------------------------*/
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "^" 
      ttDirection.iDeltaRow  = -1
      ttDirection.iDeltaCol  = 0
   .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = ">" 
      ttDirection.iDeltaRow  = 0
      ttDirection.iDeltaCol  = +1
   .

   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "v" 
      ttDirection.iDeltaRow  = +1
      ttDirection.iDeltaCol  = 0
   .

   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "<" 
      ttDirection.iDeltaRow  = 0
      ttDirection.iDeltaCol  = -1
   .

END PROCEDURE.
