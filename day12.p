
/*------------------------------------------------------------------------
    File        : day12.p
    Purpose     : Solve day 12 of Advent of Code 2024

    Syntax      :

    Description : Solution for day 12 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Thu Dec 12 22:42:13 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 12.
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
   FIELD iRow    AS INTEGER 
   FIELD iCol    AS INTEGER 
   FIELD cPlant  AS CHARACTER
   FIELD iFences AS INTEGER 
   FIELD iRegion AS INTEGER
   /* Additional Fields for Part Two */
   FIELD lNorth  AS LOGICAL
   FIELD lSouth  AS LOGICAL 
   FIELD lEast   AS LOGICAL 
   FIELD lWest   AS LOGICAL 
   FIELD iSide   AS INTEGER 
INDEX indRowCol IS UNIQUE iRow   iCol.
DEFINE BUFFER ttNextGrid FOR ttGrid.

DEFINE TEMP-TABLE ttPlant
   FIELD cPlant AS CHARACTER 
INDEX indPlant IS UNIQUE cPlant.

DEFINE TEMP-TABLE ttRegion
   FIELD cPlant  AS CHARACTER 
   FIELD iRegion AS INTEGER 
   FIELD iArea   AS INTEGER 
   FIELD iFences AS INTEGER 
   FIELD iPrice  AS INTEGER
   /* Add Field for Part Two */
   FIELD iSides         AS INTEGER 
   FIELD iDiscountPrice AS INTEGER 
INDEX indPlant IS UNIQUE cPlant iRegion.
   
DEFINE VARIABLE iRow     AS INTEGER NO-UNDO.
DEFINE VARIABLE iCol     AS INTEGER NO-UNDO.
DEFINE VARIABLE iRegion  AS INTEGER NO-UNDO.
DEFINE VARIABLE lNewGrid AS LOGICAL NO-UNDO.
DEFINE VARIABLE iPrevCol AS INTEGER NO-UNDO.
DEFINE VARIABLE iPrevRow AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttDirection
   FIELD cDirection AS CHARACTER 
   FIELD iDeltaRow  AS INTEGER 
   FIELD iDeltaCol  AS INTEGER 
INDEX indDirection IS UNIQUE cDirection.

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
   lcInput = "RRRRIICCFF~nRRRRIICCCF~nVVRRRCCFFF~nVVRCCCJFFF~nVVVVCJJCFE~nVVIVCCJJEE~nVVIIICJJEE~nMIIIIIJJEE~nMIIISIJEEE~nMMMISSJEEE".
   lcInput = "OOOOO~nOXOXO~nOOOOO~nOXOXO~nOOOOO".
   lcInput = "AAAA~nBBCD~nBBCC~nEEEC".
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
         ttGrid.cPlant = SUBSTRING (ttLine.cInput, iCol, 1)
      .
      FIND FIRST ttPlant
      WHERE ttPlant.cPlant EQ ttGrid.cPlant NO-ERROR.
      IF NOT AVAILABLE ttPlant THEN DO:
         CREATE ttPlant.
         ASSIGN
            ttPlant.cPlant = ttGrid.cPlant
         .
      END.
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
   FOR EACH ttPlant:
      RUN createPlantRegions
         (INPUT ttPlant.cPlant).
   END. /* For each Plant in the Grid */

   FOR EACH ttGrid,
   FIRST ttRegion
   WHERE ttRegion.cPlant  EQ ttGrid.cPlant
   AND   ttRegion.iRegion EQ ttGrid.iRegion:
      ASSIGN 
         ttGrid.iFences = 4
      .
      FOR EACH ttDirection,
      FIRST ttNextGrid
      WHERE ttNextGrid.iRow   EQ ttGrid.iRow + ttDirection.iDeltaRow
      AND   ttNextGrid.iCol   EQ ttGrid.iCol + ttDirection.iDeltaCol
      AND   ttNextGrid.cPlant EQ ttGrid.cPlant:
         /* Remove fences between same Plants */
         ttGrid.iFences = ttGrid.iFences - 1.
      END. /* Remove fences between same Plants */
      ASSIGN 
         ttRegion.iFences = ttRegion.iFences + ttGrid.iFences
      .
   END.
   
   FOR EACH ttRegion:
      ttRegion.iPrice = ttRegion.iArea * ttRegion.iFences.
      iSolution = iSolution + ttRegion.iPrice.
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
         (INPUT TEMP-TABLE ttGrid:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttRegion:HANDLE).
   END.


END. /* Process Part One */


IF lPart[2] THEN 
DO:
   /* Process Part Two */

   iSolution = 0.
   
   IF lPart[1] EQ FALSE THEN DO:
      FOR EACH ttPlant:
         RUN createPlantRegions
            (INPUT ttPlant.cPlant).
      END.
   END.
   
   FOR EACH ttGrid:
      /* Assign orientation of Fences (N, S, E and W) */
      ASSIGN 
         ttGrid.iFences = 4
         ttGrid.lNorth = TRUE 
         ttGrid.lSouth = TRUE 
         ttGrid.lEast  = TRUE 
         ttGrid.lWest  = TRUE 
      .
      FOR EACH ttDirection,
         FIRST ttNextGrid
         WHERE ttNextGrid.iRow   EQ ttGrid.iRow + ttDirection.iDeltaRow
         AND   ttNextGrid.iCol   EQ ttGrid.iCol + ttDirection.iDeltaCol
         AND   ttNextGrid.cPlant EQ ttGrid.cPlant:
         /* Remove fences between same Plants */
         CASE ttDirection.cDirection:
            WHEN "N" THEN ttGrid.lNorth = FALSE.
            WHEN "S" THEN ttGrid.lSouth = FALSE.
            WHEN "E" THEN ttGrid.lEast  = FALSE.
            WHEN "W" THEN ttGrid.lWest  = FALSE.
         END CASE.
         ttGrid.iFences = ttGrid.iFences - 1.
      END. /* Remove fences between same Plants */
   END. /* Assign orientation of Fences (N, S, E and W) */
   
   FOR EACH ttPlant,
   EACH  ttRegion
   WHERE ttRegion.cPlant EQ ttPlant.cPlant:
      
      FOR EACH ttGrid
      WHERE ttGrid.cPlant  EQ ttPlant.cPlant
      AND   ttGrid.iRegion EQ ttRegion.iRegion
      AND   ttGrid.lNorth  EQ TRUE
      BREAK
      BY ttGrid.iRow
      BY ttGrid.iCol:
         IF FIRST-OF(ttGrid.iRow) THEN DO:
            iPrevCol = 0.
         END.
         IF ttGrid.iCol NE iPrevCol + 1 OR iPrevCol EQ 0 THEN DO:
            ttGrid.iSide = ttGrid.iSide + 1.
            ttRegion.iSides = ttRegion.iSides + 1.
         END.         
         iPrevCol = ttGrid.iCol.
      END.
      FOR EACH ttGrid
      WHERE ttGrid.cPlant  EQ ttPlant.cPlant
      AND   ttGrid.iRegion EQ ttRegion.iRegion
      AND   ttGrid.lSouth  EQ TRUE
      BREAK
      BY ttGrid.iRow
      BY ttGrid.iCol:
         IF FIRST-OF(ttGrid.iRow) THEN 
         DO:
            iPrevCol = 0.
         END.
         IF ttGrid.iCol NE iPrevCol + 1 OR iPrevCol EQ 0 THEN 
         DO:
            ttGrid.iSide = ttGrid.iSide + 1.
            ttRegion.iSides = ttRegion.iSides + 1.
         END.         
         iPrevCol = ttGrid.iCol.
      END.
      FOR EACH ttGrid
      WHERE ttGrid.cPlant  EQ ttPlant.cPlant
      AND   ttGrid.iRegion EQ ttRegion.iRegion
      AND   ttGrid.lWest   EQ TRUE
      BREAK
      BY ttGrid.iCol
      BY ttGrid.iRow:
         IF FIRST-OF(ttGrid.iCol) THEN 
         DO:
            iPrevRow = 0.
         END.
         IF ttGrid.iRow NE iPrevRow + 1 OR iPrevRow EQ 0 THEN 
         DO:
            ttGrid.iSide = ttGrid.iSide + 1.
            ttRegion.iSides = ttRegion.iSides + 1.
         END.         
         iPrevRow = ttGrid.iRow.
      END.
      FOR EACH ttGrid
      WHERE ttGrid.cPlant  EQ ttPlant.cPlant
      AND   ttGrid.iRegion EQ ttRegion.iRegion
      AND   ttGrid.lEast   EQ TRUE
      BREAK
      BY ttGrid.iCol
      BY ttGrid.iRow:
         IF FIRST-OF(ttGrid.iCol) THEN 
         DO:
            iPrevRow = 0.
         END.
         IF ttGrid.iRow NE iPrevRow + 1 OR iPrevRow EQ 0 THEN 
         DO:
            ttGrid.iSide = ttGrid.iSide + 1.
            ttRegion.iSides = ttRegion.iSides + 1.
         END.         
         iPrevRow = ttGrid.iRow.
      END.
   END.

   FOR EACH ttRegion:
      ttRegion.iDiscountPrice = ttRegion.iArea * ttRegion.iSides.
      iSolution = iSolution + ttRegion.iDiscountPrice.
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
         (INPUT TEMP-TABLE ttGrid:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttRegion:HANDLE).         
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

PROCEDURE createPlantRegions:
/*------------------------------------------------------------------------------
 Purpose: Create Regions for a Plant
 Notes:   Regions consist of Adjacent Grids with the same Plant
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcPlant AS CHARACTER NO-UNDO.

DEFINE BUFFER ttPlant FOR ttPlant.

DEFINE VARIABLE iRegion AS INTEGER NO-UNDO.

   FIND  ttPlant 
   WHERE ttPlant.cPlant EQ ipcPlant. 
   /* For each Plant in the Grid */
   iRegion = 0.
   RegionBlock:
   REPEAT:
      /* Fill Region of Plant */
      FIND FIRST ttGrid
      WHERE ttGrid.cPlant  EQ ttPlant.cPlant
      AND   ttGrid.iRegion EQ 0 NO-ERROR.
      IF AVAILABLE ttGrid THEN 
      DO:
         /* Grid Available with this Plant without a Region */
         iRegion = iRegion + 1.
         ASSIGN 
            ttGrid.iRegion = iRegion
         .
         FIND FIRST ttRegion
            WHERE ttRegion.cPlant  EQ ttPlant.cPlant
            AND   ttRegion.iRegion EQ ttGrid.iRegion NO-ERROR.
         IF NOT AVAILABLE ttRegion THEN 
         DO:
            CREATE ttRegion.
            ASSIGN 
               ttRegion.cPlant  = ttPlant.cPlant
               ttRegion.iRegion = ttGrid.iRegion
               ttRegion.iArea   = 1
            .
         END.
         GridRegionBlock:
         REPEAT:
            /* Search all Grids of this Regions */
            lNewGrid = FALSE.
            FOR EACH ttGrid
            WHERE ttGrid.cPlant  EQ ttRegion.cPlant
            AND   ttGrid.iRegion EQ ttRegion.iRegion:
               /* Find Adjacent Grids with the same Plant to add to Region */
               FOR EACH ttDirection,
               FIRST ttNextGrid
               WHERE ttNextGrid.iRow    EQ ttGrid.iRow + ttDirection.iDeltaRow
               AND   ttNextGrid.iCol    EQ ttGrid.iCol + ttDirection.iDeltaCol
               AND   ttNextGrid.cPlant  EQ ttGrid.cPlant
               AND   ttNextGrid.iRegion EQ 0:
                  lNewGrid = TRUE.
                  ttNextGrid.iRegion = ttGrid.iRegion.
                  ttRegion.iArea = ttRegion.iArea + 1.
               END.
            END. /* Find Adjacent Grids with the same Plant to add to Region */   
            IF lNewGrid EQ FALSE THEN 
            DO:
               /* No more Adjacent Grids found */
               LEAVE GridRegionBlock.
            END.
         END. /* Search all Grids of this Regions */
      END. /* Grid Available with this Plant without a Region */
      ELSE 
      DO:
         LEAVE.
      END.
   END. /* Fill Region of Plant */      

END PROCEDURE.

PROCEDURE fillDirections:
   /*------------------------------------------------------------------------------
    Purpose: Fills all possible directions with deltaRow and deltaCol
    Notes:   ^ Up, v Down, > Right, < Left (no diagonals)
   ------------------------------------------------------------------------------*/
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "N" 
      ttDirection.iDeltaRow  = -1
      ttDirection.iDeltaCol  = 0
      .
   
   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "E" 
      ttDirection.iDeltaRow  = 0
      ttDirection.iDeltaCol  = +1
      .

   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "S" 
      ttDirection.iDeltaRow  = +1
      ttDirection.iDeltaCol  = 0
      .

   CREATE ttDirection.
   ASSIGN 
      ttDirection.cDirection = "W" 
      ttDirection.iDeltaRow  = 0
      ttDirection.iDeltaCol  = -1
      .

END PROCEDURE.
