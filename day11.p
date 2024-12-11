
/*------------------------------------------------------------------------
    File        : day11.p
    Purpose     : Solve Day 11 Advent of Code 2024

    Syntax      :

    Description : Solution of Day 11 Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Wed Dec 11 16:30:57 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 11.
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

DEFINE TEMP-TABLE ttStone
   FIELD iBlink AS INTEGER 
   FIELD iOrder AS INTEGER 
   FIELD iValue AS INT64   FORMAT "zzz,zzz,zzz,zzz,zzz,zz9"
INDEX indBlink IS UNIQUE PRIMARY iBlink iOrder.
DEFINE BUFFER ttNewStone FOR ttStone.

DEFINE VARIABLE iStone  AS INTEGER NO-UNDO.
DEFINE VARIABLE iBlink  AS INTEGER NO-UNDO.
DEFINE VARIABLE iOrder  AS INTEGER NO-UNDO.
DEFINE VARIABLE iLength AS INTEGER NO-UNDO.
   
/* Part Two */
DEFINE TEMP-TABLE ttVisited
   FIELD iValue  AS INT64   FORMAT "zzz,zzz,zzz,zzz,zzz,zz9"
   FIELD iBlinks AS INTEGER 
   FIELD iLength AS INT64   FORMAT "zzz,zzz,zzz,zzz,zzz,zz9"
INDEX indValue IS UNIQUE iValue iBlinks.

    

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getLength RETURNS INT64 
   (INPUT ipiValue AS INT64, INPUT ipiBlinks AS INTEGER) FORWARD.

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
   lcInput = "125 17".
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

   iBlink = 0.
   DO iStone = 1 TO NUM-ENTRIES (ttLine.cInputLine, " "):
      CREATE ttStone.
      ASSIGN 
         ttStone.iBlink = iBlink
         ttStone.iOrder = iStone
         ttStone.iValue = INT64(ENTRY (iStone, ttLine.cInputLine, " "))
      .
   END.
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */

   DO iBlink = 1 TO 25:
      /* Blink 1 - 25 */
      iOrder = 0.
      FOR EACH ttStone
      WHERE ttStone.iBlink EQ iBlink - 1:
         /* Rules
         - If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
         - If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. 
           The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. 
           (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
         - If none of the other rules apply, the stone is replaced by a new stone; 
           the old stone's number multiplied by 2024 is engraved on the new stone.
         */
         IF ttStone.iValue EQ 0 THEN DO:
            /* Stone 0 --> Stone 1 */
            iOrder = iOrder + 1.
            CREATE ttNewStone.
            ASSIGN 
               ttNewStone.iBlink = iBlink
               ttNewStone.iOrder = iOrder
               ttNewStone.iValue = 1
            .
         END.
         ELSE DO:
            iLength = LENGTH (STRING (ttStone.iValue)). 
            IF iLength MOD 2 EQ 0 THEN DO:
               /* Even number of Digits, Split */
               iOrder = iOrder + 1.
               CREATE ttNewStone.
               ASSIGN 
                  ttNewStone.iBlink = iBlink
                  ttNewStone.iOrder = iOrder
                  ttNewStone.iValue = INT64 (SUBSTRING (STRING (ttStone.iValue), 1, INTEGER (iLength / 2)))
               .
               iOrder = iOrder + 1.
               CREATE ttNewStone.
               ASSIGN 
                  ttNewStone.iBlink = iBlink
                  ttNewStone.iOrder = iOrder
                  ttNewStone.iValue = INT64 (SUBSTRING (STRING (ttStone.iValue), INTEGER (iLength / 2) + 1))
               .             
            END. /* Even number of Digits, Split */
            ELSE DO:
               /* New Stone is multiplied by 2024 */
               iOrder = iOrder + 1.
               CREATE ttNewStone.
               ASSIGN 
                  ttNewStone.iBlink = iBlink
                  ttNewStone.iOrder = iOrder
                  ttNewStone.iValue = 2024 * ttStone.iValue
               .
            END. /* New Stone is multiplied by 2024 */
         END.
      END. /* For each stone */
      
      IF lvlDebug THEN DO:
         MESSAGE "After Blink" iBlink
         VIEW-AS ALERT-BOX.
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttStone:HANDLE).
      END.
      
   END. /* Blink 1 - 25 */
   
   FOR EACH ttStone
   WHERE ttStone.iBlink EQ 25:
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
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 11 - Part One".
      
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttStone:HANDLE).
   END.
   
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   
   iSolution = 0.
   FOR EACH ttStone:
      iSolution = iSolution + getLength(ttStone.iValue, 75).
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 11 - Part Two".
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttVisited:HANDLE).
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


/* ************************  Function Implementations ***************** */

FUNCTION getLength RETURNS INT64 
(INPUT ipiValue  AS INT64,
 INPUT ipiBlinks AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Returns the number of numbers generated by an input value and number of blinks
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttVisited FOR ttVisited.

DEFINE VARIABLE iLength  AS INTEGER NO-UNDO.
DEFINE VARIABLE iNumbers AS INT64   NO-UNDO.
DEFINE VARIABLE iLeft    AS INT64   NO-UNDO.
DEFINE VARIABLE iRight   AS INT64   NO-UNDO.

   IF ipiBlinks EQ 0 THEN 
      RETURN 1.
   ELSE DO:
      FIND  ttVisited 
      WHERE ttVisited.iValue  EQ ipiValue
      AND   ttVisited.iBlinks EQ ipiBlinks NO-ERROR.
      IF AVAILABLE ttVisited THEN 
         RETURN ttVisited.iLength.
      ELSE DO:
         /* Now handle the transformations */
         /* Rules
            - If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
            - If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. 
              The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. 
              (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
            - If none of the other rules apply, the stone is replaced by a new stone; 
              the old stone's number multiplied by 2024 is engraved on the new stone.
         */
         IF ipiValue EQ 0 THEN DO:
            iNumbers = getLength(1, ipiBlinks - 1).
         END.
         ELSE DO:
            iLength = LENGTH (STRING (ipiValue)). 
            IF iLength MOD 2 EQ 0 THEN 
            DO:
               ASSIGN 
                  iLeft  = INT64 (SUBSTRING (STRING (ipiValue), 1, INTEGER (iLength / 2)))
                  iRight = INT64 (SUBSTRING (STRING (ipiValue), INTEGER (iLength / 2) + 1))
               .
               iNumbers = getLength(iLeft, ipiBlinks - 1) + getLength(iRight, ipiBlinks - 1).
            END. /* Even number of Digits, Split */
            ELSE 
            DO:
               /* New Stone is multiplied by 2024 */
               iNumbers = getLength(2024 * ipiValue, ipiBlinks - 1).
            END. /* New Stone is multiplied by 2024 */
         END.
      END.
      
   END.

   CREATE ttVisited.
   ASSIGN 
      ttVisited.iValue  = ipiValue
      ttVisited.iBlinks = ipiBlinks
      ttVisited.iLength = iNumbers
   .
      
   RETURN iNumbers.

END FUNCTION.
