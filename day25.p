
/*------------------------------------------------------------------------
    File        : day25.p
    Purpose     : Solve Day 25 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 25 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Wed Dec 25 11:50:44 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 25.
DEFINE VARIABLE hPLIP      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInput    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
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

DEFINE TEMP-TABLE ttKeyLock
   FIELD IDLine  AS INTEGER 
   FIELD Lock    AS LOGICAL FORMAT "Lock/Key"
   FIELD iHeight AS INTEGER EXTENT 5
INDEX indID IS UNIQUE IDLine.
DEFINE BUFFER ttKey  FOR ttKeyLock.
DEFINE BUFFER ttLock FOR ttKeyLock.

DEFINE VARIABLE cSection AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLastID  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iColumn  AS INTEGER   NO-UNDO EXTENT 5.

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
   lcInput = "".
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN DO:
      cSection = "". 
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
      .

   IF cSection EQ "" THEN DO:
      IF ttLine.cInputLine EQ FILL ("#", 5) THEN
         cSection = "Lock".
      ELSE
         cSection = "Key".
      CREATE ttKeyLock.
      ASSIGN 
         ttKeyLock.IDLine = ttLine.IDLine
         ttKeyLock.Lock   = cSection EQ "Lock"
      .
      iLastID = ttKeyLock.IDLine.
      iColumn = 0.
      NEXT ReadBlock.
   END.
   
   /*    Key       Lock
       12345      12345   
    1  .....   1  #####
    2  ..#..   2  .###.
    3  ..#..   3  ..##.
    4  ..#.#   4  ..#..
    5  .##.#   5  ..#..
    6  .##.#   6  ..#..
    7  #####   7  .....
   */
   IF cSection NE "" THEN DO:
      IF ttLine.IDLine LE iLastID + 5 THEN DO: 
         DO iChar = 1 TO LENGTH (ttLine.cInputLine):
            cChar = SUBSTRING (ttLine.cInputLine, iChar,  1).
            IF cChar EQ "#" THEN 
               iColumn[iChar] = iColumn[iChar] + 1.
         END.
      END.
      ELSE DO:
         FIND ttKeyLock WHERE ttKeyLock.IDLine EQ iLastID.
         ASSIGN 
            ttKeyLock.iHeight = iColumn
         .
      END.
   END.
                  
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttKeyLock:HANDLE).      
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttKey
   WHERE ttKey.Lock EQ FALSE:
      FOR EACH ttLock
      WHERE ttLock.Lock EQ TRUE:
         IF  ttLock.iHeight[1] + ttKey.iHeight[1] LE 5
         AND ttLock.iHeight[2] + ttKey.iHeight[2] LE 5
         AND ttLock.iHeight[3] + ttKey.iHeight[3] LE 5
         AND ttLock.iHeight[4] + ttKey.iHeight[4] LE 5
         AND ttLock.iHeight[5] + ttKey.iHeight[5] LE 5 THEN 
            iSolution = iSolution + 1.
      END.
   END.  
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 25 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   
   iSolution = 0.

      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 25 - Part Two".
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
