
/*------------------------------------------------------------------------
    File        : day09.p
    Purpose     : Solve Day 9 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 9 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Sun Jan 05 21:26:57 CET 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* Variables for Problem */
DEFINE VARIABLE cURL       AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/&1/day/&2".
DEFINE VARIABLE cCommand   AS CHARACTER NO-UNDO.
/* Variables for input handling */
DEFINE VARIABLE lDownload  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSession   AS CHARACTER NO-UNDO INITIAL "53616c7465645f5f0aa2b48889c4ecb0a71e7086a3ce378be60c9c62fff2ce2f0a803b3cf401a90e48d12df95cfd2383f2923a50c7378e392a1b5d4ce4438c7e".
DEFINE VARIABLE iYear      AS INTEGER   NO-UNDO INITIAL 2024.
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 9.
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

DEFINE TEMP-TABLE ttPosition
   FIELD iPosition AS INT64 
   FIELD iIDFile   AS INT64 
   FIELD CheckSum  AS INT64
   /* Additional fields for Part Two */
   FIELD iLength   AS INTEGER  
INDEX indPos    IS UNIQUE iPosition
INDEX indSearch IS UNIQUE iIDFile iPosition.

DEFINE BUFFER ttFromPosition FOR ttPosition.
DEFINE BUFFER ttToPosition   FOR ttPosition.
DEFINE BUFFER ttMovePosition FOR ttPosition.
DEFINE BUFFER ttNextPosition FOR ttPosition.

DEFINE TEMP-TABLE ttFile
   FIELD iIDFile        AS INTEGER
   FIELD iStartPosition AS INTEGER 
   FIELD iLength        AS INTEGER 
INDEX indID IS UNIQUE iIDFile.

DEFINE VARIABLE iNewIDFile   AS INT64   NO-UNDO.
DEFINE VARIABLE iNewPosition AS INT64   NO-UNDO.
DEFINE VARIABLE iPosition    AS INT64   NO-UNDO.
DEFINE VARIABLE iLength      AS INTEGER NO-UNDO.

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
   lcInput = "2333133121414131402".
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

   ASSIGN 
      iNewIDFile   = -1
      iNewPosition = 0
   .
   DO iChar = 1 TO LENGTH (ttLine.cInputLine):
      iLength = INTEGER (SUBSTRING (ttLine.cInputLine, iChar, 1)).
      IF iChar MOD 2 EQ 1 THEN DO:
         /* Odd numbers, files */
         iNewIDFile = iNewIDFile + 1.
         CREATE ttFile.
         ASSIGN
            ttFile.iIDFile        = iNewIDFile
            ttFile.iLength        = iLength
            ttFile.iStartPosition = iNewPosition
         .
         DO iPosition = 1 TO iLength:
            CREATE ttPosition.
            ASSIGN 
               ttPosition.iPosition = ttFile.iStartPosition + iPosition - 1
               ttPosition.iIDFile   = ttFile.iIDFile
               ttPosition.CheckSum  = ttPosition.iIDFile * ttPosition.iPosition
               /* Additional Field for Part Two */
               ttPosition.iLength   = iLength - iPosition + 1
            .
         END.
         iNewPosition = iNewPosition + iLength.
      END.
      ELSE DO:
         DO iPosition = 1 TO iLength:
            CREATE ttPosition.
            ASSIGN 
               ttPosition.iPosition = iNewPosition + iPosition - 1
               ttPosition.iIDFile   = ?
               ttPosition.iLength   = iLength - iPosition + 1
            .
         END.
         iNewPosition = iNewPosition + iLength.
      END.
   END.
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttFile:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttPosition:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   FOR EACH ttFromPosition
   WHERE ttFromPosition.iIDFile NE ?
   BY ttFromPosition.iPosition DESCENDING:
      FIND FIRST ttToPosition
      WHERE ttToPosition.iIDFile   EQ ? 
      AND   ttToPosition.iPosition LT ttFromPosition.iPosition NO-ERROR.
      IF AVAILABLE ttToPosition THEN DO:
         ASSIGN 
            ttToPosition.iIDFile    = ttFromPosition.iIDFile
            ttFromPosition.iIDFile  = ?
            ttFromPosition.CheckSum = 0
            ttToPosition.CheckSum   = ttToPosition.iIDFile * ttToPosition.iPosition
         .
      END.
   END.   
  
   FOR EACH ttPosition
   WHERE ttPosition.iIDFile NE ?:
      iSolution = iSolution + ttPosition.CheckSum.
   END.
    
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 09 - Part One".
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPosition:HANDLE).
   END.      
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   FOR EACH ttFromPosition
   WHERE ttFromPosition.iIDFile NE ?,
   FIRST ttFile
   WHERE ttFile.iIDFile EQ ttFromPosition.iIDFile
   AND   ttFile.iLength EQ ttFromPosition.iLength
   BY ttFromPosition.iPosition DESCENDING:
      FIND FIRST ttToPosition
      WHERE ttToPosition.iIDFile   EQ ? 
      AND   ttToPosition.iPosition LT ttFromPosition.iPosition 
      AND   ttToPosition.iLength   GE ttFile.iLength NO-ERROR.
      IF AVAILABLE ttToPosition THEN DO:
         IF lvlDebug THEN DO:
            MESSAGE SUBSTITUTE ("Move File #&1 with length &2, from Position &3 to Position &4 (with length &5).",
                                ttFile.iIDFile,
                                ttFile.iLength,
                                ttFromPosition.iPosition,
                                ttToPosition.iPosition,
                                ttToPosition.iLength)
            VIEW-AS ALERT-BOX.
         END.
         FIND  ttMovePosition
         WHERE ROWID (ttMovePosition) EQ ROWID (ttToPosition).
         DO iPosition = 1 TO ttFile.iLength:
            ASSIGN 
               ttMovePosition.iIDFile  = ttFromPosition.iIDFile
               ttMovePosition.iLength  = ttFile.iLength - iPosition + 1
               ttMovePosition.CheckSum = ttMovePosition.iIDFile * ttMovePosition.iPosition
            .
            FIND  ttMovePosition
            WHERE ttMovePosition.iPosition EQ ttToPosition.iPosition + iPosition.
         END.
         FIND  ttNextPosition
         WHERE ttNextPosition.iPosition EQ ttFromPosition.iPosition + ttFile.iLength NO-ERROR.
         FIND  ttMovePosition
         WHERE ROWID (ttMovePosition) EQ ROWID (ttFromPosition).
         DO iPosition = 1 TO ttFile.iLength:
            ASSIGN 
               ttMovePosition.iIDFile  = ?
               ttMovePosition.iLength  = ttFile.iLength - iPosition
               ttMovePosition.CheckSum = 0
            .
            IF AVAILABLE ttNextPosition THEN 
               ttMovePosition.iLength = ttMovePosition.iLength + ttNextPosition.iLength.
            FIND  ttMovePosition
            WHERE ttMovePosition.iPosition EQ ttFromPosition.iPosition + iPosition NO-ERROR.               
         END.
      END.
   END.   
   
   iSolution = 0.
   FOR EACH ttPosition
      WHERE ttPosition.iIDFile NE ?:
      iSolution = iSolution + ttPosition.CheckSum.
   END.

      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 09 - Part Two".
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPosition:HANDLE).
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
