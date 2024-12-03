
/*------------------------------------------------------------------------
    File        : day03.p
    Purpose     : Solve Day 3 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 3 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Mon Dec 02 23:51:52 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 3.
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
DEFINE VARIABLE lvlEnabled AS LOGICAL   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine     AS INTEGER 
   FIELD cInputLine AS CHARACTER FORMAT "X(60)"

   INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttMul
   FIELD IDLine        AS INTEGER 
   FIELD StartPosition AS INTEGER 
   FIELD MulTerm       AS INTEGER EXTENT 2
   FIELD MulResult     AS INTEGER
INDEX indID IS UNIQUE IDLine StartPosition.

DEFINE VARIABLE iLineValue AS INTEGER NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION IsNumber RETURNS LOGICAL 
   (INPUT ipcString AS CHARACTER) FORWARD.

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
   lcInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))".
   lcInput = "mul(11,8)mul(8,5))".
   lcInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))".
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

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   iSolution = 0.
   
   FOR EACH ttLine:
      RUN parseLine
         (INPUT  ttLine.IDLine,
          INPUT  ttLine.cInputLine,
          INPUT  "", /* No Switches */
          OUTPUT iLineValue).
          
      iSolution = iSolution + iLineValue.    
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 03 - Part One".
END. /* Process Part One */

IF lPart[2] THEN 
DO:

   iSolution = 0.
   lvlEnabled = TRUE.
   FOR EACH ttLine:
      RUN parseLine
         (INPUT  ttLine.IDLine,
          INPUT  ttLine.cInputLine,
          INPUT  "do(),don't()", /* Switches */
          OUTPUT iLineValue).
      iSolution = iSolution + iLineValue.
   END.          
       
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 03 - Part Two".
END. /* Process Part Two */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttMul:HANDLE).
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

PROCEDURE parseLine:
/*------------------------------------------------------------------------------
 Purpose: Parses input line for mul(X,Y) instructions
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiIDLine    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcInputLine AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcSwitches  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiLineValue AS INTEGER   NO-UNDO.

DEFINE VARIABLE iChar          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSub           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStartPosition AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOperation     AS CHARACTER NO-UNDO INITIAL "mul".
DEFINE VARIABLE iTerm          AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE cStatus        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNumber        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNumber        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCheck         AS INTEGER   NO-UNDO.

/* Switch for Part 2 */
DEFINE VARIABLE iSwitch        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSwitch        AS CHARACTER NO-UNDO.

   IF lvlDebug THEN DO:
      MESSAGE SUBSTITUTE ("Start parsing line #&1: '&2'", ipiIDLine, ipcInputLine)
      VIEW-AS ALERT-BOX.
   END.
   
   DO iChar = 1 TO LENGTH (ipcInputLine):
      /* Parse single characters */
      cChar = SUBSTRING (ipcInputLine, iChar, 1).
      cSub  = SUBSTRING (ipcInputLine, iChar).
      DO iSwitch = 1 TO NUM-ENTRIES (ipcSwitches):
         /* For Part Two, handle Switches */
         cSwitch = ENTRY (iSwitch, ipcSwitches).
         IF cSub BEGINS cSwitch THEN DO:
            CASE cSwitch:
               WHEN "do()" THEN DO: 
                  lvlEnabled = TRUE.
               END.               
               WHEN "don't()" THEN DO: 
                  lvlEnabled = FALSE.   
               END.
            END.
            iChar = iChar + LENGTH (cSwitch) - 1.
         END.
      END.
                                       
      CASE cStatus:
         WHEN "" THEN DO:
            /* Looking for operation */
            IF cSub BEGINS cOperation THEN DO:
               cStatus = "operation".
               iStartPosition = iChar.
               iChar   = iChar + LENGTH (cOperation) - 1.
            END.
         END.
         WHEN "operation" THEN DO:
            IF cChar EQ "(" THEN DO:
               cStatus = "number".
               iNumber = 1.
            END.
            ELSE DO:
               cStatus = "".
               iChar = iChar - 1.
            END.
         END.
         WHEN "number" THEN DO:
            IF INDEX (cSub, ",") EQ 0 THEN DO: 
               cNumber = ENTRY (1, cSub, ")").
               cStatus = "close".
            END.
            ELSE DO:
               IF INDEX (cSub, ",") LT INDEX (cSub, ")") THEN
                  cNumber = ENTRY (1, cSub).
               ELSE DO:
                  cNumber = ENTRY (1, cSub, ")").
                  cStatus = "close".
               END.
            END.
            IF LENGTH (cNumber) GE 1 AND LENGTH (cNumber) LE 3 AND isNumber(cNumber) AND iNumber LE 2 THEN DO:
               FIND  ttMul
               WHERE ttMul.IDLine EQ ipiIDLine
               AND   ttMul.StartPosition EQ iStartPosition NO-ERROR.
               IF NOT AVAILABLE ttMul THEN DO:
                  CREATE ttMul.
                  ASSIGN 
                     ttMul.IDLine        = ipiIDLine
                     ttMul.StartPosition = iStartPosition
                  .
               END.
               ttMul.MulTerm[iNumber] = INTEGER (cNumber).
               iNumber = iNumber + 1.
               iChar   = iChar + LENGTH (cNumber).
               IF cStatus EQ "close" THEN 
                  iChar = iChar - 1.
            END.
            ELSE DO:
               FIND  ttMul
               WHERE ttMul.IDLine EQ ipiIDLine
               AND   ttMul.StartPosition EQ iStartPosition NO-ERROR.
               IF AVAILABLE ttMul THEN DO:
                  DELETE ttMul.
               END.
               cStatus = "".
               iChar = iChar - 1.
            END.
         END.
         WHEN "close" THEN DO:
            FIND  ttMul
            WHERE ttMul.IDLine EQ ipiIDLine
            AND   ttMul.StartPosition EQ iStartPosition NO-ERROR.
            IF AVAILABLE ttMul THEN DO:
               IF lvlEnabled EQ TRUE THEN 
                  ttMul.MulResult = ttMul.MulTerm[1] * ttMul.MulTerm[2].
               ELSE 
                  ttMul.MulResult = 0. 
               ASSIGN 
                  opiLineValue = opiLineValue + ttMul.MulResult
               .
               cStatus = "".
            END.
         END.
      END. /* CASE */
   END. /* Parse single characters */
   
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION IsNumber RETURNS LOGICAL 
(INPUT ipcString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Determines if a string is a number
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE cNumbers AS CHARACTER NO-UNDO INITIAL "0123456789".
DEFINE VARIABLE iChar    AS INTEGER NO-UNDO.
DEFINE VARIABLE cChar    AS CHARACTER NO-UNDO.

   DO iChar = 1 TO LENGTH (ipcString):
      cChar = SUBSTRING (ipcString, iChar, 1).
      IF INDEX (cNumbers, cChar) EQ 0 THEN 
         RETURN FALSE.
   END.
   
   RETURN TRUE.
      
END FUNCTION.
