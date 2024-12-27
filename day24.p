
/*------------------------------------------------------------------------
    File        : day24.p
    Purpose     : Solve Day 24 of Advent of Code 2024

    Syntax      :

    Description : Solution for Day 24 of Advent of Code 2024

    Author(s)   : Wim van der Ham
    Created     : Tue Dec 24 15:16:01 CET 2024
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
DEFINE VARIABLE iDay       AS INTEGER   NO-UNDO INITIAL 24.
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
DEFINE VARIABLE cSection   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBinary    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBinaryX   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBinaryY   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBit       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxBit    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBitResult AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRemainder AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBitX      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBitY      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBitZ      AS CHARACTER NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine     AS INTEGER 
   FIELD cInputLine AS CHARACTER FORMAT "X(80)"

   INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttWire
   FIELD Wire      AS CHARACTER 
   FIELD WireValue AS INTEGER
   /* Additional Fields for Part Two */
   FIELD Switched  AS LOGICAL  
INDEX indWire IS UNIQUE Wire.
DEFINE BUFFER ttWireIn1 FOR ttWire.
DEFINE BUFFER ttWireIn2 FOR ttWire.
DEFINE BUFFER ttWireOut FOR ttWire.
DEFINE BUFFER ttWireX   FOR ttWire.
DEFINE BUFFER ttWireY   FOR ttWire.
DEFINE BUFFER ttWireZ   FOR ttWire.

DEFINE TEMP-TABLE ttGate
   FIELD IDLine     AS INTEGER 
   FIELD WireIn1    AS CHARACTER 
   FIELD WireIn2    AS CHARACTER 
   FIELD Operator   AS CHARACTER 
   FIELD WireOut    AS CHARACTER
   /* Additional Fields for Part Two */
   FIELD SwitchWith AS CHARACTER 
INDEX indID IS UNIQUE IDLine.
DEFINE BUFFER ttGate1 FOR ttGate.
DEFINE BUFFER ttGate2 FOR ttGate.

DEFINE VARIABLE lCircuitCorrect AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSolution       AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getINT64 RETURNS INT64 
   (INPUT ipcBinary AS CHARACTER) FORWARD.

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

/* Test with Roel's input */
// cInputFile = "input\24.roel.txt".

ETIME (YES).
COPY-LOB FROM FILE cInputfile TO OBJECT lcInput.

IF lvlDebug THEN 
DO:
   lcInput = "x00: 1~nx01: 1~nx02: 1~ny00: 0~ny01: 1~ny02: 0~n~nx00 AND y00 -> z00~nx01 XOR y01 -> z01~nx02 OR y02 -> z02".
END.

cSection = "Wires".

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN DO:
      IF cSection = "Wires" THEN 
         cSection = "Gates".
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
      .

   IF cSection EQ "Wires" THEN DO:
      CREATE ttWire.
      ASSIGN 
         ttWire.Wire      = ENTRY (1, ttLine.cInputLine, ":")
         ttWire.WireValue = INTEGER (ENTRY (2, ttLine.cInputLine, " "))
      .
   END. 
   
   IF cSection EQ "Gates" THEN DO:
      CREATE ttGate.
      ASSIGN 
         ttGate.IDLine   = ttLine.IDLine
         ttGate.WireIn1  = ENTRY (1, ttLine.cInputLine, " ")
         ttGate.WireIn2  = ENTRY (3, ttLine.cInputLine, " ")
         ttGate.WireOut  = ENTRY (5, ttLine.cInputLine, " ")
         ttGate.Operator = ENTRY (2, ttLine.cInputLine, " ")
      .
      FIND ttWire 
      WHERE ttWire.Wire EQ ttGate.WireIn1 NO-ERROR.
      IF NOT AVAILABLE ttWire THEN DO:
         CREATE ttWire.
         ASSIGN 
            ttWire.Wire      = ttGate.WireIn1
            ttWire.WireValue = ?
         .
      END.
      FIND ttWire 
      WHERE ttWire.Wire EQ ttGate.WireIn2 NO-ERROR.
      IF NOT AVAILABLE ttWire THEN 
      DO:
         CREATE ttWire.
         ASSIGN 
            ttWire.Wire      = ttGate.WireIn2
            ttWire.WireValue = ?
         .
      END.
      FIND ttWire 
      WHERE ttWire.Wire EQ ttGate.WireOut NO-ERROR.
      IF NOT AVAILABLE ttWire THEN 
      DO:
         CREATE ttWire.
         ASSIGN 
            ttWire.Wire      = ttGate.WireOut
            ttWire.WireValue = ?
         .
      END.
      
   END.
   
END. /* ReadBlock: */

IF lvlShow THEN 
DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttWire:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGate:HANDLE).
END.

IF lPart[1] THEN 
DO:
   /* Process Part One */
   RUN solveCircuit.
      
   cBinary = "".
   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "z":
      cBinary = SUBSTITUTE ("&1&2", ttWire.WireValue, cBinary).
   END.
   iSolution = getINT64(INPUT cBinary).
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 24 - Part One".
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGate:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttWire:HANDLE).
   END.
         
END. /* Process Part One */

IF lPart[2] THEN 
DO:
   /* Process Part Two */
   FOR EACH ttWire
   WHERE (ttWire.Wire BEGINS "x") EQ FALSE 
   AND   (ttWire.Wire BEGINS "y") EQ FALSE:
      /* Reset Output Values */
      ttWire.WireValue = ?.
   END.   
   
   /* Switch Wrong Wires */
   RUN switchGate
      (INPUT "dwp",
       INPUT "kfm").

   RUN switchGate
      (INPUT "z22",
       INPUT "gjh").
           
   RUN switchGate
      (INPUT "z31",
       INPUT "jdr").
       
   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "x":
      /* Invert X bits */
      ttWire.WireValue = (IF ttWire.WireValue EQ 0 THEN 1 ELSE 0).
   END.       

   RUN switchGate
      (INPUT "z08",
       INPUT "ffj").
       
   FIND FIRST ttWire WHERE ttWire.WireValue EQ ? NO-ERROR.
   IF AVAILABLE ttWire THEN
      RUN solveCircuit.
      
   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "x":
      cBinaryX = SUBSTITUTE ("&1&2", ttWire.WireValue, cBinaryX).
   END.
   
   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "y":
      cBinaryY = SUBSTITUTE ("&1&2", ttWire.WireValue, cBinaryY).
   END.

   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "z":
      cBinary = SUBSTITUTE ("&1&2", ttWire.WireValue, cBinary).
   END.
   
   lCircuitCorrect = TRUE.
   
   OUTPUT TO "output\24.txt".
   PUT UNFORMATTED 
      " " cBinaryX " " getINT64(cBinaryX) SKIP 
      " " cBinaryY " " getINT64(cBinaryY) SKIP
      cBinary  " " getINT64(cBinary)  SKIP (1).
   
   FIND LAST ttWire WHERE ttWire.Wire BEGINS "z".
   iMaxBit = INTEGER (TRIM (ttWire.Wire, "z")).
   DO iBit = 0 TO iMaxBit:
      FIND ttWireX WHERE ttWireX.Wire EQ SUBSTITUTE ("x&1", STRING (iBit, "99")) NO-ERROR.
      FIND ttWireY WHERE ttWireY.Wire EQ SUBSTITUTE ("y&1", STRING (iBit, "99")) NO-ERROR.
      FIND ttWireZ WHERE ttWireZ.Wire EQ SUBSTITUTE ("z&1", STRING (iBit, "99")).
      
      IF  AVAILABLE ttWireX
      AND AVAILABLE ttWireY THEN DO:
         iBitResult = ttWireX.WireValue + ttWireY.WireValue + iRemainder.
         IF iBitResult GT 1 THEN
            ASSIGN  
               iRemainder = 1
               iBitResult = iBitResult - 2
            .
         ELSE 
            ASSIGN 
               iRemainder = 0
            .
         PUT UNFORMATTED 
            SUBSTITUTE ("&1: &2 (&3) + &4 (&5) = &6R&7 &8 (&9)", 
            STRING (iBit, "99"),
            ttWireX.Wire,
            ttWireX.WireValue,
            ttWireY.Wire,
            ttWireY.WireValue,
            iBitResult,
            iRemainder,
            ttWireZ.Wire,
            ttWireZ.WireValue).
      END.
      ELSE DO:
         iBitResult = iRemainder.
         iRemainder = 0.
         PUT UNFORMATTED 
            SUBSTITUTE ("&1: &2 (&3) + &4 (&5) = &6R&7 &8 (&9)", 
            STRING (iBit, "99"),
            "   ",
            " ",
            "   ",
            " ",
            iBitResult,
            iRemainder,
            ttWireZ.Wire,
            ttWireZ.WireValue).
      END.
      
      IF iBitResult NE ttWireZ.WireValue THEN DO:
         PUT UNFORMATTED "<--- NOT CORRECT".
         lCircuitCorrect = FALSE.
      END.
      PUT UNFORMATTED SKIP.                      
   END.
   PUT UNFORMATTED SKIP (1).
      
   /* Start with X */      
   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "x":
      FOR EACH ttGate
      WHERE ttGate.WireIn1 EQ ttWire.Wire OR ttGate.WireIn2 EQ ttWire.Wire:
         RUN outputGate
            (INPUT 0 /* Level */,
             INPUT ttGate.IDLine).
      END.          
      PUT UNFORMATTED SKIP (1).
   END.
       
/*   FOR EACH ttGate                                              */
/*   WHERE ttGate.WireIn1 BEGINS "x" OR ttGate.WireIn2 BEGINS "x",*/
/*   FIRST ttWireIn1 WHERE ttWireIn1.Wire EQ ttGate.WireIn1,      */
/*   FIRST ttWireIn2 WHERE ttWireIn2.Wire EQ ttGate.WireIn2,      */
/*   FIRST ttWireOut WHERE ttWireOut.Wire EQ ttGate.WireOut:      */
/*      ASSIGN                                                    */
/*         ttGate.iColumn = 1                                     */
/*         ttGate.iRow    = 1                                     */
/*      .                                                         */
/*      PUT UNFORMATTED                                           */
/*         SUBSTITUTE ("&1 (&2) &3 &4 (&5) --> &6 (&7)",          */
/*                     ttWireIn1.Wire,                            */
/*                     ttWireIn1.WireValue,                       */
/*                     ttGate.Operator,                           */
/*                     ttWireIn2.Wire,                            */
/*                     ttWireIn2.WireValue,                       */
/*                     ttWireOut.Wire,                            */
/*                     ttWireOut.WireValue) SKIP                  */
/*      .                                                         */
/*   END.                                                         */
   PUT UNFORMATTED SKIP (2).

   /* Continue with Y */      
   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "y":
      FOR EACH ttGate
      WHERE ttGate.WireIn1 EQ ttWire.Wire OR ttGate.WireIn2 EQ ttWire.Wire:
         RUN outputGate
            (INPUT 0 /* Level */,
             INPUT ttGate.IDLine).
      END.          
      PUT UNFORMATTED SKIP (1).          
   END.
      
/*   FOR EACH ttGate                                              */
/*   WHERE ttGate.WireIn1 BEGINS "y" OR ttGate.WireIn2 BEGINS "y",*/
/*   FIRST ttWireIn1 WHERE ttWireIn1.Wire EQ ttGate.WireIn1,      */
/*   FIRST ttWireIn2 WHERE ttWireIn2.Wire EQ ttGate.WireIn2,      */
/*   FIRST ttWireOut WHERE ttWireOut.Wire EQ ttGate.WireOut:      */
/*      ASSIGN                                                    */
/*         ttGate.iColumn = 1                                     */
/*         ttGate.iRow    = 2                                     */
/*      .                                                         */
/*      PUT UNFORMATTED                                           */
/*         SUBSTITUTE ("&1 (&2) &3 &4 (&5) --> &6 (&7)",          */
/*         ttWireIn1.Wire,                                        */
/*         ttWireIn1.WireValue,                                   */
/*         ttGate.Operator,                                       */
/*         ttWireIn2.Wire,                                        */
/*         ttWireIn2.WireValue,                                   */
/*         ttWireOut.Wire,                                        */
/*         ttWireOut.WireValue) SKIP                              */
/*      .                                                         */
/*   END.                                                         */
   PUT UNFORMATTED SKIP (2). 
   
   FOR EACH ttWire
   WHERE ttWire.Wire BEGINS "z":
      FOR EACH ttGate
      WHERE ttGate.WireOut EQ ttWire.Wire:
         RUN outputOutGate
            (INPUT 0 /* Level */,
             INPUT ttGate.IDLine).
      END.             
   END.          
          
/*   FOR EACH ttGate                                        */
/*   WHERE ttGate.WireOut BEGINS "z",                       */
/*   FIRST ttWireIn1 WHERE ttWireIn1.Wire EQ ttGate.WireIn1,*/
/*   FIRST ttWireIn2 WHERE ttWireIn2.Wire EQ ttGate.WireIn2,*/
/*   FIRST ttWireOut WHERE ttWireOut.Wire EQ ttGate.WireOut:*/
/*      ASSIGN                                              */
/*         ttGate.iColumn = 1                               */
/*         ttGate.iRow    = 26                              */
/*      .                                                   */
/*      PUT UNFORMATTED                                     */
/*         SUBSTITUTE ("&1 (&2) &3 &4 (&5) --> &6 (&7)",    */
/*         ttWireIn1.Wire,                                  */
/*         ttWireIn1.WireValue,                             */
/*         ttGate.Operator,                                 */
/*         ttWireIn2.Wire,                                  */
/*         ttWireIn2.WireValue,                             */
/*         ttWireOut.Wire,                                  */
/*         ttWireOut.WireValue) SKIP                        */
/*      .                                                   */
/*   END.                                                   */
   OUTPUT CLOSE.
   
   RUN sy\win\show-file.w
      (INPUT "output\24.txt").  
      
   IF lCircuitCorrect EQ TRUE THEN DO:
      FOR EACH ttWire
      WHERE ttWire.Switched:
         cSolution = SUBSTITUTE ("&1&2&3",
                                 cSolution,
                                 (IF cSolution NE "" THEN "," ELSE ""),
                                 ttWire.Wire).
      END.
                                             
      OUTPUT TO "clipboard".
      PUT UNFORMATTED cSolution SKIP.
      OUTPUT CLOSE.
   
      MESSAGE 
         SUBSTITUTE ("Solution: &1.", 
         cSolution) SKIP (1)
         SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
         VIEW-AS ALERT-BOX TITLE " 2024 - Day 24 - Part Two".
   END.
   ELSE DO:
      MESSAGE "No solution found"
      VIEW-AS ALERT-BOX TITLE " 2024 - Day 24 - Part Two".
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


PROCEDURE outputGate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiLevel  AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiIDLine AS INTEGER NO-UNDO.

DEFINE BUFFER ttGate    FOR ttGate.
DEFINE BUFFER ttWireIn1 FOR ttWire.
DEFINE BUFFER ttWireIn2 FOR ttWire.
DEFINE BUFFER ttWireOut FOR ttWire.

DEFINE VARIABLE cFill AS CHARACTER NO-UNDO.

   FIND  ttGate 
   WHERE ttGate.IDLine EQ ipiIDLine.
   FIND  ttWireIn1 
   WHERE ttWireIn1.Wire EQ ttGate.WireIn1.
   FIND  ttWireIn2
   WHERE ttWireIn2.Wire EQ ttGate.WireIn2.
   FIND  ttWireOut 
   WHERE ttWireOut.Wire EQ ttGate.WireOut.
   
   IF ipiLevel GT 0 THEN
      cFill = FILL (" ", 3 * ipiLevel).

   PUT UNFORMATTED 
      SUBSTITUTE ("&1 &2 (&3)", cFill, ttGate.WireOut, ttWireOut.WireValue)   SKIP 
      SUBSTITUTE ("&1 &2", cFill, ttGate.Operator)                            SKIP 
      SUBSTITUTE ("&1   &2 (&3)", cFill, ttWireIn1.Wire, ttWireIn1.WireValue) SKIP.

   FIND  ttGate
   WHERE ttGate.WireOut EQ ttWireIn1.Wire NO-ERROR.
   IF AVAILABLE ttGate THEN DO:
      RUN outputGate
         (INPUT ipiLevel + 1,
          INPUT ttGate.IDLine).
   END.       
   
   PUT UNFORMATTED 
      SUBSTITUTE ("&1   &2 (&3)", cFill, ttWireIn2.Wire, ttWireIn2.WireValue) SKIP.

   FIND  ttGate
   WHERE ttGate.WireOut EQ ttWireIn2.Wire NO-ERROR.
   IF AVAILABLE ttGate THEN 
   DO:
      RUN outputGate
         (INPUT ipiLevel + 1,
          INPUT ttGate.IDLine).
   END.       

/*   PUT UNFORMATTED                                 */
/*      SUBSTITUTE ("&1 (&2) &3 &4 (&5) --> &6 (&7)",*/
/*      ttWireIn1.Wire,                              */
/*      ttWireIn1.WireValue,                         */
/*      ttGate.Operator,                             */
/*      ttWireIn2.Wire,                              */
/*      ttWireIn2.WireValue,                         */
/*      ttWireOut.Wire,                              */
/*      ttWireOut.WireValue) SKIP                    */
/*   .                                               */
   
/*   FIND  ttGate                                                                        */
/*   WHERE ttGate.WireIn1 EQ ttWireOut.Wire OR ttGate.WireIn2 EQ ttWireOut.Wire NO-ERROR.*/
/*   IF AVAILABLE ttGate THEN DO:                                                        */
/*      RUN outputGate                                                                   */
/*         (INPUT ipiLevel + 1,                                                          */
/*          INPUT ttGate.IDLine).                                                        */
/*   END.                                                                                */

END PROCEDURE.

PROCEDURE outputOutGate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiLevel  AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiIDLine AS INTEGER NO-UNDO.

DEFINE BUFFER ttGate     FOR ttGate.
DEFINE BUFFER ttWireIn1  FOR ttWire.
DEFINE BUFFER ttWireIn2  FOR ttWire.
DEFINE BUFFER ttWireOut  FOR ttWire.
DEFINE BUFFER ttNextGate FOR ttGate.

DEFINE VARIABLE cFill AS CHARACTER NO-UNDO.

   IF ipiLevel GT 0 THEN
      cFill = FILL (" ", 3 * ipiLevel).
      
   IF ipiLevel GT 5 THEN DO:
      PUT UNFORMATTED SUBSTITUTE ("&1&2", cFill, ".....") SKIP. 
      RETURN.
   END.
      
   FIND  ttGate 
   WHERE ttGate.IDLine EQ ipiIDLine.
   FIND  ttWireIn1 
   WHERE ttWireIn1.Wire EQ ttGate.WireIn1.
   FIND  ttWireIn2
   WHERE ttWireIn2.Wire EQ ttGate.WireIn2.
   FIND  ttWireOut 
   WHERE ttWireOut.Wire EQ ttGate.WireOut.

   PUT UNFORMATTED 
      SUBSTITUTE ("&1&2 (&3) = &4 (&5) &6 &7 (&8)", 
                  cFill, 
                  ttGate.WireOut, 
                  ttWireOut.WireValue,
                  ttWireIn1.Wire, 
                  ttWireIn1.WireValue,
                  ttGate.Operator,
                  ttWireIn2.Wire, 
                  ttWireIn2.WireValue) SKIP.

   FIND  ttNextGate
   WHERE ttNextGate.WireOut EQ ttWireIn1.Wire NO-ERROR.
   IF AVAILABLE ttNextGate THEN
      RUN outputOutGate
         (INPUT ipiLevel + 1,
          INPUT ttNextGate.IDLine).

   FIND  ttNextGate
   WHERE ttNextGate.WireOut EQ ttWireIn2.Wire NO-ERROR.
   IF AVAILABLE ttNextGate THEN
      RUN outputOutGate
         (INPUT ipiLevel + 1,
          INPUT ttNextGate.IDLine).
          
/*   PUT UNFORMATTED                                 */
/*      SUBSTITUTE ("&1 (&2) &3 &4 (&5) --> &6 (&7)",*/
/*      ttWireIn1.Wire,                              */
/*      ttWireIn1.WireValue,                         */
/*      ttGate.Operator,                             */
/*      ttWireIn2.Wire,                              */
/*      ttWireIn2.WireValue,                         */
/*      ttWireOut.Wire,                              */
/*      ttWireOut.WireValue) SKIP                    */
/*   .                                               */

END PROCEDURE.

PROCEDURE solveCircuit:
/*------------------------------------------------------------------------------
 Purpose: Solve the Circuit by giving a value to all wires
 Notes:
------------------------------------------------------------------------------*/
   CircuitBlock:
   REPEAT:
      /* While we have Wire with unknow Value, repeat */
      FOR EACH ttGate,
      FIRST ttWireIn1
      WHERE ttWireIn1.Wire EQ ttGate.WireIn1
      AND   ttWireIn1.WireValue NE ?,
      FIRST ttWireIn2
      WHERE ttWireIn2.Wire EQ ttGate.WireIn2
      AND   ttWireIn2.WireValue NE ?,
      FIRST ttWireOut
      WHERE ttWireOut.Wire EQ ttGate.WireOut:
         CASE ttGate.Operator:
            WHEN "AND" THEN 
            DO:
               IF  ttWireIn1.WireValue EQ 1
                  AND ttWireIn2.WireValue EQ 1 THEN 
                  ttWireOut.WireValue = 1.
               ELSE 
                  ttWireOut.WireValue = 0.
            END.
            WHEN "OR" THEN 
            DO:
               IF  ttWireIn1.WireValue EQ 0
                  AND ttWireIn2.WireValue EQ 0 THEN
                  ttWireOut.WireValue = 0.
               ELSE 
                  ttWireOut.WireValue = 1.
            END.
            WHEN "XOR" THEN 
            DO:
               IF ttWireIn1.WireValue NE ttWireIn2.WireValue THEN 
                  ttWireOut.WireValue = 1.
               ELSE 
                  ttWireOut.WireValue = 0.
            END.
         END CASE.
      END.
      FIND FIRST ttWire
      WHERE ttWire.WireValue EQ ? NO-ERROR.
      IF NOT AVAILABLE ttWire THEN 
         LEAVE CircuitBlock.
   END. /* While we have Wire with unknow Value, repeat CircuitBlock */

END PROCEDURE.

PROCEDURE switchGate:
/*------------------------------------------------------------------------------
 Purpose: Switches the Output of two Gates
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcFromGate AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcToGate   AS CHARACTER NO-UNDO.

DEFINE BUFFER ttGate1 FOR ttGate.
DEFINE BUFFER ttGate2 FOR ttGate.
DEFINE BUFFER ttWire1 FOR ttWire.
DEFINE BUFFER ttWire2 FOR ttWire.

   FIND ttGate1 WHERE ttGate1.WireOut EQ ipcFromGate.
   FIND ttWire1 WHERE ttWire1.Wire    EQ ttGate1.WireOut.
   FIND ttGate2 WHERE ttGate2.WireOut EQ ipcToGate.
   FIND ttWire2 WHERE ttWire2.Wire    EQ ttGate2.WireOut.
   
   ASSIGN 
      ttGate1.WireOut    = ipcToGate
      ttGate1.SwitchWith = ttGate2.WireOut
      ttWire1.Switched   = TRUE 
      ttGate2.WireOut    = ipcFromGate
      ttGate2.SwitchWith = ttGate1.WireOut
      ttWire2.Switched   = TRUE 
   .    

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION getINT64 RETURNS INT64 
(INPUT ipcBinary AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns an INT64 from a Binary String
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iBit   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBit   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBase  AS INT64     NO-UNDO.
DEFINE VARIABLE iINT64 AS INT64     NO-UNDO.

   iBase = 1.
   DO iBit = LENGTH (ipcBinary) TO 1 BY -1:
      cBit = SUBSTRING (ipcBinary, iBit, 1).
      IF cBit EQ "1" THEN 
         iINT64 = iINT64 + iBase.
      iBase = iBase * 2.
   END.
                  
   RETURN iINT64.
      
END FUNCTION.
