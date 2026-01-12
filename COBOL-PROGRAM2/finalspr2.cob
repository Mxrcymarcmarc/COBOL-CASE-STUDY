       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM2.
       AUTHOR. GROUP4.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GSYSTEM-OUT ASSIGN TO "GSYS-OUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  GSYSTEM-OUT
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS PRINT-REC.
       01  PRINT-REC PIC X(250).

       WORKING-STORAGE SECTION.
       01  HEADER1.
           02 FILLER PIC X(50) VALUE SPACES.
           02 FILLER PIC X(22) VALUE "POLYTECHNIC UNIVERSITY".
           02 FILLER PIC X(19) VALUE " OF THE PHILIPPINES".
       01  HEADER2.
           02 FILLER PIC X(49) VALUE SPACES.
           02 FILLER PIC X(20) VALUE "COLLEGE OF COMPUTER".
           02 FILLER PIC X(25) VALUE "AND INFORMATION SCIENCE".
       01  HEADER3.
           02 FILLER PIC X(61) VALUE SPACES.
           02 FILLER PIC X(25) VALUE "STUDENT GRADING SYSTEM".

       01  C-HEADERS-ROW1.
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "YEAR LEVEL".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "NO. OF".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "PRELIM".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "MIDTERM".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "FINAL".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "AVERAGE".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(10) VALUE "PASSED".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(10) VALUE "FAILED".

       01  C-HEADERS-ROW2.
           02 FILLER PIC X(25)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "STUDENTS".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "GRADE".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "GRADE".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "GRADE".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(15) VALUE "GRADE".
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(10) VALUE SPACES.
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FILLER PIC X(10) VALUE SPACES.

       01  DISPLAY-DETAILS.
           02 FILLER             PIC X(5) VALUE SPACES.
           02 YEAR-LEVEL-OUT     PIC X(15).
           02 FILLER             PIC X(8) VALUE SPACES.
           02 NO-OF-STUDENTS-OUT PIC ZZ9.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 PRELIM-GRADE-OUT   PIC ZZ9.99.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 MIDTERM-GRADE-OUT  PIC ZZ9.99.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 FINAL-GRADE-OUT    PIC ZZ9.99.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 AVERAGE-GRADE-OUT  PIC ZZ9.99.
           02 FILLER             PIC X(17) VALUE SPACES.
           02 PASSED-OUT         PIC ZZ9.
           02 FILLER             PIC X(12) VALUE SPACES.
           02 FAILED-OUT         PIC ZZ9.

       01  TOTAL-LINE.
           02 FILLER           PIC X(5)  VALUE SPACES.
           02 FILLER           PIC X(15) VALUE "TOTAL".
           02 FILLER           PIC X(8) VALUE SPACES.
           02 TOT-STUDENTS     PIC ZZ9.
           02 FILLER           PIC X(97) VALUE SPACES.
           02 TOT-PASSED       PIC ZZ9.
           02 FILLER           PIC X(12) VALUE SPACES.
           02 TOT-FAILED       PIC ZZ9.

       01  YEAR-NAME-TABLE.
           02 YEAR-NAME-VALUE OCCURS 4 PIC X(15).

       01  YEAR-LEVEL-TABLE.
           02 YEAR-ENTRY OCCURS 4.
              03 YEAR-NAME    PIC X(15).
              03 STUDENTS     PIC 9(3)      VALUE 0.
              03 SUM-PRELIM   PIC 9(6)V99   VALUE 0.
              03 SUM-MIDTERM  PIC 9(6)V99   VALUE 0.
              03 SUM-FINAL    PIC 9(6)V99   VALUE 0.
              03 SUM-AVERAGE  PIC 9(6)V99   VALUE 0.
              03 PASS-COUNT   PIC 9(3)      VALUE 0.
              03 FAIL-COUNT   PIC 9(3)      VALUE 0.

       01  WS-GRADE-FIELDS.
           02 WS-GRADE-FIELDS-PRELIM   PIC 999V99  VALUE 0.
           02 WS-GRADE-FIELDS-MIDTERM  PIC 999V99  VALUE 0.
           02 WS-GRADE-FIELDS-FINAL    PIC 999V99  VALUE 0.
           02 WS-GRADE-FIELDS-AVG      PIC 999V99  VALUE 0.
           02 WS-GRADE-FIELDS-TEMP     PIC 999V99  VALUE 0.

       01  WS-GRADE-INPUT-STRING.
           02 WS-GRADE-INPUT-STR       PIC X(10)   VALUE SPACES.

       01  WS-GRADE-AVERAGES.
           02 WS-PRELIM-AVG       PIC 999V99  VALUE 0.
           02 WS-MIDTERM-AVG      PIC 999V99  VALUE 0.
           02 WS-FINAL-AVG        PIC 999V99  VALUE 0.
           02 WS-AVERAGE-AVG      PIC 999V99  VALUE 0.

       01  WS-COUNTERS.
           02 WS-STUDENT-COUNT    PIC 9(3)    VALUE 0.
           02 WS-STUDENT-IDX      PIC 9(3)    VALUE 0.
           02 GRAND-STUDENTS      PIC 9(5)    VALUE 0.
           02 GRAND-PASSED        PIC 9(5)    VALUE 0.
           02 GRAND-FAILED        PIC 9(5)    VALUE 0.
           02 Y-SUB               PIC 9       VALUE 1.

       01  SCREEN-POSITION.
           02 SCREEN-LINE         PIC 99      VALUE 1.
           02 SCREEN-COLUMN       PIC 99      VALUE 5.

       SCREEN SECTION.
       01  BLANK-SCREEN-DEF.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
           PERFORM INIT-YEAR-NAMES.
           PERFORM COLLECT-DATA.
           OPEN OUTPUT GSYSTEM-OUT.
           PERFORM WRITE-HEADERS.
           PERFORM WRITE-DETAIL-LINES.
           PERFORM WRITE-TOTAL-LINE.
           CLOSE GSYSTEM-OUT.
           STOP RUN.

       INIT-YEAR-NAMES.
           MOVE "Freshmen       " TO YEAR-NAME-VALUE(1)
           MOVE "Sophomore      " TO YEAR-NAME-VALUE(2)
           MOVE "Junior         " TO YEAR-NAME-VALUE(3)
           MOVE "Senior         " TO YEAR-NAME-VALUE(4)
           PERFORM VARYING Y-SUB FROM 1 BY 1 UNTIL Y-SUB > 4
               MOVE YEAR-NAME-VALUE(Y-SUB) TO YEAR-NAME(Y-SUB)
           END-PERFORM.

       COLLECT-DATA.
           DISPLAY BLANK-SCREEN-DEF.
           MOVE 3 TO SCREEN-LINE.
           
           DISPLAY "STUDENT GRADING SYSTEM" 
               LINE SCREEN-LINE COLUMN 15.
           
           PERFORM VARYING Y-SUB FROM 1 BY 1 UNTIL Y-SUB > 4
               DISPLAY BLANK-SCREEN-DEF
               MOVE 2 TO SCREEN-LINE
               
         DISPLAY "===================================================="
                   LINE SCREEN-LINE COLUMN SCREEN-COLUMN
               ADD 1 TO SCREEN-LINE
               
               DISPLAY YEAR-NAME-VALUE(Y-SUB)
                   LINE SCREEN-LINE COLUMN 10
               ADD 1 TO SCREEN-LINE
               
         DISPLAY "===================================================="
                   LINE SCREEN-LINE COLUMN SCREEN-COLUMN
               ADD 1 TO SCREEN-LINE
               ADD 1 TO SCREEN-LINE
               
               DISPLAY "Number of students: "
                   LINE SCREEN-LINE COLUMN SCREEN-COLUMN
               ACCEPT WS-STUDENT-COUNT 
                   LINE SCREEN-LINE COLUMN 30
               
               MOVE WS-STUDENT-COUNT TO STUDENTS(Y-SUB)
               MOVE 0 TO SUM-PRELIM(Y-SUB) SUM-MIDTERM(Y-SUB)
                         SUM-FINAL(Y-SUB) SUM-AVERAGE(Y-SUB)
                         PASS-COUNT(Y-SUB) FAIL-COUNT(Y-SUB)
               
               IF WS-STUDENT-COUNT > 0
                   PERFORM VARYING WS-STUDENT-IDX FROM 1 BY 1
                           UNTIL WS-STUDENT-IDX > WS-STUDENT-COUNT
                       ADD 2 TO SCREEN-LINE
                       
                       DISPLAY "Student #" WS-STUDENT-IDX
                           LINE SCREEN-LINE COLUMN SCREEN-COLUMN
                       ADD 1 TO SCREEN-LINE
                       
                       DISPLAY "-----"
                           LINE SCREEN-LINE COLUMN SCREEN-COLUMN
                       ADD 1 TO SCREEN-LINE
                       
                       DISPLAY "Prelim grade: "
                           LINE SCREEN-LINE COLUMN SCREEN-COLUMN
                       ACCEPT WS-GRADE-INPUT-STR
                           LINE SCREEN-LINE COLUMN 25
                       MOVE FUNCTION NUMVAL(WS-GRADE-INPUT-STR)
                           TO WS-GRADE-FIELDS-PRELIM
                       ADD 1 TO SCREEN-LINE
                       
                       DISPLAY "Midterm grade: "
                           LINE SCREEN-LINE COLUMN SCREEN-COLUMN
                       ACCEPT WS-GRADE-INPUT-STR
                           LINE SCREEN-LINE COLUMN 25
                       MOVE FUNCTION NUMVAL(WS-GRADE-INPUT-STR)
                           TO WS-GRADE-FIELDS-MIDTERM
                       ADD 1 TO SCREEN-LINE
                       
                       DISPLAY "Final grade: "
                           LINE SCREEN-LINE COLUMN SCREEN-COLUMN
                       ACCEPT WS-GRADE-INPUT-STR
                           LINE SCREEN-LINE COLUMN 25
                       MOVE FUNCTION NUMVAL(WS-GRADE-INPUT-STR)
                           TO WS-GRADE-FIELDS-FINAL
                       
                       COMPUTE WS-GRADE-FIELDS-AVG ROUNDED =
                               (WS-GRADE-FIELDS-PRELIM + 
                                WS-GRADE-FIELDS-MIDTERM + 
                                WS-GRADE-FIELDS-FINAL) / 3
                       ADD WS-GRADE-FIELDS-PRELIM TO SUM-PRELIM(Y-SUB)
                       ADD WS-GRADE-FIELDS-MIDTERM TO SUM-MIDTERM(Y-SUB)
                       ADD WS-GRADE-FIELDS-FINAL TO SUM-FINAL(Y-SUB)
                       ADD WS-GRADE-FIELDS-AVG TO SUM-AVERAGE(Y-SUB)
                       IF WS-GRADE-FIELDS-AVG >= 75
                           ADD 1 TO PASS-COUNT(Y-SUB)
                       ELSE
                           ADD 1 TO FAIL-COUNT(Y-SUB)
                       END-IF
                   END-PERFORM
               END-IF
               ADD WS-STUDENT-COUNT TO GRAND-STUDENTS
               ADD PASS-COUNT(Y-SUB) TO GRAND-PASSED
               ADD FAIL-COUNT(Y-SUB) TO GRAND-FAILED
           END-PERFORM.
           
           DISPLAY BLANK-SCREEN-DEF.
           MOVE 5 TO SCREEN-LINE.
           DISPLAY "Data Collection Complete!"
               LINE SCREEN-LINE COLUMN 10.
           ADD 1 TO SCREEN-LINE.
           DISPLAY "Generating Report..."
               LINE SCREEN-LINE COLUMN 10.

       WRITE-HEADERS.
           WRITE PRINT-REC FROM HEADER1.
           WRITE PRINT-REC FROM HEADER2.
           WRITE PRINT-REC FROM HEADER3.
           MOVE SPACES TO PRINT-REC.
           WRITE PRINT-REC.
           WRITE PRINT-REC FROM C-HEADERS-ROW1 AFTER 1 LINE.
           WRITE PRINT-REC FROM C-HEADERS-ROW2 AFTER 1 LINE.
           MOVE ALL "-" TO PRINT-REC(6:150)
           WRITE PRINT-REC AFTER 1 LINE.

       WRITE-DETAIL-LINES.
           PERFORM VARYING Y-SUB FROM 1 BY 1 UNTIL Y-SUB > 4
               IF STUDENTS(Y-SUB) > 0
                   COMPUTE WS-PRELIM-AVG ROUNDED =
                           SUM-PRELIM(Y-SUB) / STUDENTS(Y-SUB)
                   COMPUTE WS-MIDTERM-AVG ROUNDED =
                           SUM-MIDTERM(Y-SUB) / STUDENTS(Y-SUB)
                   COMPUTE WS-FINAL-AVG ROUNDED =
                           SUM-FINAL(Y-SUB) / STUDENTS(Y-SUB)
                   COMPUTE WS-AVERAGE-AVG ROUNDED =
                           SUM-AVERAGE(Y-SUB) / STUDENTS(Y-SUB)
               ELSE
                   MOVE 0 TO WS-PRELIM-AVG WS-MIDTERM-AVG
                            WS-FINAL-AVG WS-AVERAGE-AVG
               END-IF
               PERFORM PREPARE-DETAIL-OUTPUT
               WRITE PRINT-REC FROM DISPLAY-DETAILS AFTER 1 LINE
           END-PERFORM.

       PREPARE-DETAIL-OUTPUT.
           MOVE YEAR-NAME(Y-SUB)     TO YEAR-LEVEL-OUT.
           MOVE STUDENTS(Y-SUB)      TO NO-OF-STUDENTS-OUT.
           MOVE WS-PRELIM-AVG        TO PRELIM-GRADE-OUT.
           MOVE WS-MIDTERM-AVG       TO MIDTERM-GRADE-OUT.
           MOVE WS-FINAL-AVG         TO FINAL-GRADE-OUT.
           MOVE WS-AVERAGE-AVG       TO AVERAGE-GRADE-OUT.
           MOVE PASS-COUNT(Y-SUB)    TO PASSED-OUT.
           MOVE FAIL-COUNT(Y-SUB)    TO FAILED-OUT.

       WRITE-TOTAL-LINE.
           MOVE GRAND-STUDENTS TO TOT-STUDENTS
           MOVE GRAND-PASSED   TO TOT-PASSED
           MOVE GRAND-FAILED   TO TOT-FAILED
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 2 LINES.
