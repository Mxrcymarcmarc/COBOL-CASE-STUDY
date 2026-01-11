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
           02 FILLER             PIC X(17) VALUE SPACES.
           02 NO-OF-STUDENTS-OUT PIC ZZ9.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 PRELIM-GRADE-OUT   PIC ZZ9.99.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 MIDTERM-GRADE-OUT  PIC ZZ9.99.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 FINAL-GRADE-OUT    PIC ZZ9.99.
           02 FILLER             PIC X(14) VALUE SPACES.
           02 AVERAGE-GRADE-OUT  PIC ZZ9.99.
           02 FILLER             PIC X(12) VALUE SPACES.
           02 PASSED-OUT         PIC 999.
           02 FILLER             PIC X(12) VALUE SPACES.
           02 FAILED-OUT         PIC 999.

       01  TOTAL-LINE.
           02 FILLER           PIC X(5)  VALUE SPACES.
           02 FILLER           PIC X(15) VALUE "TOTAL".
           02 FILLER           PIC X(17) VALUE SPACES.
           02 TOT-STUDENTS     PIC ZZ9.
           02 FILLER           PIC X(92) VALUE SPACES.
           02 TOT-PASSED       PIC 999.
           02 FILLER           PIC X(12) VALUE SPACES.
           02 TOT-FAILED       PIC 999.

       01  INPUT-DETAILS.
           02 FILLER PIC X(5) VALUE SPACES.
           02 YEAR-LEVEL-IN      PIC X(15).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 NO-OF-STUDENTS-IN  PIC 9(3).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 PRELIM-GRADE-IN    PIC 999.99.
           02 FILLER PIC X(5)  VALUE SPACES.
           02 MIDTERM-GRADE-IN   PIC 999.99.
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FINAL-GRADE-IN     PIC 999.99.
           02 FILLER PIC X(5)  VALUE SPACES.
           02 AVERAGE-GRADE-IN   PIC 999.99.
           02 FILLER PIC X(5)  VALUE SPACES.
           02 PASSED-IN          PIC 9(3).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 FAILED-IN          PIC 9(3).

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

       77  WS-STUDENT-COUNT    PIC 9(3)    VALUE 0.
       77  WS-STUDENT-IDX      PIC 9(3)    VALUE 0.
       77  WS-PRELIM           PIC 999V99  VALUE 0.
       77  WS-MIDTERM          PIC 999V99  VALUE 0.
       77  WS-FINAL            PIC 999V99  VALUE 0.
       77  WS-AVG              PIC 999V99  VALUE 0.
       77  WS-PRELIM-AVG       PIC 999V99  VALUE 0.
       77  WS-MIDTERM-AVG      PIC 999V99  VALUE 0.
       77  WS-FINAL-AVG        PIC 999V99  VALUE 0.
       77  WS-AVERAGE-AVG      PIC 999V99  VALUE 0.
       77  GRAND-STUDENTS      PIC 9(5)    VALUE 0.
       77  GRAND-PASSED        PIC 9(5)    VALUE 0.
       77  GRAND-FAILED        PIC 9(5)    VALUE 0.
       77  Y-SUB               PIC 9       VALUE 1.

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
           PERFORM VARYING Y-SUB FROM 1 BY 1 UNTIL Y-SUB > 4
               DISPLAY SPACES
               DISPLAY "=================================="
               DISPLAY "  " YEAR-NAME-VALUE(Y-SUB)
               DISPLAY "=================================="
               DISPLAY "Number of students: "
                   WITH NO ADVANCING
               ACCEPT WS-STUDENT-COUNT
               MOVE WS-STUDENT-COUNT TO STUDENTS(Y-SUB)
               MOVE 0 TO SUM-PRELIM(Y-SUB) SUM-MIDTERM(Y-SUB)
                         SUM-FINAL(Y-SUB) SUM-AVERAGE(Y-SUB)
                         PASS-COUNT(Y-SUB) FAIL-COUNT(Y-SUB)
               IF WS-STUDENT-COUNT > 0
                   PERFORM VARYING WS-STUDENT-IDX FROM 1 BY 1
                           UNTIL WS-STUDENT-IDX > WS-STUDENT-COUNT
                       DISPLAY SPACES
                       DISPLAY "  Student #" WS-STUDENT-IDX
                       DISPLAY "  -----------"
                       DISPLAY "    Prelim grade:  "
                           WITH NO ADVANCING
                       ACCEPT WS-PRELIM
                       DISPLAY "    Midterm grade: "
                           WITH NO ADVANCING
                       ACCEPT WS-MIDTERM
                       DISPLAY "    Final grade:   "
                           WITH NO ADVANCING
                       ACCEPT WS-FINAL
                       COMPUTE WS-AVG ROUNDED =
                               (WS-PRELIM + WS-MIDTERM + WS-FINAL) / 3
                       ADD WS-PRELIM TO SUM-PRELIM(Y-SUB)
                       ADD WS-MIDTERM TO SUM-MIDTERM(Y-SUB)
                       ADD WS-FINAL TO SUM-FINAL(Y-SUB)
                       ADD WS-AVG TO SUM-AVERAGE(Y-SUB)
                       IF WS-AVG >= 75
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
               MOVE YEAR-NAME(Y-SUB)     TO YEAR-LEVEL-OUT
               MOVE STUDENTS(Y-SUB)      TO NO-OF-STUDENTS-OUT
               MOVE WS-PRELIM-AVG        TO PRELIM-GRADE-OUT
               MOVE WS-MIDTERM-AVG       TO MIDTERM-GRADE-OUT
               MOVE WS-FINAL-AVG         TO FINAL-GRADE-OUT
               MOVE WS-AVERAGE-AVG       TO AVERAGE-GRADE-OUT
               MOVE PASS-COUNT(Y-SUB)    TO PASSED-OUT
               MOVE FAIL-COUNT(Y-SUB)    TO FAILED-OUT
               WRITE PRINT-REC FROM DISPLAY-DETAILS AFTER 1 LINE
           END-PERFORM.

       WRITE-TOTAL-LINE.
           MOVE GRAND-STUDENTS TO TOT-STUDENTS
           MOVE GRAND-PASSED   TO TOT-PASSED
           MOVE GRAND-FAILED   TO TOT-FAILED
           WRITE PRINT-REC FROM TOTAL-LINE AFTER 1 LINE.
