       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program1 as "HomeworkTransaction.Program1".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.   SELECT CLIENTSINFO   ASSIGN TO "E:\COBOL Stuff\HomeworkTransaction\HomeworkTransaction\Clients_info.txt"
                       organization is line sequential.
                       SELECT FINALREPORT   ASSIGN TO "E:\COBOL Stuff\HomeworkTransaction\HomeworkTransaction\outputClients.txt"
                       organization is line sequential.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTSINFO.
       01 CLIENT_INFO.
         05 NAME-CLIENT.
            10 INITIAL1 PICTURE X.
            10 INITIAL2 PICTURE X.
            10 LASTNAME PICTURE X(10).
         05 DATE_INPUT PICTURE X(6).
         05 AMOUNT PICTURE 999999.

       FD FINALREPORT.
       01 OUTPUTREPORT.
         05 NAME-OUT.
           10 INITIAL1-OUT PICTURE X.
           10 PERIOD1 PICTURE X VALUE SPACES.
           10 INITIAL2-OUT PICTURE X.
           10 PERIOD2 PICTURE X VALUE SPACES.
           10 LASTNAME-OUT PICTURE X(10).
         05 PICTURE X(6) VALUE SPACES.
         05 DATE_PLACE PICTURE X(7).
         05 PICTURE X(11) VALUE SPACES.
         05 AMOUNT-OUT PICTURE $ZZZ,ZZ9.
         05 PICTURE X(11) VALUE SPACES.

       WORKING-STORAGE SECTION.
       01 WS_DATE PICTURE 99/9999.
       
       01 HEADER.
         05 PICTURE X(5) VALUE SPACES.
         05 NAME PICTURE X(4) VALUE 'NAME'.
         05 PICTURE X(5) VALUE SPACES.
         05 DATEINFO PICTURE X(19) VALUE 'DATE OF TRANSACTION'.
         05 PICTURE XXX VALUE SPACES.
         05 AMTRANS PICTURE X(21) VALUE 'AMOUNT OF TRANSACTION'.
         05 PICTURE XX VALUE SPACES.

       01 ARE-THERE-MORE-RECORDS  PICTURE XXX VALUE 'YES'.

       01 INSERTLINE PICTURE x(80).

       procedure division.
       100-MAIN-MODULE.
            OPEN INPUT CLIENTSINFO
                 OUTPUT FINALREPORT
            MOVE INSERTLINE TO OUTPUTREPORT
            WRITE OUTPUTREPORT
            BEFORE ADVANCING 1 LINE
            MOVE HEADER TO OUTPUTREPORT
            WRITE OUTPUTREPORT
             BEFORE ADVANCING 2 LINES
          PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
             READ CLIENTSINFO
               AT END
                   MOVE 'NO' TO ARE-THERE-MORE-RECORDS
               NOT AT END
                  PERFORM 200-REST-OF-PROCEDURE
           END-READ
        END-PERFORM
           CLOSE CLIENTSINFO
                 FINALREPORT
           STOP RUN.
       200-REST-OF-PROCEDURE.
           MOVE SPACES TO OUTPUTREPORT
           MOVE INITIAL1 TO INITIAL1-OUT
           MOVE  '.' TO PERIOD1
           MOVE INITIAL2 TO INITIAL2-OUT
           MOVE '.' TO PERIOD2
           MOVE LASTNAME TO LASTNAME-OUT
           MOVE DATE_INPUT TO WS_DATE
           MOVE WS_DATE TO DATE_PLACE
           MOVE AMOUNT TO AMOUNT-OUT.
           WRITE OUTPUTREPORT.
       end program Program1.
        