       IDENTIFICATION DIVISION.
       program-id. Program1 as "EmployeeSalary.Program1".
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.  SELECT inputFile assign to "E:\COBOL Stuff\EmployeeSalary\InputEmployees.txt"
                                  organization is line sequential.
                      SELECT outputFile assign to "E:\COBOL Stuff\EmployeeSalary\OutputFile.txt"
                                      organization is line sequential.
       DATA DIVISION.
       FILE SECTION.
       FD inputFile.
       01 EmployeeRecord.
         05 EmpNbIn picture XXXXX.
         05 EmpName picture X(20).
         05 picture X(4) value spaces.
         05 AnnualSalaryIn picture 9(6).
         05 picture x(13) value spaces.
         05 EmpDues picture 999v99.
         05 EmpInsuIn picture 999v99.
         05 picture x(22) value spaces.
       FD outputFile.
       01 Print-Rec.
         05 picture X(3).
         05 EmpNbOut picture X(5).
         05 picture XX.
         05 EmpNameOut picture X(20).
         05 picture xx.
         05 OldSalary picture ZZZZZ9.
         05 picture x.
         05 NewSalary picture ZZZZZZ9.
         05 picture XX.
         05 OldDues picture ZZZ.99.
         05 picture x.
         05 NewDues picture ZZZZ.99.
         05 picture xx.
         05 OldInsu picture ZZZ.99.
         05 picture x.
         05 NewInsu picture ZZZZ.99.
       WORKING-STORAGE SECTION.

       01 VoidLine picture X(80) value spaces.

       01 WS-CURRENT-DATE-DATA.
         05 WS-CURRENT-DATE.
           10 WS-CURRENT-YEAR PIC 9(04).
           10 WS-CURRENT-MONTH PIC 9(02).
           10 WS-CURRENT-DAY PIC 9(02).

       01 HEADER.
         05 picture x(21) value spaces.
         05 headerTitle picture x(25) value 'PAYROLL REPORT'.
         05 picture x(21) value spaces.
         05 Ws_Date.
           10 day1 picture xx.
           10 picture x value '/'.
           10 month picture xx.
           10 picture x value '/'.
           10 year picture xxxx.
         05 picture x value spaces.

       01 miniTitles1.
         05 picture x value spaces.
         05 emp picture x(8) value 'EMPLOYEE'.
         05 picture x(9) value spaces.
         05 nam picture XXXX value 'NAME'.
         05 picture x(11) value spaces.
         05 old picture x(3) value 'OLD'.
         05 picture x(5) value spaces.
         05 ne picture x(3) value 'NEW'.
         05 picture x(5) value spaces.
         05 old picture x(3) value 'OLD'.
         05 picture x(5) value spaces.
         05 ne picture x(3) value 'NEW'.
         05 picture x(5) value spaces.
         05 old picture x(3) value 'OLD'.
         05 picture x(5) value spaces.
         05 ne picture x(3) value 'NEW'.
         05 picture xxxx value spaces.

       01 miniTitles2.
         05 picture xxxx value spaces.
         05 picture x(3) value 'NO.'.
         05 picture x(25).
         05 picture x(6) value 'SALARY'.
         05 picture x value spaces.
         05 picture x(6) value 'SALARY'.
         05 picture xxx value spaces.
         05 picture xxxx value 'DUES'.
         05 picture xxxx value spaces.
         05 picture xxxx value 'DUES'.
         05 picture xxx value spaces.
         05 picture x(6) value 'INSUR.'.
         05 picture xx value spaces.
         05 picture x(6) value 'INSUR.'.

       01 ARE-THERE-MORE-RECORDS picture XXX value 'YES'.

       procedure division.
       100-MAIN-MODULE.
           open input inputFile
             output outputFile
           move VoidLine to Print-Rec
           write Print-Rec
             after advancing 5 lines
           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE-DATA.
           move WS-CURRENT-DAY to day1
           move WS-CURRENT-MONTH to month
           move WS-CURRENT-YEAR to year
           move HEADER to Print-Rec
           write Print-Rec
           move miniTitles1 to Print-Rec
           write Print-Rec
           after advancing 2 lines
           move miniTitles2 to Print-Rec
           write Print-Rec
           move VoidLine to Print-Rec
           write Print-Rec
           perform until ARE-THERE-MORE-RECORDS = 'NO'
               read inputFile
                   at end
                       move 'NO' to ARE-THERE-MORE-RECORDS
                   not at end
                       perform 200-REST-OF-PROCESS
               end-read
           end-perform
           close inputFile
             outputFile
           stop run.
       200-REST-OF-PROCESS.
           move spaces to Print-Rec
           move EmpNbIn to EmpNbOut
           move EmpName to EmpNameOut
           move AnnualSalaryIn to OldSalary
           COMPUTE NewSalary rounded = AnnualSalaryIn * .07 + AnnualSalaryIn
           move EmpDues to OldDues
           COMPUTE NewDues rounded = EmpDues * .04 + EmpDues
           move EmpInsuIn to OldInsu
           COMPUTE NewInsu rounded = EmpInsuIn * .03 + EmpInsuIn
           write Print-Rec
           
       end program Program1.
       