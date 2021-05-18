       program-id.
       Program1 as "SalesPersonsProblems.Program1".

       Environment division.
       Input-Output section.
                       
       File-Control.   Select SalesPersonsList assign to 'E:\COBOL Stuff\SalesPersonsProblems\SalesPersonsProblems\input7.txt'
                       organization is line sequential.
                       Select OutputSalesPersons assign to 'E:\COBOL Stuff\SalesPersonsProblems\SalesPersonsProblems\OutputFile.txt'
                       organization is line sequential.

       Data division.
       file section.
       Fd SalesPersonsList.
       01 SalesRecord.
         05 PersonId picture 99.
         05 PersonName picture x(20).
         05 amt-of-sales picture 999v99.

       Fd OutputSalesPersons.
       01 Print-Rec.
         05 SalesPersonRecord.
           10 picture x(15) value spaces.
           10 PersonID-Out picture 99.
           10 picture x(12) value spaces.
           10 PersonName-Out picture x(20).
           10 picture x(5) value spaces.
           10 amt-of-sales-out picture $ZZ,ZZZ.99.
           10 picture x(34).

      *x(16) value spaces 
           
       Working-storage section.

       01 Are-There-More-Records picture xxx value 'YES'.

       01 HEADER.
         05 picture x(31) value spaces.
         05 picture x(32) value 'TOTAL SALES FOR EACH SALESPERSON'.
         05 picture x(4) value spaces.
         05 Date-Out.
           10 day1 picture 99.
           10 line1 picture x value '/'.
           10 month picture 99.
           10 line2 picture x value '/'.
           10 year picture 9999.
         05 picture xxx value spaces.

       01 VoidLine picture x(80) value spaces.

       01 ListingHeader.
         05 picture x(10) value spaces.
         05 picture x(15) value 'SALESPERSON NO.'.
         05 picture xxxx value spaces.
         05 picture x(16) value 'SALESPERSON NAME'.
         05 picture x(9) value spaces.
         05 picture x(11) value 'TOTAL SALES'.
         05 picture x(15) value spaces.

       01 WS-CURRENT-DATE-DATA.
         05 WS-CURRENT-DATE.
           10 WS-CURRENT-YEAR picture 9(4).
           10 WS-CURRENT-MONTH picture 9(2).
           10 WS-CURRENT-DAY picture 9(2).

       01 FinalResultOut.
         05 picture x(40) value spaces.
         05 picture x(19) value 'TOTAL COMPANY SALES'.
         05 picture xxx value spaces.
         05 Total picture $$,$$$,$$$.99.
         05 picture x value space.
         05 picture x value '*'.

       01 ID-List occurs 20 times picture 99.

      * Loop variables
       01 w picture 99 value 1.
       01 y picture 99 value 1.
       01 u picture 99 value 1.

       01 SalaryHolder picture 99999v99 occurs 20 times.
       01 EmpNameHolder picture x(20) occurs 20 times.
       01 amt-holder picture 99999999v99.

       Procedure division.
       100-Main-Module.
           Open input SalesPersonsList
             output OutputSalesPersons
          perform 200-Print-File

          perform varying w
           from 1
           by 1 
           until w > 20
               compute ID-List(w) = w
          end-perform

           perform varying u
             from 1
             by 1
             until u > 20
               compute SalaryHolder(u) = 0
           end-perform

           perform until Are-There-More-Records = 'NO'
                   read SalesPersonsList
                       at end 
                       move 'NO'  to Are-There-More-Records
                       not at end
                       perform 300-Perf-Math
                   end-read
           end-perform
           perform 400-Print-Record
           perform 500-Total-Result
           close SalesPersonsList
                   OutputSalesPersons
       stop run.
       200-Print-File.
           Move FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE-DATA
           Move WS-CURRENT-YEAR to year
           Move WS-CURRENT-MONTH to month
           Move WS-CURRENT-DAY to day1
           move '/' to line1
           move '/' to line2
           move HEADER to Print-Rec
           write Print-Rec
             after advancing 5 lines
           move ListingHeader to Print-Rec
           write Print-Rec
             after advancing 2 lines
           move VoidLine to Print-Rec
           write Print-Rec
           after advancing 1 line.

       300-Perf-Math.
           move spaces to Print-Rec
           move PersonName to EmpNameHolder(PersonId)
           compute SalaryHolder(PersonId) = SalaryHolder(PersonId) + amt-of-sales.
           
       400-Print-Record.
           move spaces to Print-Rec
           perform varying y
             from 1
             by 1
             until y > 20
               move ID-List(y) to PersonID-Out
               move EmpNameHolder(y) to PersonName-Out
               move SalaryHolder(y) to amt-of-sales-out
               compute amt-holder = amt-holder + SalaryHolder(y)

               write Print-Rec
                 after advancing 1 lines
           end-perform
           move amt-holder to Total.
       500-Total-Result.
           move FinalResultOut to Print-Rec
           write Print-Rec
             after advancing 4 lines.

       end program Program1.
