       Identification division.
       program-id. Program1 as "CargoFileUsingSearch.Program1".

       Environment division.
       Input-Output section.
       File-control.    Select shipData assign to 'E:\COBOL Stuff\CargoFileUsingSearch\CargoFileUsingSearch\CargosProducts.txt'
                           organization is line sequential. 
                        Select Outputfile assign to 'E:\COBOL Stuff\CargoFileUsingSearch\CargoFileUsingSearch\FinalReport.txt'
                        organization is line sequential.

       Data division.

       File section.
       Fd shipData.
       01 shipRecord.
         05 ShipName picture x(20).
         05 ShipProduct picture x(10).
         05 prodUnits picture 9(5).
         05 Country-In picture x(14).

       Fd Outputfile.
       01 Print-Rec.
         05 picture x(8).
         05 ShipName-Out picture x(20).
         05 picture x(8).
         05 ShipProduct-Out picture x(10).
         05 picture x(8).
         05 prodUnits-Out picture ZZZZ9.
         05 picture x(8).
         05 TotalPrice picture ZZZ99.99.
         05 picture x(7).
         05 Country-Out picture x(14).

       Working-storage section.

       01 Are-There-More-Records picture xxx value 'Yes'.
       01 VoidLine picture x(100) value spaces.

       01 HEADER.
         05 picture x(30) value spaces.
         05 picture x(26) value 'SHIPS REPORT - MARCHANDISE'.
         05 picture x(15) value spaces.
         05 Date-Out.
           10 day1 picture 99.
           10 line1 picture x value '/'.
           10 month picture 99.
           10 line2 picture x value '/'.
           10 year picture 9999.
         05 picture x(28) value spaces.

       01 Sub-Header.
         05 picture x(12) value spaces.
         05 picture x(9) value 'SHIP NAME'.
         05 picture x(15) value spaces.
         05 picture x(5) value 'CARGO'.
         05 picture x(13) value spaces.
         05 picture x(5) value 'UNITS'.
         05 picture x(6) value spaces.
         05 picture x(11) value 'TOTAL VALUE'.
         05 picture x(6) value spaces.
         05 picture x(7) value 'COUNTRY'.

       01 WS-CURRENT-DATE-DATA.
         05 WS-CURRENT-DATE.
           10 WS-CURRENT-YEAR picture 9(4).
           10 WS-CURRENT-MONTH picture 9(2).
           10 WS-CURRENT-DAY picture 9(2).



       01 DataContainer picture x(112) value 'BUTANE    0040COPPER    0075IRON ORE  1050OIL       2123RUBBER    1080SUGAR     0815TIMBER    0046WHEAT     0240'.
      * DONE :D


      *Here redefines let you say : DataContainer will be putted inside ProductTable, so you call Datacontainer when using ProductTable
       01 ProductTable redefines DataContainer.
         05 arrayTable occurs 8 times indexed by A.
           10 WS-prodName picture x(10).
           10 WS-prodCost picture 99v99.


       01 FinalPriceContainer picture 99999v99 value 0.

       Procedure division.
       100-Main-Module.
           open input shipData
             output OutputFile

           Move FUNCTION CURRENT-DATE (1:8) to WS-CURRENT-DATE-DATA
           perform 200-Printing-Header


           perform until Are-There-More-Records = 'No'
               read shipData
                   at end
                       move 'No' to Are-There-More-Records
                   not at end
                       perform 300-Making-Math
                       perform 400-Output-Record
               end-read
           end-perform

           close shipData
             OutputFile

           stop run.

       200-Printing-Header.

           Move spaces to Print-Rec
           Move WS-CURRENT-DAY to day1
           Move WS-CURRENT-MONTh to month
           Move WS-CURRENT-YEAR to year

           Move HEADER to Print-Rec
           write Print-Rec
             after advancing 5 lines

           move sub-Header to Print-Rec
           write Print-Rec
             after advancing 3 lines
           move VoidLine to Print-Rec
           write Print-Rec.

       300-Making-Math.
           Set A to 1

           SEARCH arrayTable
               at end
                   display 'NOT FOUND'
               WHEN WS-prodName(A) = ShipProduct
                   Compute FinalPriceContainer = prodUnits * WS-prodCost(A)
                   move FinalPriceContainer to TotalPrice.

       400-Output-Record.
           Move shipName to shipName-Out
           Move shipProduct to ShipProduct-Out
           Move prodUnits to prodUnits-Out
           Move Country-In to Country-Out
           write Print-Rec.

       end program Program1.
