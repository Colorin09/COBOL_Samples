       Identification division.
       Program-id.
       Program1 as "InflationRateResults.Program1".
       
       Environment division.
       Input-Output section.
       File-control.     Select ItemList assign to 'E:\COBOL Stuff\InflationRateResults\InflationRateResults\ItemList.txt'
			                organization is line sequential.
		                 Select OutputData assign to 'E:\COBOL Stuff\InflationRateResults\InflationRateResults\OutputList.txt'                            
			                organization is line sequential.
       Data division.
       File section.
       Fd ItemList.
       01 Item-Info.
         05 ItemID picture 9(5).
         05 ItemDescript picture X(20).
         05 ItemPrice picture 999v99.

       Fd OutputData.
       01 Print-rec picture x(100).

       Working-storage section.

       01 Are-there-More-Records picture xxx value 'YES'.
       
       01 WS-date.
         05 WS-year picture 9(4).
         05 WS-Month picture 99.
         05 WS-days picture 99.

       01 HEADER.
         05 picture x(17) value spaces.
         05 picture x(31) value 'I N F L A T I O N   R E P O R T'.
         05 picture x(10) value spaces.
         05 Date-Out.
           10 day1 picture 99.
           10 line1 picture x.
           10 month picture 99.
           10 line2 picture x.
           10 year picture 9999.
         05 picture x(5) value spaces.
         05 pageNbIntro picture x(5) value 'PAGE '.
         05 pageNb-Out picture ZZ9.
         05 picture x(19) value spaces.

       01 InflationHeader.
         05 picture x(12) value spaces.
         05 picture x(4) value 'YEAR'.
         05 picture x(5) value spaces.
         05 picture x(14) value 'INFLATION RATE'.
         05 picture x(5) value spaces.
         05 picture x(24) value 'ITEM COST WITH INFLATION'.
         05 picture x(36) value spaces.

       01 InflationContainer.
         05 ResultLine.
           10 picture x(10) value spaces.
           10 yearDisplay picture Z9.
           10 picture x(12) value spaces.
           10 inflationRate picture Z9.
           10 percentSign1 picture x value '%'.
           10 picture x(22) value spaces.
           10 finalCost picture Z9.99.
           10 picture x(40) value spaces.

      *to display the product info
         01 ItemNumber.
           10 picture x(4).
           10 picture x(13) value 'ITEM NUMBER: '.
           10 ItemID-out picture x(5).
         10 picture x(78) value spaces.

         01 ItemLife.
           10 picture x(4).
           10 picture x(19) value 'ITEM DESCRIPTION:  '.
           10 ItemDescript-out picture x(20).
         10 picture x(57) value spaces.
 
         01 ItemCost.
           10 picture x(4).
           10 picture x(12) value 'ITEM COST:  '.
           10 ItemPrice-out picture $$$9.99.
         10 picture x(77) value spaces.



      * Loop variables
       01 w picture 99.
       01 y picture 99.
       01 z picture 99.
       01 PageNbCount picture 999 value 1.

      * Interest variables
       01 IntPercent1 picture 9v99999 value 1.08.
       01 IntPercent2 picture 9v99999 value 1.06.

       01 percent1 picture 99 value 8.
       01 percent2 picture 99 value 6.

       01 InflationRateLoop picture 99v99999.

       01 yearLoop picture 99 value 1.
       01 finalCostLoop picture 99v99.
       

       procedure division.
       100-Main-Module.
            open input ItemList
              output OutputData
            
            perform until Are-There-More-Records = 'NO'
                 read ItemList
                     at end
                         move 'NO' to Are-there-More-Records
                     not at end
                         perform 150-Page-Number
                         perform 200-Create-Page
                         perform 250-Item-Output
                         perform 275-Second-Header
                         perform 300-Output-Inflation
                         perform 400-Closing-Page
                 end-read
            end-perform
            close ItemList
              OutputData
            stop run.

       150-Page-Number.
           move PageNbCount to pageNb-Out
           Compute PageNbCount = PageNbCount + 1.

       200-Create-Page.
           move spaces to Print-rec
           Move FUNCTION CURRENT-DATE (1:8) TO WS-date
           move WS-days to day1
           move WS-Month to month
           move WS-year to year
           move '/' to line1
           move '/' to line2
           Move HEADER to Print-rec
           write Print-rec
             after advancing 5 lines.

       250-Item-Output.
           move ItemID to ItemID-out
           move ItemDescript to ItemDescript-out
           move ItemPrice to ItemPrice-out

           move ItemNumber to Print-rec
           write Print-rec
           after advancing 2 lines
           move ItemLife to Print-rec
           write Print-rec
           move ItemCost to Print-rec
           write Print-rec.

       275-Second-Header.
           move InflationHeader to Print-rec
           write Print-rec
           after advancing 2 lines.

       300-Output-Inflation.
           move 1 to w
           move 1 to yearLoop
           move 0 to y
           move 0 to finalCost
           move 1 to z
           move 8 to percent1 
           move 6 to percent2

           perform with test before until w = 11
               move spaces to Print-rec
               move yearLoop to yearDisplay
               Compute yearLoop = yearLoop + 1

               if w < 6
                   move w to z
               end-if
               Compute IntPercent1 = IntPercent1 ** z

               if w > 5
                   move w to y
                   compute y = y - 5
               end-if
               Compute IntPercent2 = IntPercent2 ** y

               if w < 6
                   move 8 to percent1
                   move percent1 to inflationRate
               else
                   move 6 to percent2
                   move percent2 to inflationRate
               end-if

               Compute InflationRateLoop rounded = IntPercent1 * IntPercent2
               
               Compute finalCostLoop rounded = inflationRateLoop * ItemPrice
               move finalCostLoop to finalCost

               move InflationContainer to Print-rec

               move 1.08 to IntPercent1
               move 1.06 to IntPercent2

               write Print-rec
                 after advancing 1 line
               compute w = w + 1
           end-perform.

       400-Closing-Page.
           move spaces to Print-rec
           write Print-rec
             after advancing page.

       end program Program1.
