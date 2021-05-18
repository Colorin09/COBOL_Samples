       identification division.
       program-id. Program1 as "CarRent.Program1".
       
       environment division.
       data division.
       working-storage section.

      *Math Variables
       01 PriceDay picture 999.
       01 PriceMile picture 9v99.

       01 Continue-Input picture x value 'Y'.
       01 VoidLine picture x(80) value spaces.

      *Input data fields

       01 Client-data.
         05 Client_Lname Picture x(20).
         05 firstInitial Picture x.
         05 CarType Picture x.
         05 MilesDriven Picture 9(5).
         05 RentedDays Picture 999.

      *Output Fields

       01 PrintOut.
         05 picture xxx value spaces.
         05 Client_LnameOut Picture x(20).
         05 picture x value spaces.
         05 ClientInitial picture x.
         05 period1 picture x value '.'.
         05 picture xxx value spaces.
         05 CarTypeOut picture x(10).
         05 picture xxx value spaces.
         05 AmountMiles picture $ZZZZZ9.99.
         05 picture xxx value spaces.
         05 AmountDays picture $ZZZZZ9.99.
         05 picture xxx value spaces.
         05 TotalAmount picture $ZZZZZZ9.99.
         05 picture xx value spaces.

       01 WS-AmountMiles picture 999999v99.
       01 WS-AmountDays picture 999999v99.
       
       01 HEADER.
         05 picture x(17) value spaces.
         05 picture x(47) value 'Rental Car Company Report - Customer Rent Total'.
         05 picture x(16) value spaces.
      *Fields must be more little than 80 spaces, if not screen will fail
       01 field1.
         05 picture x(6) value spaces.
         05 picture x(8) value 'CUSTOMER'.
         05 picture x(8) value spaces.
         05 picture x(5) value 'FIRST'.
         05 picture xxx value spaces.
         05 picture x(4) value 'TYPE'.
         05 picture x(13) value spaces.
         05 picture x(5) value 'MILES'.
         05 picture x(8) value spaces.
         05 picture xxxx value 'DAYS'.
         05 picture x(8) value spaces.
         05 picture x(5) value 'FINAL'.

      * Miles driven erased

       01 field2.
         05 picture x(8) value spaces.
         05 picture x(4) value 'NAME'.
         05 picture x(10) value spaces.
         05 picture x(5) value 'INIT.'.
         05 picture x(20) value spaces.
         05 picture x(4) value 'COST'.
         05 picture x(9) value spaces.
         05 picture x(4) value 'COST'.
         05 picture x(8) value spaces.
         05 picture x(5) value 'TOTAL'.

       SCREEN SECTION.
       01 Screen-1.
         05 blank screen beep foreground-color 2 background-color 0 highlight required.
         05 line 1 column 5 from HEADER.
         05 line 4 column 3 value 'Please enter the wanted value on each field: '.
         05 line 5 column 5 value '*Press tab to change of field*'.
         05 line 7 column 3 value 'First Initial: '.
         05 column 19 picture x to firstInitial.
         05 line 9 column 3 value 'Last Name: '.
         05 column 14 picture x(20) to Client_LName.
         05 line 11 column 3 value 'Car Number Type: '.
         05 column 20 picture 9 to CarType.
         05 line 13 column 3 value 'Amount of miles driven: '.
         05 column 27 picture 9(5) to MilesDriven.
         05 line 15 column 3 value 'Number of days rented: '.
         05 column 28 picture 999 to RentedDays.
         05 line 17 column 3 value 'Press Enter once Finished'.

       01 Screen-2.
         05 blank screen foreground-color 2 background-color 0 highlight.
         05 line 1 column 1 from VoidLine.
         05 line 3 column 1 from HEADER.
         05 line 5 column 1 from field1.
         05 line 6 column 1 from field2.
         05 line 8 column 1 from PrintOut.
         05 line 20 column 3 value 'Is there more records to enter? (Enter Y or N)'.
         05 line 21 column 3 value 'Enter answer :'.
         05 column 20 to Continue-Input.
         

       procedure division.
       100-Initial-Module.
           perform until Continue-Input = 'n' or 'N'
               perform 150-Display-Screen
               perform 200-calc-module
               perform 300-Display-Output
           end-perform
             stop run.
       150-Display-Screen.
           Display Screen-1
           accept Screen-1.
       200-calc-module.
           move Client_Lname to Client_LnameOut
           move firstInitial to Clientinitial
           move '.' to period1

           if Cartype is equal to 1
               move 26 to PriceDay
               move 0.18 to PriceMile
               move 'Toyota' to CarTypeOut

           else
               if Cartype is equal to 2
                   move 32 to PriceDay
                   move 0.22 to PriceMile
                   move 'Chevrolet' to CarTypeOut
                   else
           
                if CarType is equal to 3
                   move 43 to PriceDay
                   move 0.28 to PriceMile
                   move 'Cadillac' to CarTypeOut
                   end-if

               end-if

           end-if

               if MilesDriven > 100
                   Compute WS-AmountMiles rounded = (MilesDriven - 100) * PriceMile
               else
                     Move 0 to WS-AmountMiles
               end-if

               Compute WS-AmountDays rounded = RentedDays * Priceday
               Compute TotalAmount rounded = WS-AmountDays + WS-AmountMiles

               move WS-AmountMiles to AmountMiles
               move WS-AmountDays to AmountDays.

       300-Display-Output.
           Display Screen-2
           Accept Screen-2.

       end program Program1.
