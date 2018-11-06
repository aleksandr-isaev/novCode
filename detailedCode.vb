Module Module1

    Public Structure recording  'a public structure that requires, when called to involve cowId, day and yield
        Public cowID As Integer
        Public day As Integer
        Public yield As Single
    End Structure

    Dim LOWYIELD As Single = 12 'set to 12 because minimum allowed is 12

    Sub Main()
        Dim herdSize As Integer
        Dim choice As Char
        Dim cows() As Integer
        Dim herdData(,) As Single
        Dim yearlyRecords(,) As Integer
        Dim record1 As recording          'this variable when called requires three things - cowId, day and yield
        Dim total As Single
        Dim userinput As Integer 'added!!!! user input for whether they want to run the practice program or not
        Dim count As Integer = -1 'added!!
        Dim dead(0) As Integer  'added!!!! 'make sure dim dead(0)
        Dim input As Integer 'added!!!! user input for the herd size
        Dim temp(,) As Single 'added!!!! to help for adding cow
        Dim temp2(,) As Integer 'added!!!! to help for adding cow
        Dim best As Integer = 0 'added!!!! to help for weekly stats
        Dim bestCow As Integer 'added!!!! to help for weekly stats
        Dim bestAr(,) As Single 'added!!!! to help for weekly stats
        Do 'added loop!!!! This do loop makes sure that the user inputs 1 or 0. 
            Console.WriteLine("Do you want to run the practice program? 1 for yes and 0 for no")
            userinput = Console.ReadLine()
        Loop Until userinput = 0 Or userinput = 1
        If userinput = 1 Then                            'added!!!! if they want practice, added console.readline = "1" 'array file - gives preset data of 7
            cows = loadCowData(7)                         'this goes to the function where there are preset cow IDs available - 579,516,907,669,184 etc.
            herdData = loadHerdData(herdData)             'this calls the function with alL the preset data of yielded stuff
            yearlyRecords = loadYearData(yearlyRecords)   'this calls the function with all the preset data of the yearly stuff - pre done that outputs all the weeks done so far
            herdSize = 7                                  'sets herd size to 7 
        ElseIf userinput = 0 Then                          'added!!!! 'if they dont want to practice = "0" 
            Console.WriteLine("Input herd size")           'make their own data
            input = Console.ReadLine()                     'added variable!!!!created this variable input so that herd size can be stored
            If input = 0 Or input = 1 Then                 'added if statement!!!! this if statement to make sure that herd size inputed is not 0 or 1
                Console.WriteLine("Input herd size again. 0 or 1 is not a valid herd size. ")
                Do                                        'added do loop!!!! this do loop keeps outputting the message until the user inputs a valid herd size
                    input = Console.ReadLine()
                    If input = 1 Or input = 0 Then
                        Console.WriteLine("Input herd size again. 0 or 1 is not a valid herd size. ")
                    End If
                Loop Until input <> 0 And input <> 1
            End If
            herdSize = input - 1                            'this is minus one because arrays counting is one less because arrays start from o
            ReDim cows(herdSize)                            'essentially resets the data in arrays so can fill up on own 'an array declared initially at the top
            ReDim herdData(herdSize, 14)                    'REDIM gives a blank array and changes the size of array  'an array declared initially at the top
            ReDim yearlyRecords(herdSize, 52)               'an array declared initially at the top
            cows = inputCowIds(herdSize)                    'calls a function that provides the cow IDs numbers randomly
        End If

        Do
            menu()                                          'this calls the menu function which shows the options that the user can choice
            choice = Console.ReadLine                       'the choice is read
            Select Case LCase(choice)                       'the choice is lower cased 
                Case "a"                                    'this action is : Input a milking data
                    displayCows(cows, dead)                 'this calls a subroutine which prints the cowID number and "    " (spaces)
                    record1 = TakeRecording(cows, herdData, herdSize) 'added , herdSize 
                    For x = 0 To count                      'added for loop !!!! This for loop just makes sure that they don't want to add yield to a cow that has been executed
                        If record1.cowID = dead(count) Then
                            Console.WriteLine("Sorry but that cow is dead")
                            Exit Select
                        End If
                    Next
                    herdData = UpdateYield(cows, herdData, record1)  'this calls a function that allows overriding of milk yield. data type is integer
                Case "b"                                             'this action is: Output weekldy data
                    Console.WriteLine("Weekly yield table so far this week")
                    outputWeeklyData(cows, herdData)        'this subroutine outputs the stats so far for the week done
                    outputWeeklyData1(cows, herdData)
                Case "c"                                     'this action is weekly statistics
                    Console.WriteLine()                     'spacing so all is legible
                    Console.WriteLine("WEEKLY STATISTICS SO FAR THIS WEEK")
                    Console.WriteLine()
                    CowConcern(cows, herdData, herdSize)    'this sub outputs the cow ID of those with a yield of less than 12 for 4 or more days
                    Console.WriteLine()
                    Console.WriteLine("Top cow this week: " & cows(TopCow(herdData, herdSize))) 'this function outputs the cow id of that with top ranks. Data type : integer
                    Console.WriteLine()
                    For x = 0 To herdSize
                        total += Average(herdData, x)       'this sums the total averages for cow herd size 'function calls average per cow per week 'removed cows, herdSize
                        Console.WriteLine("Cow " & cows(x) & " average = " & Math.Round(Average(herdData, x))) 'added math.Round!!!!'this prints the average per cow 'Math.Round added to round to nearest litre
                    Next
                    Console.WriteLine()
                    Console.WriteLine("Weekly total so far " & Math.Round(WeeklyTotal(herdData, herdSize))) 'added Math.Round!!!!'this function adds all the milk yielded so far 'Math.Round added to round to nearest litre
                    Console.WriteLine()
                    Console.WriteLine("Average so far this week " & Math.Round(total / (herdSize + 1))) 'added Math.Round and brackets around herdSize + 1!!!!'Math.Round added to round to nearest litre'added brackets since set herdsize to -1 for array and so need to bring it back to the actual size
                    Console.WriteLine()
                    Console.WriteLine()
                Case "d"                                       'end week
                    yearlyRecords = EndWeek(cows, herdData, herdSize, yearlyRecords) 'function outputs average per cow
                Case "e"                                     'output yearly data
                    outputYearlyData(cows, yearlyRecords)    'this sub outputs the yearly data
                    outputYearlyData2(cows, yearlyRecords)
                Case = "f"                                      'added case!!!! this option  allows the user to kill a cow
                    count = count + 1
                    ReDim dead(count)
                    dead(count) = KillCow(cows)
                Case "g" 'added case!!!! this option allows the user to add a cow
                    cows = AddCow(cows, herdSize)
                    herdSize += 1
                    temp = herdData
                    ReDim herdData(herdSize, 14)
                    For x = 0 To herdSize - 1
                        For y = 0 To 13
                            herdData(x, y) = temp(x, y)
                        Next
                    Next
                    temp2 = yearlyRecords
                    ReDim yearlyRecords(herdSize, 52)
                    For x = 0 To herdSize - 1
                        For y = 0 To 51
                            yearlyRecords(x, y) = temp2(x, y)
                        Next
                    Next
                    Console.WriteLine("Cow has been added. The ID number is: " & cows(cows.Length - 1))
                Case "x"                                   'close system
                    Console.WriteLine("goodbye")
                Case Else
                    Console.WriteLine("Please pick from the menu")
            End Select
        Loop Until choice = "x"
        Console.ReadLine()

    End Sub

    Function Average(ByVal milkings(,) As Single, ByVal row As Integer) As Single 'function returning the average milking per cow
        For y = 0 To 13
            Average += milkings(row, y)                                             'this sums up each line
        Next
        Return Average / 7                                                          'this division means average per day

    End Function


    Function EndWeek(ByVal herd() As Integer, ByVal milkings(,) As Single, ByVal NumCows As Integer, ByVal yearData(,) As Integer) As Integer(,)
        Dim total As Single = 0                           'you use herd(x) because in the future, herd((x) is gonna be used as a reference later on
        Dim currentWeek As Integer = -1
        Dim check As Boolean = False                     ' added !!!!
        Do
            currentWeek += 1
            If yearData(0, currentWeek) = 0 And NumCows > 1 Then 'added IF statements!!!! These make sure that there is more than one cow for the week
                If yearData(1, currentWeek) = 0 Then
                    check = True
                End If
            End If
        Loop Until check = True And yearData(0, currentWeek) = 0 'added check = true to condition!!!!'loops until week number is 52 and more than 1 cow
        For x = 0 To NumCows
            total = 0
            For y = 0 To 13
                total += milkings(x, y)                          'adds total per cow per week
            Next
            yearData(x, currentWeek) = total                    'stores the data
            Console.WriteLine("Cow " & herd(x) & " average = " & Math.Round((Average(milkings, x)))) 'added Math.Round!!!! 'outputs average per cow 'Math.Round rounds the yield average 'removed numcows and herd byval!!!!
        Next
        For x = 0 To NumCows                                'added for loop!!!! this for loop is added to clear the weekly table once the week has ended 
            For y = 0 To 13                                  'goes through each cow and each reading and sets the data all back to 0
                milkings(x, y) = 0
            Next
        Next
        Return yearData
    End Function


    Function TopCow(ByVal milkings(,) As Single, ByVal NumCows As Integer) As Integer
        Dim total As Single = 0
        Dim max = -5
        For x = 0 To NumCows             'goes through each cow
            total = 0
            For y = 0 To 13
                total += milkings(x, y) 'this is total = total + milkings(x,y) 'adds all for that one cow
            Next
            If total > max Then         'this basically allows maximum to be stored
                max = total
                TopCow = x
            End If
        Next
        Return TopCow                  'returns position
    End Function


    Sub CowConcern(ByVal cows() As Integer, ByVal milkings(,) As Single, ByVal NumCows As Integer) '
        Dim total As Single = 0
        Dim average As Single = 0
        Dim lowCows As String = ""
        Dim lowDay As Integer = 0
        For x = 0 To NumCows    'goes through each cow
            total = 0           'set to 0 so that each cow can be recorded on its own
            average = 0
            lowDay = 0
            For y = 0 To 6
                If milkings(x, (y * 2)) + milkings(x, ((y * 2) + 1)) < LOWYIELD Then 'this line means if milking total per day is less than 12 then add 1 to the lowDay count
                    lowDay += 1
                End If
            Next
            If lowDay >= 4 Then           'this does the check if the lowday count is greater than equal to 4
                lowCows &= cows(x) & ", " 'lowCow = lowCows & cows(x) & ", " <- low cows is a string where if for that cow, low day is more than or equal to 4 then its cow id number is stored
            End If
        Next
        If lowCows = "" Then
            Console.WriteLine("All cows have exceeded minimum yield this week")
        Else
            Console.WriteLine("The following cows have low yield this week " & Mid(lowCows, 1, Len(lowCows) - 2) & ".") 'prints it all out 'Mid function gets rid of the ", " at the end
        End If
    End Sub

    Function WeeklyTotal(ByVal milkings(,) As Single, ByVal NumCows As Integer) As Integer
        For x = 0 To NumCows
            For y = 0 To 13
                WeeklyTotal = WeeklyTotal + milkings(x, y) 'adds all readings per cow per taking
            Next
        Next
        Return Math.Round(WeeklyTotal) 'added Math.Round!!!!'rounds weekly total 
    End Function


    Function UpdateYield(ByVal cows() As Integer, ByVal milkings(,) As Single, ByVal newData As recording) As Single(,)
        Dim location As Integer
        For x = 0 To cows.Length - 1
            If cows(x) = newData.cowID Then
                location = x
            End If
        Next
        If milkings(location, newData.day * 2) = 0 Then 'changed operators!!!! 'changed operation from <> to =            
            milkings(location, (newData.day * 2)) = newData.yield
        ElseIf milkings(location, (newData.day * 2) + 1) = 0 Then 'added one brings you to the second milking of the data
            milkings(location, (newData.day * 2) + 1) = newData.yield 'this brings you to the first milking of that day
        Else
            Console.WriteLine("This cow has already been milked twice today. ")
        End If
        Return milkings
    End Function

    Function TakeRecording(ByVal cows() As Integer, ByVal milkings(,) As Single, ByVal herdSize As Integer) As recording
        Dim newVal As Single 'added variable!!!! This variable is needed to make sure that yield is added to one d.p.
        Dim input As Integer 'added variable!!!! This variable is needed to make sure that the cow ID entered is valid (i.e. exists)
        Dim correct As Integer = 0 'added variable!!!! This variable is needed to make sure that the cow ID entered is valid
        Console.WriteLine()
        Console.WriteLine("Please Enter the cow ID")
        input = Console.ReadLine  'added!!!!'input their cow ID
        For x = 1 To herdSize       'added for loop!!!!For loop to check if cow ID entered is a real one or not
            If input = cows(x) Then
                correct = 1
            End If
        Next
        Do While correct <> 1   'added do while loop!!!!This loop gives the validity
            For x = 0 To herdSize
                If input = cows(x) Then
                    correct = correct + 1
                End If
            Next x
            If correct <> 1 Then
                Console.WriteLine("the cow ID entered was invalid. Please re enter")
                input = Console.ReadLine()
            End If
        Loop
        TakeRecording.cowID = input
        Console.WriteLine("Please enter Day mon, tue, etc (give the first 3 letters only)") 'added - not entirely essential though!!!! added first 3 letters only - to make sense
        TakeRecording.day = ConvertDay(LCase(Console.ReadLine))  'added lcase!!!! So incase they write MON ... still works'inputs day of week
        Do While TakeRecording.day = 7 'added do while loop!!!! This loop validates the input of the day of week
            Console.WriteLine("That is not a valid day. Please enter the day: mon, tue, etc")
            TakeRecording.day = ConvertDay(LCase(Console.ReadLine))
        Loop
        Console.WriteLine("Please Enter the yield")
        TakeRecording.yield = Console.ReadLine      'input how much milk they made 
        newVal = (((TakeRecording.yield * 10) \ 1) / 10) 'added calc!!!! 'this and the do while loop makes sure that it is one d.p. '
        Do While newVal <> TakeRecording.yield 'added do while loop!!!! 'this yield makes sure that yield is to 1 d.p.
            Console.WriteLine("Please enter a yield to one decimal place")
            TakeRecording.yield = Console.ReadLine
        Loop
        Do While TakeRecording.yield < 0 'added do while loop!!!! 'this loop makes sure that yield is greater than or equal to 0
            Console.WriteLine("Please enter a yield greater than or equal to 0. Please try again.")
            TakeRecording.yield = Console.ReadLine
        Loop
        Return TakeRecording
    End Function

    Function ConvertDay(ByVal day As String) As String 'convert day to number
        Select Case day
            Case "mon"
                Return 0
            Case "tue"
                Return 1
            Case "wed"
                Return 2
            Case "thu"
                Return 3
            Case "fri"
                Return 4
            Case "sat"
                Return 5

            Case "sun"
                Return 6
            Case Else
                Return 7 'changed from 7 to 1!!!! Change makes sure that the value entered doesn't get mixed up in Tuesdays data
        End Select
    End Function

    Sub displayCows(ByVal cows() As Integer, ByVal rip() As Integer) 'added rip() in byVal!!!! This is so that if a cow has been killed, it is no longer a visible option to choose from
        For x = 0 To cows.Length - 1 '
            For y = 0 To rip.Length - 1 'added for loop!!!!
                If cows(x) = rip(y) Then 'added if statement!! This if statement checks that if cow killed is same as rip then move on and print the next cow
                    x = x + 1
                End If
            Next
            Console.Write(cows(x) & " ")
        Next
    End Sub

    Sub menu()
        Console.WriteLine("Select from the following options")
        Console.WriteLine("a. Input a milking data")
        Console.WriteLine("b. Output Weekly Data")
        Console.WriteLine("c. Weekly Statistics")
        Console.WriteLine("d. End Week")
        Console.WriteLine("e. Output Yearly Data")
        Console.WriteLine("f. Remove Cow") 'added this option!!!! This allows user to have the chance to kill a cow
        Console.WriteLine("g. Add Cow") 'added this option!!!! This allows user to have the chance to add a cow later on in the program
        Console.WriteLine("x. Close System")
    End Sub

    Function KillCow(ByVal cows() As Integer) 'added function!!!! The main aim is to identify which cow should be killed
        Dim death As Integer
        Console.WriteLine("Which cow do you want to kill?")
        death = Console.ReadLine()
        Return death
    End Function


    Sub outputYearlyData(ByVal cows() As Integer, ByVal milkingData(,) As Integer)
        'original
        '     Console.WriteLine()
        '    Console.WriteLine("YEARLY WEEKLY TOTAL DATA")
        '   Console.Write("Cow ID ")
        '  For x = 1 To 52 'This loop prints weeks 1 -52
        ' If x < 10 Then
        'Console.Write("  " & x & " ")
        'Else
        'Console.Write(" " & x & " ")
        'End If
        'Next
        'Console.WriteLine()
        'For x = 0 To cows.Length - 1 'row headings '.length returns the number of stuff in the array
        'Console.Write(cows(x) & "      ")
        'For y = 0 To 51

        'Console.Write(milkingData(x, y) & " ") 'prints the weekly data
        'For z = 0 To 2 - Len(CStr(milkingData(x, y))) 'spacing for the table
        'Console.Write(" ")
        'Next
        'Next
        'Console.WriteLine()
        'Next
        'Console.WriteLine()
        'Console.WriteLine()

        'edited'
        Console.WriteLine()
        Console.WriteLine("YEARLY WEEKLY TOTAL DATA")
        Console.Write("    ")   'changed "cow id" to "     "
        For x = 0 To cows.Length - 1        'added for loop!!!! This for loop prints each cow id along the top
            Console.Write(cows(x) & " ")
        Next
        Console.WriteLine()
        For x = 0 To 51 'added for loop!!!! this goes through each number of weeks
            If x < 9 Then 'added if statement!!!! this is necessary in order to allow for allignment in the table
                Console.Write("  " & x + 1 & " ")
            Else
                Console.Write(" " & x + 1 & " ")
            End If
            For y = 0 To cows.Length - 1 'added for loop!!!!'this for loop inputs the data
                Console.Write(milkingData(y, x) & " ")
                For z = 0 To 2 - Len(CStr(milkingData(y, x))) 'added for loop!!!!'this adds any spaces necessary so that the table is alligned
                    Console.Write(" ")
                Next
            Next
            Console.WriteLine()
        Next
        Console.WriteLine()
        Console.WriteLine()
        Console.WriteLine()
    End Sub


    Sub outputWeeklyData(ByVal cows() As Integer, ByVal milkingData(,) As Single)
        Console.WriteLine()
        Console.WriteLine("WEEKLY DATA")
        Console.WriteLine("Cow ID" & "     " & "Mon           " & "Tue           " & "Wed           " & "Thu           " & "Fri           " & "Sat           " & "Sun           ")
        For x = 0 To cows.Length - 1
            Console.Write(cows(x) & "        ")
            For y = 0 To 13
                Console.Write(milkingData(x, y)) ' x is the cow in whatever position is wanted and y is the thing whether it is milking 1 or 2 of each day 0- mon 1; 1- mon 2; 3 - tue 1 etc.
                For z = 0 To 6 - Len(CStr(milkingData(x, y))) 'this for loop counts number of spaces required to make this a nice neat table
                    Console.Write(" ")
                Next
            Next
            Console.WriteLine() ' so each cow is on a new line
        Next
        Console.WriteLine()     'keeps space per case chosen
        Console.WriteLine()        'keeps space per case chosen
    End Sub

    Function inputCowIds(ByVal cows As Integer) As Integer() 'Function that creates the random cowIDs, returns them as an integer value
        Randomize()                                   'Added !!!! This is necessary to randomise the stuff'
        Dim uniqueID As Boolean = True                'Added !!!! This variable is necessary in order to make sure that each cowID is unique
        Dim currentCowId As Integer                   'Added !!!! This variable is necessary in order to make sure that each cowID is unique
        Dim newCowIds(cows) As Integer
        For x = 0 To cows                             'this for loop generates the cowIDs
            Do                                      'Added do loop  and for loop!!!!'this do loop makes sure that no two cow IDs are the same
                uniqueID = True
                currentCowId = ((Rnd() * 899) + 100) 'changed variable from newCowIds(x) to currentCowId!!
                For count = 1 To cows
                    If currentCowId = newCowIds(count) Then
                        uniqueID = False
                    End If
                Next
            Loop Until uniqueID
            newCowIds(x) = currentCowId
        Next
                Return newCowIds                            'the randomly generated cow IDs are returned
    End Function
    Function AddCow(ByVal cows() As Integer, ByVal cowNum As Integer) 'added this function!!!!
        Dim newCows(cowNum + 1) As Integer
        Dim nwAr(0) As Integer
        For x = 0 To cowNum
            newCows(x) = cows(x)
        Next
        nwAr = inputCowIds(0)
        newCows(cowNum + 1) = nwAr(0)
        Return newCows
    End Function
    Function loadCowData(ByVal size As Integer) As Integer()
        loadCowData = {579, 516, 907, 669, 184, 757, 970, 727}
        Return loadCowData
    End Function

    Function loadHerdData(ByVal herd(,) As Single) As Single(,)
        herd = {{1.51, 10.56, 15.03, 5.8, 4.54, 13.09, 6.46, 16.31, 3.38, 0, 0, 10.93, 0, 0},
                  {0, 6.38, 7.98, 18.5, 4.74, 6.13, 0, 16.37, 3.3, 13.91, 1.8, 0, 0, 0},
                  {14.97, 9.24, 0, 3.98, 0.35, 8.34, 0, 10.45, 5.68, 10.99, 1.22, 0, 0, 0},
                  {12.97, 14.43, 12.2, 8.59, 0, 9.65, 7.19, 7.86, 14, 2.53, 18.44, 4.05, 0, 0},
                  {0, 16.17, 8.32, 4.99, 5.11, 13.33, 17.1, 1.21, 0, 8.66, 6.72, 12.44, 0, 0},
                  {11.67, 9.9, 12.69, 8.75, 3.39, 6.6, 16.95, 9, 4.84, 8.79, 16.62, 3, 0, 0},
                  {14.98, 6.03, 13.67, 16.04, 7.84, 15.43, 0, 8.78, 14, 9.42, 14.67, 0, 0, 0},
                  {12.69, 16.35, 0.75, 13.65, 5.07, 0.47, 18.47, 12.08, 11.12, 1.83, 17.63, 8.31, 0, 0}}
        Return herd
    End Function

    Function loadYearData(ByVal year(,) As Integer) As Integer(,)
        year = {{86, 90, 86, 100, 72, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {77, 82, 62, 110, 78, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {65, 68, 60, 84, 101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {67, 86, 72, 67, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {101, 106, 104, 107, 80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {100, 102, 88, 62, 108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {109, 64, 99, 85, 98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {96, 76, 79, 99, 82, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}}
        Return year
    End Function
    Function TableSortYear(ByVal cows() As Integer, ByVal milkingData(,) As Integer) As Integer() 'added function!!!!
        Dim lowestCow As Integer = 1000
        Dim previousCow As Integer = 0
        Dim position(cows.Length - 1) As Integer
        For z = 0 To cows.Length - 1
            For x = 0 To cows.Length - 1
                If cows(x) > previousCow Then
                    If cows(x) < lowestCow Then
                        lowestCow = cows(x)
                        position(z) = x
                    End If

                End If
            Next
            previousCow = lowestCow
            lowestCow = 1000
        Next
        Return position
    End Function
    Function TableSortWeek(ByVal cows() As Integer, ByVal milkingData(,) As Single) As Integer() 'added function!!!!
        Dim lowestCow As Integer = 1000
        Dim previousCow As Integer = 0
        Dim position(cows.Length - 1) As Integer
        For z = 0 To cows.Length - 1
            For x = 0 To cows.Length - 1
                If cows(x) > previousCow Then
                    If cows(x) < lowestCow Then
                        lowestCow = cows(x)
                        position(z) = x
                    End If

                End If
            Next
            previousCow = lowestCow
            lowestCow = 1000
        Next
        Return position
    End Function
    Sub Table1(ByVal cows() As Integer, ByVal milkingData(,) As Integer, ByVal position() As Integer)
        Dim currentPosition As Integer
        For x = 1 To 28
            If x < 10 Then
                Console.Write("  " & x & " ")
            Else
                Console.Write(" " & x & " ")
            End If
        Next

        Console.WriteLine()
        For x = 0 To cows.Length - 1
            currentPosition = position(x)
            Console.Write(cows(currentPosition) & "      ")
            For y = 0 To 27

                Console.Write(milkingData(currentPosition, y) & " ")
                For z = 0 To 2 - Len(CStr(milkingData(currentPosition, y)))
                    Console.Write(" ")
                Next
            Next
            Console.WriteLine()
        Next

        Console.Write("        ")
        For x = 29 To 52
            If x < 10 Then
                Console.Write("  " & x & " ")
            Else
                Console.Write(" " & x & " ")
            End If
        Next
        Console.WriteLine()
        For x = 0 To cows.Length - 1
            currentPosition = position(x)
            Console.Write(cows(currentPosition) & "      ")
            For y = 28 To 51

                Console.Write(milkingData(currentPosition, y) & " ")
                For z = 0 To 2 - Len(CStr(milkingData(currentPosition, y)))
                    Console.Write(" ")
                Next
            Next
            Console.WriteLine()
        Next
    End Sub
    Sub Table2(ByVal cows() As Integer, ByVal milkingData(,) As Integer, ByVal position() As Integer)
        Dim currentposition As Integer
        For x = 0 To cows.Length - 1
            currentposition = position(x)
            Console.Write(cows(currentposition) & "     ")
        Next
        Console.WriteLine()
        For x = 0 To 51
            If x < 9 Then
                Console.Write("  " & x + 1 & "       ")
            Else
                Console.Write(" " & x + 1 & "       ")
            End If
            For y = 0 To cows.Length - 1
                currentposition = position(y)
                Console.Write(milkingData(currentposition, x) & "     ")
                For z = 0 To 2 - Len(CStr(milkingData(currentposition, x)))
                    Console.Write(" ")
                Next
            Next
            Console.WriteLine()
        Next
    End Sub
    Sub outputWeeklyData1(ByVal cows() As Integer, ByVal milkingData(,) As Single)
        Dim position(cows.Length) As Integer
        Dim currentposition As Integer
        Console.WriteLine()
        position = TableSortWeek(cows, milkingData)
        Console.WriteLine("Sorted")
        Console.WriteLine("WEEKLY DATA")
        Console.WriteLine("Cow ID" & "     " & "Mon           " & "Tue           " & "Wed           " & "Thu           " & "Fri           " & "Sat           " & "Sun           ")
        For x = 0 To cows.Length - 1
            currentposition = position(x)
            Console.Write(cows(currentposition) & "        ")
            For y = 0 To 13

                Console.Write(milkingData(currentposition, y))
                For z = 0 To 6 - Len(CStr(milkingData(currentposition, y)))
                    Console.Write(" ")
                Next


            Next

            Console.WriteLine()
        Next
        Console.WriteLine()
        Console.WriteLine()
    End Sub
    Sub outputYearlyData2(ByVal cows() As Integer, ByVal milkingData(,) As Integer)
        Dim position(cows.Length - 1) As Integer
        Console.WriteLine()
        position = TableSortYear(cows, milkingData)
        Console.WriteLine("Sorted")
        Console.WriteLine("YEARLY WEEKLY TOTAL DATA")
        Console.WriteLine("Table 1")
        Console.WriteLine()
        Console.Write("Cow ID ")
        Table1(cows, milkingData, position)
        Console.WriteLine()
        Console.WriteLine("Table 2")
        Console.WriteLine()
        Console.Write("Cow ID ")
        Console.Write("   ")
        Table2(cows, milkingData, position)
        Console.WriteLine()
        Console.WriteLine()

    End Sub
End Module

