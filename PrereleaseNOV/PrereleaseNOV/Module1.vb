Module Module1

    Public Structure recording
        Public cowID As Integer
        Public day As Integer
        Public yield As Single
    End Structure
    Dim LOWYIELD As Single = 12
    Sub Main()
        Dim herdSize As Integer
        Dim choice As Char
        Dim cows() As Integer
        Dim herdData(,) As Single
        Dim yearlyRecords(,) As Integer
        Dim record1 As recording
        Dim total As Single
        Dim dead As Integer
        Dim count As Integer = -1
        Dim temp(,) As Single
        Dim temp2(,) As Integer
        Dim best As Integer = 0
        Dim bestCow As Integer
        Dim bestAr(,) As Single

        Console.WriteLine("Do you want to run the practice program? 1 for yes and 0 for no")
        If Console.ReadLine Then
            cows = loadCowData(7)
            herdData = loadHerdData(herdData)
            yearlyRecords = loadYearData(yearlyRecords)
            herdSize = 7
            ReDim bestAr(herdSize, 14)
        Else
            Console.WriteLine("Input herd size")
            herdSize = Console.ReadLine() - 1
            ReDim cows(herdSize)
            ReDim herdData(herdSize, 14)
            ReDim yearlyRecords(herdSize, 52)
            ReDim bestAr(herdSize, 14)
            cows = inputCowIds(herdSize)
        End If

        Do
            menu()
            choice = Console.ReadLine
            Select Case LCase(choice)
                Case "a"
                    displayCows(cows)
                    record1 = TakeRecording(cows, herdData)
                    herdData = UpdateYield(cows, herdData, record1)
                Case "b"
                    Console.WriteLine("Weekly yield table so far this week")
                    outputWeeklyData(cows, herdData)

                Case "c"
                    For x = 0 To herdSize
                        For y = 0 To 13
                            bestAr(x, y) = herdData(x, y)
                        Next
                    Next
                    Console.WriteLine()
                    Console.WriteLine("WEEKLY STATISTICS SO FAR THIS WEEK")
                    Console.WriteLine()
                    CowConcern(cows, herdData, herdSize)
                    Console.WriteLine()
                    Console.WriteLine("Top cow this week: " & cows(TopCow(herdData, herdSize)))
                    Console.WriteLine()
                    For x = 0 To herdSize
                        For y = 0 To herdSize
                            If Average(cows, bestAr, herdSize, y) > best Then
                                best = Average(cows, bestAr, herdSize, y)
                                bestCow = cows(y)
                            End If
                        Next
                        For z = 0 To herdSize
                            If bestCow = cows(z) Then
                                bestAr(z,z) = -1000
                            End if                        
                        Next
                        total += Average(cows, herdData, herdSize, x)
                        Console.WriteLine("Cow " & bestCow & " average = " & Math.Round(best))
                        best = 0
                    Next
                    Console.WriteLine()
                    Console.WriteLine("Weekly total so far " & Math.Round(WeeklyTotal(herdData, herdSize)))
                    Console.WriteLine()
                    Console.WriteLine("Average so far this week " & Math.Round(total / herdSize + 1))
                    Console.WriteLine()
                    

                Case "d"
                    yearlyRecords = (EndWeek(cows, herdData, herdSize, yearlyRecords))
                Case "e"
                    outputYearlyData(cows, yearlyRecords)
                Case "f"
                    displayCows(cows)
                    Console.WriteLine("Enter cow you want to kill")
                    dead = Console.ReadLine()
                    yearlyRecords = KillCow(cows, yearlyRecords, dead)
                    For x = 0 To cows.Length - 1
                        If dead = cows(x) Then
                            For z = 0 To 13
                                If x = cows.Length - 1 Then
                                    Exit For
                                End If
                                If x = cows.Length - 2 Then
                                    herdData(x + 1, z) = 0
                                Else
                                    herdData(x, z) = herdData(x + 1, z)
                                End If
                            Next
                        End If
                    Next
                    For x = 0 To cows.Length - 1
                        If dead = cows(x) Then
                            If x = cows.Length - 2 Then
                                cows(x + 1) = 0
                                Exit For
                            Else
                                cows(x) = cows(x + 1)
                            End If
                        End If
                    Next
                    herdSize = herdSize - 1

                Case "g"
                            cows = AddCow(cows, herdSize)
                    herdSize += 1
                    temp = herdData
                    ReDim herdData(herdSize, 14)
                    ReDim bestAr(herdSize, 14)
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
                    Console.WriteLine("Cow has been added: ID" & cows(cows.Length - 1))
                Case "x"
                    Console.WriteLine("goodbye")
                Case Else
                    Console.WriteLine("Please pick from the menu")
            End Select

        Loop Until choice = "x"
        Console.ReadLine()

    End Sub
    Function Average(ByVal herd() As Integer, ByVal milkings(,) As Single, ByVal NumCows As Integer, ByVal row As Integer) As Single

        For y = 0 To 13
            Average += milkings(row, y)
        Next

        Return Average / 7

    End Function
    Function EndWeek(ByVal herd() As Integer, ByVal milkings(,) As Single, ByVal NumCows As Integer, ByVal yearData(,) As Integer) As Integer(,)
        Dim total As Single = 0
        Dim currentWeek As Integer = -1
        Dim check As Boolean = False
        Do
            currentWeek += 1
            If milkings(0, currentWeek) = 0 And NumCows > 1 Then
                If milkings(1, currentWeek) = 0 Then
                    check = True
                End If
            Else
                check = True
            End If

        Loop Until check = True
        For x = 0 To NumCows
            total = 0
            For y = 0 To 13
                total += milkings(x, y)
            Next

            yearData(x, currentWeek) = total
            Console.WriteLine("Cow " & herd(x) & " average = " & Math.Round(Average(herd, milkings, NumCows, x)))
        Next
        For x = 0 To NumCows
            For y = 0 To 13
                milkings(x, y) = 0
            Next
        Next
        Return yearData

    End Function
    Function TopCow(ByVal milkings(,) As Single, ByVal NumCows As Integer) As Integer
        Dim total As Single = 0
        Dim max = -5
        For x = 0 To NumCows
            total = 0

            For y = 0 To 13
                total += milkings(x, y)
            Next

            If total > max Then
                max = total
                TopCow = x
            End If
        Next

        Return TopCow

    End Function
    Sub CowConcern(ByVal cows() As Integer, ByVal milkings(,) As Single, ByVal NumCows As Integer)
        Dim total As Single = 0
        Dim average As Single = 0
        Dim lowCows As String = ""
        Dim lowDay As Integer = 0
        For x = 0 To NumCows
            total = 0
            average = 0
            lowDay = 0
            For y = 0 To 6
                If milkings(x, (y * 2)) + milkings(x, ((y * 2) + 1)) < LOWYIELD Then
                    lowDay += 1
                End If
            Next

            If lowDay >= 4 Then
                lowCows &= cows(x) & ", "
            End If
        Next

        If lowCows = "" Then
            Console.WriteLine("All cows have exceeded minimum yield this week")
        Else
            Console.WriteLine("The following cows have low yield this week " & Mid(lowCows, 1, Len(lowCows) - 2) & ".")
        End If

    End Sub
    Function WeeklyTotal(ByVal milkings(,) As Single, ByVal NumCows As Integer) As Integer
        For x = 0 To NumCows
            For y = 0 To 13
                WeeklyTotal = WeeklyTotal + milkings(x, y)
            Next
        Next
        Return Math.Round(WeeklyTotal)
    End Function
    Function UpdateYield(ByVal cows() As Integer, ByVal milkings(,) As Single, ByVal newData As recording) As Single(,)
        Dim location As Integer
        For x = 0 To cows.Length - 1
            If cows(x) = newData.cowID Then
                location = x
            End If
        Next
        If milkings(location, newData.day * 2) <> 0 Then
            milkings(location, (newData.day * 2) + 1) = newData.yield
        Else
            milkings(location, newData.day * 2) = newData.yield

        End If

        Return milkings
    End Function
    Function TakeRecording(ByVal cows() As Integer, ByVal milkings(,) As Single) As recording
        Dim dayCheck As Integer
        Dim cowCheck As Integer
        Dim newVal As Single
        Console.WriteLine("Please Enter the cow ID")
        TakeRecording.cowID = Console.ReadLine
        For x = 0 To cows.Length
            If TakeRecording.cowID = cows(x) Then
                cowCheck = x
                Exit For
            End If
        Next
        Console.WriteLine("Please enter Day mon, tue, etc")
        TakeRecording.day = ConvertDay(Console.ReadLine)
        For x = 0 To 6
            If TakeRecording.day = x And milkings(cowCheck, TakeRecording.day * 2 + 1) <> 0 Then
                Console.WriteLine("You have already entered 2 milkings for this day")
                Exit Function
            End If
        Next
        Console.WriteLine("Please Enter the yield")
        TakeRecording.yield = Console.ReadLine
        newVal = (((TakeRecording.yield * 10) \ 1) / 10)
        Do While TakeRecording.yield < 0
            Console.WriteLine("Please enter a yield greater than or equal to 0. Please try again.")
            TakeRecording.yield = Console.ReadLine
        Loop
        Do While newVal <> TakeRecording.yield
            Console.WriteLine("Please enter a yield to one decimal place")
            TakeRecording.yield = Console.ReadLine
        Loop

        Return TakeRecording

    End Function
    Function ConvertDay(ByVal day As String) As String
        Dim num As Integer
        Do Until num = 1
            Select Case LCase(day)
                Case "mon"
                    Return 0
                    Exit Do
                Case "tue"
                    Return 1
                    Exit Do
                Case "wed"
                    Return 2
                    Exit Do
                Case "thu"
                    Return 3
                    Exit Do
                Case "fri"
                    Return 4
                    Exit Do
                Case "sat"
                    Return 5
                    Exit Do
                Case "sun"
                    Return 6
                    Exit Do
                Case Else
                    Console.WriteLine("Incorrect data, try again")
                    day = Console.ReadLine()
            End Select
        Loop
    End Function
    Sub displayCows(ByVal cows() As Integer)
        For x = 0 To cows.Length - 1
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
        Console.WriteLine("f. Kill Cow")
        Console.WriteLine("g. Add Cow")
        Console.WriteLine("x. Close System")
    End Sub
    Sub outputYearlyData(ByVal cows() As Integer, ByVal milkingData(,) As Integer)
        Dim counter As Integer = 0
        Dim sortAr(cows.Length - 1) As Integer
        Dim milkSortAr(cows.Length - 1, 52)
        Console.WriteLine()
        Console.WriteLine("   YEARLY WEEKLY TOTAL DATA")
        Console.Write("Cow ")
        For x = 0 To cows.Length - 1
            sortAr(x) = cows(x)
        Next
        System.Array.Sort(cows)
        For x = 0 To cows.Length - 1
            For y = 0 To cows.Length - 1
                If sortAr(x) = cows(y) Then
                    For z = 0 To 51
                        milkSortAr(y, z) = milkingData(x, z)
                    Next
                End If
            Next
        Next
        For x = 0 To cows.Length - 1
            Console.Write(cows(x) & " ")
        Next
        Console.WriteLine()

        For x = 0 To 51
            If x < 9 Then
                Console.Write("  " & x + 1 & " ")
            Else
                Console.Write(" " & x + 1 & " ")
            End If

            For f = 0 To cows.Length - 1
                Console.Write(milkSortAr(f, x) & " ")
                For z = 0 To 2 - Len(CStr(milkSortAr(f, x)))
                    Console.Write(" ")
                Next
            Next
            Console.WriteLine()
            If milkSortAr(0, x) = 0 Then
                counter += 1
            End If
            If cows.Length > 1 Then
                If milkSortAr(1, x) = 0 Then
                    counter += 1
                End If
            Else
                If milkSortAr(0, x) = 0 Then
                    counter += 1
                End If
            End If
            If counter = 2 Then
                Exit For
            End If
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
                Console.Write(milkingData(x, y))
                For z = 0 To 6 - Len(CStr(milkingData(x, y)))
                    Console.Write(" ")
                Next
            Next
            Console.WriteLine()
        Next
        Console.WriteLine()
        Console.WriteLine()
    End Sub
    Function inputCowIds(ByVal cows As Integer) As Integer()
        Dim newCowIds(cows) As Integer
        Dim counter As Integer = 0
        For x = 0 To cows
            newCowIds(x) = (Rnd() * 899) + 100
        Next
        For y = 0 To cows
            For z = 0 To cows
                If newCowIds(y) = newCowIds(z) Then
                    counter += 1
                    If counter >= 2 Then
                        newCowIds(y) = (Rnd() * 899) + 100
                    End If
                End If
                counter = 0
            Next
        Next
        Return newCowIds
    End Function
    Function KillCow(ByVal cows() As Integer, ByVal milkings(,) As Integer, ByVal death As Integer)
        Dim rand As Integer
        For x = 0 To cows.Length - 1
            If death = cows(x) Then
                For z = 0 To 51
                    If x = cows.Length - 1 Then
                        Exit For
                    End If
                    If x = cows.Length - 2 Then
                        milkings(x + 1, z) = 0
                    Else
                        milkings(x, z) = milkings(x + 1, z)
                    End If
                Next
            End If
        Next
        Randomize()
        rand = (Rnd() * 3 + 1)
        Console.WriteLine()
        Console.WriteLine("((___))
[ x x ]
 \   /
 (' ')
  (U)")
        Console.WriteLine()
        Select Case rand
            Case 1
                Console.WriteLine("Cow " & death & " has been shot behind the barn")
            Case 2
                Console.WriteLine("Cow " & death & " has been turned into a medium-rare Ribeye")
            Case 3
                Console.WriteLine("Cow " & death & " has been sent to a place, far away from here")
        End Select
        Console.WriteLine()
        Return milkings
    End Function
    Function AddCow(ByVal cows() As Integer, ByVal cowNum As Integer)
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

End Module