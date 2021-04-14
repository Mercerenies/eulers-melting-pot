Imports System

Public Class Problem122

    Dim BestAnswers(199) as Long

    Public Sub New()
        Dim Index As Long
        For Index = 0 To 199
            BestAnswers(Index) = 300
        Next Index
    End Sub

    Public Sub Recurse(ByVal Visited() As Long, ByVal Recent As Long)
        Dim Index As Long, Jndex As Long, Kndex As Long
        If Recent > 200 Then
            Exit Sub
        End If
        BestAnswers(Recent - 1) = Math.Min(BestAnswers(Recent - 1), Visited.Length - 1)
        For Each Index In Visited
            For Each Jndex In Visited
                If Index <= Jndex Then
                    Dim Okay As Boolean = True
                    For Each Kndex In Visited
                        If Kndex = Index + Jndex Then
                            Okay = False
                        End If
                    Next Kndex
                    If Okay Then
                        If Index + Jndex > Recent And Index + Jndex <= 200 Then
                            If Visited.Length - 1 < BestAnswers(Index + Jndex - 1) Then
                                Dim NewVisited(Visited.Length) as Long
                                NewVisited(0) = Index + Jndex
                                For Kndex = 0 To Visited.Length - 1
                                    NewVisited(Kndex + 1) = Visited(Kndex)
                                Next Kndex
                                Recurse(NewVisited, Index + Jndex)
                            End If
                        End If
                    End If
                End If
            Next Jndex
        Next Index
    End Sub

    Public Function Sum()
        Dim Element As Long
        Sum = 0
        For Each Element In BestAnswers
            Sum += Element
        Next Element
    End Function

    Public Shared Sub Main()
        Dim Problem As Problem122 = New Problem122()
        Dim StartingPoint() As Long = {1}
        Problem.Recurse(StartingPoint, 1)
        Console.WriteLine(Problem.Sum())
    End Sub

End Class
