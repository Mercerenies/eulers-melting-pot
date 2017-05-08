
Function Pentagonal(N As Long) As Long
    Pentagonal = N * (3 * N - 1) / 2
End Function

Function IsPentagonal(N As Long) As Boolean
    Dim Sqrt As Double
    Sqrt = Math.Sqr(1 + 24 * N)
    IsPentagonal = (Sqrt = Int(Sqrt)) And (Sqrt Mod 6 = 5)
End Function

Sub Euler44()
    Dim Index As Long, Jndex As Long, IPent As Long, JPent As Long
    Index = 1
    While True
        For Jndex = Index - 1 To 1 Step -1
            IPent = Pentagonal(Index)
            JPent = Pentagonal(Jndex)
            If IsPentagonal(IPent + JPent) And IsPentagonal(IPent - JPent) Then
                Range("A1").Value = IPent - JPent
                Exit Sub
            End If
        Next Jndex
        Index = Index + 1
    Wend
End Sub
