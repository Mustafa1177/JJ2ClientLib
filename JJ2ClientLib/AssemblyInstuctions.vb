Public Class AssemblyInstuctions
    Public Shared Sub sar(ByRef op1 As Int32, ByVal op2 As Byte)
        For i = 1 To op2
            op1 /= 2
        Next
    End Sub
End Class
