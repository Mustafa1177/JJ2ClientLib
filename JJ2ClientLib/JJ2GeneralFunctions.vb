Imports JJ2ClientLib.JJ2
Namespace JJ2
    Public Class JJ2GeneralFunctions
        Public Shared Function GetGameModeShortString(gameMode As Byte, customMode As Byte) As String
            If customMode = 0 Then
                Select Case gameMode
                    Case 0
                        Return "SP"
                    Case 1
                        Return "Coop"
                    Case 2
                        Return "Battle"
                    Case 3
                        Return "Race"
                    Case 4
                        Return "Treasure"
                    Case 5
                        Return "CTF"
                    Case Else
                        Return ""
                End Select
            Else
                Select Case customMode
                    Case 1
                        Return "RT"
                    Case 2
                        Return "LRS"
                    Case 3
                        Return "XLRS"
                    Case 4
                        Return "PEST"
                    Case 11
                        Return "TB"
                    Case 12
                        Return "JB"
                    Case 13
                        Return "DCTF"
                    Case 14
                        Return "FR"
                    Case 15
                        Return "TLRS"
                    Case 16
                        Return "Dom"
                    Case 17
                        Return "HH"
                    Case Else
                        Return ""
                End Select
            End If
        End Function

        Public Shared Function GetUnformattedString(value As String, Optional uncolor As Boolean = True) As String
            '§2
            Dim result = New String(value)
            result = If(uncolor, result.Replace("|"c, ""), result)
            Dim charIndex = result.IndexOf("§"c)
            While charIndex >= 0
                result = result.Remove(charIndex, If(result.Length > charIndex + 1, 2, 1))
                charIndex = result.IndexOf("§"c)
            End While
            Return result
        End Function

        Public Shared Function GetUnformattedName(value As String) As String
            Return value.Replace("|"c, "").Replace("*"c, "")
        End Function

    End Class
End Namespace
