Public Class PlusGameplaySubpacketConstruct

    Public Shared Function FireBullet(gun As Byte, playerID As Byte) As Byte()
        Dim result(21 - 1) As Byte
        Dim subpacketID = CByte(&H10)
        result(0) = subpacketID

        'playerID is inside result(10) (bit2-bit6)

        Dim checksum As Int32 'MBY CHECKSUM IS AN INT16
        Array.Copy(BitConverter.GetBytes(checksum), 0, result, 8, 4)
        Return result
    End Function
End Class

Public Structure PlayerStatus
    Public Property SugarRush As Boolean
    Public Property Shield As Byte
    Public Property Shield2 As Byte '0=none, 1=water, ...
    Public Property Vx As Byte
    Public Property Vy As Byte

    Public Function GetPacket() As Byte

    End Function

    Const MAX_LOCAL_PLAYERS = 4
    Const PLAYER_ARRAY_SIZE_PLUS = 12
    Public Shared Function ConstructPacket(ByRef src As PlayerStatus(), Optional ByVal includedPlayers As Byte = &H1, Optional plus As Boolean = True)
        Dim result() As Byte = {}
        Dim numOfPlayers = 0
        For i = 0 To MAX_LOCAL_PLAYERS - 1
            If CBool(includedPlayers And CByte(2 ^ i)) Then
                numOfPlayers += 1
            End If
        Next
        If numOfPlayers <> src.Length Then
            Return result
        End If

        If plus Then
            ReDim result(5 + numOfPlayers * PLAYER_ARRAY_SIZE_PLUS - 1)
            result(2) = &H1
            result(4) = includedPlayers

            Dim playerStartIndex = 5
            For i = 0 To numOfPlayers - 1
                result(playerStartIndex + 1) = result(playerStartIndex + 1) Or ((src(i).Shield And CByte(&H7)) << 3)
                result(playerStartIndex + 1) = result(playerStartIndex + 1) Or If(src(i).SugarRush, &H2, 0)
                result(playerStartIndex + 9) = result(playerStartIndex + 9) Or CByte((src(i).Shield2 And CByte(&H3)) << 6)
                result(playerStartIndex + 10) = src(i).Vx
                result(playerStartIndex + 11) = src(i).Vy

                playerStartIndex += PLAYER_ARRAY_SIZE_PLUS
            Next



        End If
        Return result
    End Function
End Structure
