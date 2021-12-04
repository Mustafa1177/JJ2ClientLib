Namespace JJ2
    Public Class JJ2SocketInfo 'or client
        Const MAXSPLITSCREENPLAYERS = 8
        Public Property NumOfPlayers As Byte = 0
        Public Property IsAdmin As Boolean = 0
        Public Property isSpectating As Byte = 0
        Public Property AntiSpamCount As UShort = 0
        Public playerID(MAXSPLITSCREENPLAYERS - 1) As Byte
        Public IP As String = ""
        Public Sub reset()
            NumOfPlayers = 0
            For i As Byte = 0 To playerID.Length - 1
                playerID(i) = &HFF
            Next
            IsAdmin = 0
            isSpectating = 0
            AntiSpamCount = 0
            IP = ""
        End Sub
        Sub New()
            For i = 0 To playerID.Length - 1
                playerID(i) = &HFF
            Next
        End Sub
    End Class

    Public Class JJ2Player
        Public Property ClientID As Byte = &HFF
        Public Property Character As JJ2_Character = 0
        Public Property Team As Byte = &HFF
        Public Property Name As String = ""
        Public Property latency As Int16 = -1
        Public Color(3) As Byte
        Public Sub reset()
            ClientID = &HFF
            Team = &HFF
            latency = -1
        End Sub
        Public Sub Update(ByVal playersocketIndex As Byte, ByVal playerCharacter As Byte, ByVal playerTeam As Byte)
            ClientID = playersocketIndex
            Character = playerCharacter
            Team = playerTeam
        End Sub
        Sub New(ByVal playersocketIndex As Byte, ByVal playerCharacter As Byte, ByVal playerTeam As Byte)
            ClientID = playersocketIndex
            Character = playerCharacter
            Team = playerTeam
        End Sub
    End Class

    Public Structure JJ2PlusGameSettings
        Public Property PlusOnly As Boolean
        Public Property FriendlyFire As Boolean
        Public Property NoMovement As Boolean
        Public Property NoBliking As Boolean
        Public Property ReadyCommandEnabled As Boolean
        Public Property FireBall As Boolean
        Public Property WallJumping As Boolean
    End Structure
End Namespace
