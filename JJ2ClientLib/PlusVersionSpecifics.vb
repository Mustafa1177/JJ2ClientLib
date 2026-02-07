Imports System.Text

Namespace JJ2.PlusVersionSpecifics

    Public Structure Pckt0x12ReadResult
        Public TotNumOfPlayers As Byte
        Public UpdatedClientsIndices As Byte()
        Public UpdatedPlayersIDs As Byte()
    End Structure

    Public MustInherit Class PlusVerHandler
        Public Property Client As JJ2Client 'Owner/Parent

        Public Sub New(ByVal owner As JJ2Client)
            Me.Client = owner
        End Sub

        Public Overridable Function ReadPacket0x12(ByVal recv As Byte(), ByVal packStartingIndex As Integer) As Pckt0x12ReadResult
        End Function

    End Class

    Public Class PlusVerHandler0500 'JJ2+ v5.0
        Inherits PlusVerHandler

        Public Sub New(ByVal owner As JJ2Client)
            MyBase.New(owner)
        End Sub

        Overrides Function ReadPacket0x12(ByVal recv As Byte(), ByVal packStartingIndex As Integer) As Pckt0x12ReadResult
            With Client
                Dim totNumOfPlayers As Byte = recv(packStartingIndex + 2)
                Dim updatedClientsIndices As New List(Of Byte)
                Dim updatedPlayersIDs As New List(Of Byte)
                Dim numOfPlayersDoneWithInSock(32) As Byte
                If totNumOfPlayers <> 0 Then
                    Dim playerArrStartingIndex As Int16 = packStartingIndex + 3

                    For workingOnPlayer As Byte = 0 To totNumOfPlayers - 1
                        Dim playerSocketIndex As Byte = recv(playerArrStartingIndex)
                        Dim playerNumber As Byte = recv(playerArrStartingIndex + 1)
                        Dim charTeam As Byte = recv(playerArrStartingIndex + 2)

                        If playerSocketIndex < .JJ2ClientsSockInfo.Length And playerNumber < .Players.Length Then
                            updatedClientsIndices.Add(playerSocketIndex)
                            updatedPlayersIDs.Add(playerNumber)

                            'init socket info
                            If .JJ2ClientsSockInfo(playerSocketIndex) Is Nothing Then
                                .JJ2ClientsSockInfo(playerSocketIndex) = New JJ2SocketInfo
                            Else
                                '    JJ2ClientsSockInfo(playerSocketIndex).reset()
                            End If
                            .JJ2ClientsSockInfo(playerSocketIndex).PlayerID(numOfPlayersDoneWithInSock(playerSocketIndex)) = playerNumber
                            numOfPlayersDoneWithInSock(playerSocketIndex) += 1
                            .ActiveClients(playerSocketIndex) = True

                            'init player
                            If .Players(playerNumber) Is Nothing Then
                                .Players(playerNumber) = New JJ2Player(playerSocketIndex, charTeam Mod &H10, recv(playerArrStartingIndex + 3), .JJ2ClientsSockInfo(playerSocketIndex))
                            Else
                                '   Players(playerNumber).reset()
                                .Players(playerNumber).Update(playerSocketIndex, charTeam Mod &H10, recv(playerArrStartingIndex + 3), .JJ2ClientsSockInfo(playerSocketIndex))
                            End If
                            .Players(playerNumber).ClearStats(.PlusVersion)

                            Array.Copy(recv, playerArrStartingIndex + 4, .Players(playerNumber).Color, 0, 4)
                            Dim playerNameLength As Byte = 0
                            Dim whileHelper As UShort = playerArrStartingIndex + 14
                            While recv(whileHelper) <> &H0
                                playerNameLength += 1
                                whileHelper += 1
                                If recv.Length = whileHelper Then
                                    Exit While
                                End If
                            End While
                            .Players(playerNumber).Name = Encoding.UTF7.GetString(recv, (playerArrStartingIndex + 14), playerNameLength)
                            Debug.WriteLine("[" & .Players(playerNumber).Name & "] " & recv(playerArrStartingIndex + 14))
                            'assign NumOfPlayers
                            Dim tempNumOfPlayersFromClient As Byte = 0
                            For Each b As Byte In .JJ2ClientsSockInfo(playerSocketIndex).PlayerID
                                If b <> &HFF Then
                                    tempNumOfPlayersFromClient += 1
                                End If
                            Next
                            .JJ2ClientsSockInfo(playerSocketIndex).NumOfPlayers = tempNumOfPlayersFromClient

                            playerArrStartingIndex += 15 + playerNameLength
                            'MsgBox(Players(playerNumber).Name & " in server")
                        End If
                    Next
                End If
                Return New Pckt0x12ReadResult() With {.TotNumOfPlayers = totNumOfPlayers, .UpdatedClientsIndices = updatedClientsIndices.ToArray, .UpdatedPlayersIDs = updatedPlayersIDs.ToArray}
                'RaiseEvent Players_List_Update_Event(updatedPlayersIDs.ToArray, updatedClientsIndices.ToArray, UserData)

            End With
        End Function

    End Class

    Public Class PlusVerHandler0606 'JJ2+ v5.0
        Inherits PlusVerHandler0500

        Public Sub New(ByVal owner As JJ2Client)
            MyBase.New(owner)
        End Sub

        Overrides Function ReadPacket0x12(ByVal recv As Byte(), ByVal packStartingIndex As Integer) As Pckt0x12ReadResult
            With Client
                Dim totNumOfPlayers As Byte = recv(packStartingIndex + 2)
                Dim updatedClientsIndices As New List(Of Byte)
                Dim updatedPlayersIDs As New List(Of Byte)
                Dim numOfPlayersDoneWithInSock(32) As Byte
                If totNumOfPlayers <> 0 Then
                    Dim playerArrStartingIndex As Int16 = packStartingIndex + 3

                    For workingOnPlayer As Byte = 0 To totNumOfPlayers - 1
                        Dim playerSocketIndex As Byte = recv(playerArrStartingIndex)
                        Dim playerNumber As Byte = recv(playerArrStartingIndex + 1)
                        Dim charTeam As Byte = recv(playerArrStartingIndex + 2)

                        If playerSocketIndex < .JJ2ClientsSockInfo.Length And playerNumber < .Players.Length Then
                            updatedClientsIndices.Add(playerSocketIndex)
                            updatedPlayersIDs.Add(playerNumber)

                            'init socket info
                            If .JJ2ClientsSockInfo(playerSocketIndex) Is Nothing Then
                                .JJ2ClientsSockInfo(playerSocketIndex) = New JJ2SocketInfo
                            Else
                                '    JJ2ClientsSockInfo(playerSocketIndex).reset()
                            End If
                            .JJ2ClientsSockInfo(playerSocketIndex).PlayerID(numOfPlayersDoneWithInSock(playerSocketIndex)) = playerNumber
                            numOfPlayersDoneWithInSock(playerSocketIndex) += 1
                            .ActiveClients(playerSocketIndex) = True

                            'init player
                            If .Players(playerNumber) Is Nothing Then
                                .Players(playerNumber) = New JJ2Player(playerSocketIndex, charTeam Mod &H10, recv(playerArrStartingIndex + 3), .JJ2ClientsSockInfo(playerSocketIndex))
                            Else
                                '   Players(playerNumber).reset()
                                .Players(playerNumber).Update(playerSocketIndex, charTeam Mod &H10, recv(playerArrStartingIndex + 3), .JJ2ClientsSockInfo(playerSocketIndex))
                            End If
                            .Players(playerNumber).ClearStats(.PlusVersion)

                            Array.Copy(recv, playerArrStartingIndex + 4, .Players(playerNumber).Color, 0, 4)
                            .Players(playerNumber).IconID = recv(playerArrStartingIndex + 14)
                            Dim playerNameLength As Byte = 0
                        Dim whileHelper As UShort = playerArrStartingIndex + 15
                        While recv(whileHelper) <> &H0
                                playerNameLength += 1
                                whileHelper += 1
                                If recv.Length = whileHelper Then
                                    Exit While
                                End If
                            End While
                            .Players(playerNumber).Name = Encoding.UTF7.GetString(recv, (playerArrStartingIndex + 15), playerNameLength)
                            Debug.WriteLine("[" & .Players(playerNumber).Name & "] " & recv(playerArrStartingIndex + 14) & " " & .Players(playerNumber).IconID)
                            'assign NumOfPlayers
                            Dim tempNumOfPlayersFromClient As Byte = 0
                            For Each b As Byte In .JJ2ClientsSockInfo(playerSocketIndex).PlayerID
                                If b <> &HFF Then
                                    tempNumOfPlayersFromClient += 1
                                End If
                            Next
                            .JJ2ClientsSockInfo(playerSocketIndex).NumOfPlayers = tempNumOfPlayersFromClient

                            playerArrStartingIndex += 15 + playerNameLength
                            'MsgBox(Players(playerNumber).Name & " in server")
                        End If
                    Next
                End If
                Return New Pckt0x12ReadResult() With {.TotNumOfPlayers = totNumOfPlayers, .UpdatedClientsIndices = updatedClientsIndices.ToArray, .UpdatedPlayersIDs = updatedPlayersIDs.ToArray}
                'RaiseEvent Players_List_Update_Event(updatedPlayersIDs.ToArray, updatedClientsIndices.ToArray, UserData)

            End With
        End Function

    End Class

End Namespace
