Imports System.Text
Imports JJ2ClientLib.JJ2

Public Class PlusVersionSpecifics

    Public MustInherit Class PlusVerHandler
        Public Property Client As JJ2Client 'Owner/Parent

        Public Overridable Function ReadPacket0x12(ByVal recv As Byte(), ByVal packStartingIndex As Integer)
        End Function

    End Class

    Public Class PlusVerHandler0500
        Inherits PlusVerHandler

        Overrides Function ReadPacket0x12(ByVal recv As Byte(), ByVal packStartingIndex As Integer)
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
                'RaiseEvent Players_List_Update_Event(updatedPlayersIDs.ToArray, updatedClientsIndices.ToArray, UserData)

            End With
        End Function

    End Class

End Class
