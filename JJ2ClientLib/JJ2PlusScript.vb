Imports System
Imports JJ2ClientLib.JJ2

Namespace JJ2
    Public MustInherit Class JJ2PlusScript
        Implements IDisposable

        Public Property ScriptEnabled As Boolean = False
        Public Property ScriptModuleID As SByte = -1
        Public Property ScriptModuleIDUnsigned As Byte = &HFF
        Public Property Client As JJ2Client = Nothing

        Private _disposed As Boolean = False


        Sub New(ByVal client As JJ2Client, ByVal scriptModuleID As SByte)
            Initialize(client, scriptModuleID)
        End Sub

        Sub New(ByVal client As JJ2Client, ByVal scriptName As String)
            Dim scriptModuleID As Integer = client.GetScriptModuleID(scriptName)
            Initialize(client, If(scriptModuleID >= 0 AndAlso scriptModuleID <= SByte.MaxValue, CSByte(scriptModuleID), -1))
        End Sub

        Private Sub Initialize(ByVal client As JJ2Client, ByVal scriptModuleID As SByte)
            Me.Client = client
            Me.ScriptModuleID = scriptModuleID
            Me.ScriptModuleIDUnsigned = If(scriptModuleID < 0, CByte(CInt(scriptModuleID) And &HFF), CByte(scriptModuleID))

            If Me.Client IsNot Nothing AndAlso Me.ScriptModuleID >= 0 Then
                InitializeEvents()
                ScriptEnabled = True
            End If
        End Sub

        Private Sub InitializeEvents()
            AddHandler Client.Level_Initialized_Event, AddressOf OnLevelLoadEventHandler
            AddHandler Client.JJ2_Plus_Network_Stream_Data_Arrival, AddressOf OnReceiveEventHandler
        End Sub

        Private Sub DeinitializeEvents()
            If Client IsNot Nothing Then
                RemoveHandler Client.Level_Initialized_Event, AddressOf OnLevelLoadEventHandler
                RemoveHandler Client.JJ2_Plus_Network_Stream_Data_Arrival, AddressOf OnReceiveEventHandler
            End If
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub

        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposed Then
                If disposing Then
                    ' CRITICAL: Manually unsubscribe here using RemoveHandler
                    Close()
                    _Client = Nothing
                End If
                _disposed = True
            End If
        End Sub

        ''' <summary>
        ''' Turns off the class entirely, unsubscribes from events, and disables the script. After calling this method, the class will no longer respond to events or receive data.
        ''' </summary>
        Public Overridable Sub Close()
            DeinitializeEvents()
            ScriptEnabled = False
        End Sub

        Public Function SendJJ2PlusNetworkStream(ByVal streamData As Byte()) As Boolean
            Return Client.SendJJ2PlusNetworkStream(streamData, ScriptModuleIDUnsigned)
        End Function

        Public Function SendJJ2PlusNetworkStream(ByVal sw As jjStreamWritter) As Boolean
            Return Client.SendJJ2PlusNetworkStream(sw, ScriptModuleIDUnsigned)
        End Function

        Public Function QueueSendNetworkStream(ByVal sw As jjStreamWritter, Optional ByVal highPriority As Boolean = False) As Boolean
            Return Client.QueueJJ2PlusNetworkStream(sw, ScriptModuleIDUnsigned, highPriority)
        End Function

        Public Function QueueSendNetworkStream(ByVal streamData As Byte(), Optional ByVal highPriority As Boolean = False) As Boolean
            Return Client.QueueJJ2PlusNetworkStream(streamData, ScriptModuleIDUnsigned, highPriority)
        End Function



        Public Overridable Sub OnLevelLoad()

        End Sub

        Public Overridable Sub OnReceive(packetStream As jjStreamReader, packet As Byte(), length As Integer, offset As Integer)

        End Sub


        Private Sub OnLevelLoadEventHandler(ByVal levelName As String, ByVal yourName As String, ByVal yourID As Byte, ByVal yourSocketIndex As Byte, ByVal user As Object)
            If ScriptEnabled Then OnLevelLoad()
        End Sub

        Private Sub OnReceiveEventHandler(ByVal packet As Byte(), sourceID As Byte, packetStream As jjStreamReader, user As Object)
            'If ScriptEnabled Then ????
            If sourceID = Me.ScriptModuleID Then
                OnReceive(packetStream, packet, packet.Length, 0)
            End If
        End Sub

    End Class

End Namespace
