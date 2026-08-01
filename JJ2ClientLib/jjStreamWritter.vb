Namespace JJ2
    Public Class jjStreamWritter
        Dim _buffer As List(Of Byte)

        Public ReadOnly Property Length
            Get
                Return _buffer.Count
            End Get
        End Property

        Sub New()
            _buffer = New List(Of Byte)()
        End Sub

        Sub New(capacity As Integer)
            _buffer = New List(Of Byte)(capacity)
        End Sub

        Sub clear()
            _buffer.Clear()
        End Sub

        Function getSize() As Integer
            Return _buffer.Count
        End Function

        Public Function discard(count As Integer) As Boolean
            Return False
        End Function

        Public Function ToArray() As Byte()
            Return _buffer.ToArray
        End Function

        Public Sub push(value As Boolean)
            _buffer.Add(If(value, CByte(1), CByte(0)))
        End Sub

        Public Sub push(value As Byte)
            _buffer.Add(value)
        End Sub

        Public Sub push(value As SByte)
            ' If(_buffer(_nextOffset) < 128, _buffer(_nextOffset), _buffer(_nextOffset) - 256)
            _buffer.Add(BitConverter.GetBytes(value)(0))
        End Sub

        Public Sub push(value As UShort)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As Short)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As UInteger)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As Integer)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As ULong)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As Long)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As Single)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As Double)
            _buffer.AddRange(BitConverter.GetBytes(value))
        End Sub

        Public Sub push(value As String)
            _buffer.AddRange(BitConverter.GetBytes(CUInt(value.Length)))
            _buffer.AddRange(System.Text.Encoding.ASCII.GetBytes(value))
        End Sub

    End Class
End Namespace
