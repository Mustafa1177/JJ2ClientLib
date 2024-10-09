Namespace JJ2
    Public Class jjStreamReader

        Dim _buffer As Byte() = {}
        Dim _leangth As Integer = 0
        Dim _nextOffset As Integer = 0
        Public ReadOnly Property Length
            Get
                Return _leangth
            End Get
        End Property

        Sub New(Optional ByVal mem As Byte() = Nothing, Optional offset As Integer = 0, Optional length As Integer = 0)
            If mem IsNot Nothing Then
                If offset = 0 AndAlso (length = 0 OrElse length = mem.Length) Then
                    _buffer = mem
                    Me._leangth = _buffer.Length
                Else
                    length = mem.Length - offset
                    ReDim Me._buffer(length - 1)
                    Array.Copy(mem, offset, Me._buffer, 0, length)
                    Me._leangth = length
                End If
            End If
    End Sub

        Sub clear()
            _buffer = {}
            _leangth = 0
            _nextOffset = 0
        End Sub

        Function getSize() As Integer
            Return _leangth
        End Function

        Public Function discard(count As Integer) As Boolean
            If count <= _leangth - _nextOffset Then
                _nextOffset += count
                Return True
            Else
                _nextOffset = _leangth
                Return False
            End If
        End Function

        Public Sub pop(ByRef value As Boolean)
            value = CBool(_buffer(_nextOffset))
            _nextOffset += 1
        End Sub

        Public Sub pop(ByRef value As Byte)
            value = _buffer(_nextOffset)
            _nextOffset += 1
        End Sub

        Public Sub pop(ByRef value As SByte)
            value = If(_buffer(_nextOffset) < 128, _buffer(_nextOffset), _buffer(_nextOffset) - 256)
            _nextOffset += 1
        End Sub

        Public Sub pop(ByRef value As UShort)
            value = BitConverter.ToUInt16(_buffer, _nextOffset)
            _nextOffset += 2
        End Sub

        Public Sub pop(ByRef value As Short)
            value = BitConverter.ToInt16(_buffer, _nextOffset)
            _nextOffset += 2
        End Sub

        Public Sub pop(ByRef value As UInteger)
            value = BitConverter.ToUInt32(_buffer, _nextOffset)
            _nextOffset += 4
        End Sub

        Public Sub pop(ByRef value As Integer)
            value = BitConverter.ToInt32(_buffer, _nextOffset)
            _nextOffset += 4
        End Sub


        Public Sub pop(ByRef value As ULong)
            value = BitConverter.ToUInt64(_buffer, _nextOffset)
            _nextOffset += 8
        End Sub

        Public Sub pop(ByRef value As Long)
            value = BitConverter.ToInt64(_buffer, _nextOffset)
            _nextOffset += 8
        End Sub

        Public Sub pop(ByRef value As Single)
            value = BitConverter.ToSingle(_buffer, _nextOffset)
            _nextOffset += 4
        End Sub

        Public Sub pop(ByRef value As Double)
            value = BitConverter.ToDouble(_buffer, _nextOffset)
            _nextOffset += 8
        End Sub

        Public Sub pop(ByRef value As String)
            Dim len = BitConverter.ToUInt32(_buffer, _nextOffset)
            _nextOffset += 4
            System.Text.Encoding.UTF7.GetString(_buffer, _nextOffset, len)
            _nextOffset += len
        End Sub

        Public Function pop2(ByRef value As Object) As Boolean
            Dim result = False
            Select Case value.GetType
                Case GetType(Byte)
                    Me._nextOffset += 1
                Case GetType(SByte)
                    Me._nextOffset += 1
                Case GetType(UShort)
                    Me._nextOffset += 2
                Case GetType(Short)
                    Me._nextOffset += 2
                Case GetType(UInteger)
                    Me._nextOffset += 4
                Case GetType(Integer)
                    Me._nextOffset += 4
                Case GetType(Single)
                    Me._nextOffset += 4
                Case GetType(ULong)
                    Me._nextOffset += 8
                Case GetType(Long)
                    Me._nextOffset += 8
                Case GetType(Double)
                    Me._nextOffset += 8
                Case GetType(String)
            End Select

            Return result
        End Function

    End Class
End Namespace
