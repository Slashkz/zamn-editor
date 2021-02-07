Public Class ZAMNCompress

    Shared bitReader As BitReader

    Shared putbuf As UShort
    Shared putlen As Byte
    Shared result As List(Of Byte)
    '   LZSS Parameters
    Const N = 4096 'Sliding Window Length
    Const F = 60 'Length of String
    Const THRESHOLD = 2
    Const NODENIL = N 'End of tree's node

    Const Max = 128

    Shared text_buf(N - F - 1) As Byte
    Shared match_position As Integer
    Shared match_length As Integer

    'Huffman Parameters
    Const N_CHAR = (256 - THRESHOLD + F)
    Const T = (N_CHAR * 2 - 1)
    Const R = (T - 1)
    Const MAX_FREQ = &H8000

    Structure Node
        Dim Dad As Integer
        Dim LSon As Integer
        Dim RSon As Integer
    End Structure

    Shared p_len As Byte() = {
         &H3, &H4, &H4, &H4, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H6, &H6, &H6, &H6,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H8, &H8, &H8, &H8, &H8, &H8, &H8, &H8,
         &H8, &H8, &H8, &H8, &H8, &H8, &H8, &H8
    }

    Shared p_code As Byte() = {
         &H0, &H20, &H30, &H40, &H50, &H58, &H60, &H68,
         &H70, &H78, &H80, &H88, &H90, &H94, &H98, &H9C,
         &HA0, &HA4, &HA8, &HAC, &HB0, &HB4, &HB8, &HBC,
         &HC0, &HC2, &HC4, &HC6, &HC8, &HCA, &HCC, &HCE,
         &HD0, &HD2, &HD4, &HD6, &HD8, &HDA, &HDC, &HDE,
         &HE0, &HE2, &HE4, &HE6, &HE8, &HEA, &HEC, &HEE,
         &HF0, &HF1, &HF2, &HF3, &HF4, &HF5, &HF6, &HF7,
         &HF8, &HF9, &HFA, &HFB, &HFC, &HFD, &HFE, &HFF
    }

    Shared d_code As Byte() = {
         &H0, &H0, &H0, &H0, &H0, &H0, &H0, &H0,
         &H0, &H0, &H0, &H0, &H0, &H0, &H0, &H0,
         &H0, &H0, &H0, &H0, &H0, &H0, &H0, &H0,
         &H0, &H0, &H0, &H0, &H0, &H0, &H0, &H0,
         &H1, &H1, &H1, &H1, &H1, &H1, &H1, &H1,
         &H1, &H1, &H1, &H1, &H1, &H1, &H1, &H1,
         &H2, &H2, &H2, &H2, &H2, &H2, &H2, &H2,
         &H2, &H2, &H2, &H2, &H2, &H2, &H2, &H2,
         &H3, &H3, &H3, &H3, &H3, &H3, &H3, &H3,
         &H3, &H3, &H3, &H3, &H3, &H3, &H3, &H3,
         &H4, &H4, &H4, &H4, &H4, &H4, &H4, &H4,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H8, &H8, &H8, &H8, &H8, &H8, &H8, &H8,
         &H9, &H9, &H9, &H9, &H9, &H9, &H9, &H9,
         &HA, &HA, &HA, &HA, &HA, &HA, &HA, &HA,
         &HB, &HB, &HB, &HB, &HB, &HB, &HB, &HB,
         &HC, &HC, &HC, &HC, &HD, &HD, &HD, &HD,
         &HE, &HE, &HE, &HE, &HF, &HF, &HF, &HF,
         &H10, &H10, &H10, &H10, &H11, &H11, &H11, &H11,
         &H12, &H12, &H12, &H12, &H13, &H13, &H13, &H13,
         &H14, &H14, &H14, &H14, &H15, &H15, &H15, &H15,
         &H16, &H16, &H16, &H16, &H17, &H17, &H17, &H17,
         &H18, &H18, &H19, &H19, &H1A, &H1A, &H1B, &H1B,
         &H1C, &H1C, &H1D, &H1D, &H1E, &H1E, &H1F, &H1F,
         &H20, &H20, &H21, &H21, &H22, &H22, &H23, &H23,
         &H24, &H24, &H25, &H25, &H26, &H26, &H27, &H27,
         &H28, &H28, &H29, &H29, &H2A, &H2A, &H2B, &H2B,
         &H2C, &H2C, &H2D, &H2D, &H2E, &H2E, &H2F, &H2F,
         &H30, &H31, &H32, &H33, &H34, &H35, &H36, &H37,
         &H38, &H39, &H3A, &H3B, &H3C, &H3D, &H3E, &H3F
        }
    Shared d_len As Byte() = {
         &H3, &H3, &H3, &H3, &H3, &H3, &H3, &H3,
         &H3, &H3, &H3, &H3, &H3, &H3, &H3, &H3,
         &H3, &H3, &H3, &H3, &H3, &H3, &H3, &H3,
         &H3, &H3, &H3, &H3, &H3, &H3, &H3, &H3,
         &H4, &H4, &H4, &H4, &H4, &H4, &H4, &H4,
         &H4, &H4, &H4, &H4, &H4, &H4, &H4, &H4,
         &H4, &H4, &H4, &H4, &H4, &H4, &H4, &H4,
         &H4, &H4, &H4, &H4, &H4, &H4, &H4, &H4,
         &H4, &H4, &H4, &H4, &H4, &H4, &H4, &H4,
         &H4, &H4, &H4, &H4, &H4, &H4, &H4, &H4,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H5, &H5, &H5, &H5, &H5, &H5, &H5, &H5,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H6, &H6, &H6, &H6, &H6, &H6, &H6, &H6,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H7, &H7, &H7, &H7, &H7, &H7, &H7, &H7,
         &H8, &H8, &H8, &H8, &H8, &H8, &H8, &H8,
         &H8, &H8, &H8, &H8, &H8, &H8, &H8, &H8
        }

    Shared freq(T + 1) As UInteger 'cumulative freq table
    Shared prnt(T + N_CHAR) As UShort
    Shared son(T) As UShort
    Shared Tree(N + 256) As Node

    Shared getlen As Byte = 0
    Shared getbuf As UInteger = 0

    Private Shared Sub PutCode(ByVal l As Integer, ByVal c As Integer)
        putbuf = putbuf Or (c >> putlen)
        putlen += l
        If putlen >= 8 Then
            result.Add((putbuf >> 8) And &HFF)
            putlen -= 8
            If putlen >= 8 Then
                putlen -= 8
                result.Add(putbuf And &HFF)
                putbuf = (c << (l - putlen)) And &HFFFF
            Else
                putbuf <<= 8
            End If
        End If
    End Sub

    Private Shared Sub InitTree()
        Dim i As Integer
        For i = N + 1 To N + 256
            Tree(i).LSon = NODENIL
            Tree(i).RSon = NODENIL
        Next
        For i = 0 To N - 1
            Tree(i).Dad = NODENIL

        Next
    End Sub

    '{-initialize freq tree}
    Private Shared Sub StartHuff()
        Dim i, j As UShort

        For i = 0 To N_CHAR - 1
            freq(i) = 1
            son(i) = i + T
            prnt(i + T) = i
        Next
        i = 0
        j = N_CHAR
        While j <= R
            freq(j) = freq(i) + freq(i + 1)
            son(j) = i
            prnt(i) = j
            prnt(i + 1) = j
            i += 2
            j += 1
        End While
        freq(T) = &HFFFF
        prnt(R) = 0
    End Sub

    '{-reconstruct freq tree }
    Private Shared Sub reconst()

        Dim i, j, k, f, l As UShort
        '  {-halven cumulative freq for leaf nodes}
        j = 0
        For i = 0 To T - 1
            If son(i) >= T Then
                freq(j) = (freq(i) + 1) >> 1
                son(j) = son(i)
                j += 1
            End If
        Next
        ' {-make a tree : first, connect children nodes}
        i = 0
        For j = N_CHAR To T - 1
            k = i + 1
            f = freq(i) + freq(k)
            freq(j) = f
            k = j - 1
            While (f < freq(k))
                k -= 1
            End While
            k += 1
            l = (j - k) * 2
            System.Array.Copy(freq, k, freq, k + 1, l)
            freq(k) = f
            System.Array.Copy(son, k, son, k + 1, l)
            son(k) = i
            i += 2
        Next
        '  {-connect parent nodes}
        For i = 0 To T - 1
            k = son(i)
            prnt(k) = i
            If k < T Then
                prnt(k + 1) = i
            End If
        Next
    End Sub

    '{-update freq tree}
    Private Shared Sub update(ByVal c As UShort)

        Dim i, j, k, l As UShort

        If freq(R) = MAX_FREQ Then
            reconst()
        End If
        c = prnt(c + T)
        Do
            freq(c) += 1
            k = freq(c)
            ' {-swap nodes to keep the tree freq-ordered}
            l = c + 1
            If (k > freq(l)) Then
                While k > freq(l + 1)
                    l += 1
                End While
                freq(c) = freq(l)
                freq(l) = k

                i = son(c)
                prnt(i) = l
                If (i < T) Then
                    prnt(i + 1) = l
                End If
                j = son(l)
                son(l) = i
                prnt(j) = c
                If j < T Then
                    prnt(j + 1) = c
                End If
                son(c) = j
                c = l
            End If
            c = prnt(c)
        Loop Until c = 0 ' do it until reaching the root

    End Sub


    Private Shared Sub EncodeChar(ByVal c As UShort)
        Dim code, len, k As UShort
        code = 0
        len = 0
        k = prnt(c + T)
        '{-search connections from leaf node to the root}
        Do
            code = code >> 1
            '{-if node's address is odd, output 1 else output 0}
            If (k And 1) > 0 Then
                code += &H8000
            End If
            len += 1
            k = prnt(k)
        Loop Until k = R
        PutCode(len, code)
        update(c)
    End Sub

    Private Shared Sub EncodePosition(ByVal c As Integer)
        Dim i As Integer
        '//-output upper 6 bits with encoding
        i = c >> 6
        PutCode(p_len(i), CUShort(p_code(i)) << 8)
        '  //-output lower 6 bits directly
        PutCode(6, (c And &H3F) << 10)
    End Sub

    Private Shared Sub EncodeEnd()
        If putlen > 0 Then
            result.Add((putbuf >> 8) And &HFF)
        End If
    End Sub

    Private Shared Function DecodePosition() As UShort
        Dim i, j, c As UShort
        'decode upper 6 bits from given table
        i = bitReader.Read(8)
        c = d_code(i)
        c <<= 6
        j = d_len(i)
        'input lower 6 bits directly
        j -= 2
        While j > 0
            j -= 1
            i = (i << 1) Or bitReader.Read
        End While
        Return c Or (i And &H3F)
    End Function

    Private Shared Function DecodeChar() As UShort
        Dim c As UShort
        c = son(R)
        'start searching tree from the root to leaves.
        'Choose Node #(son[]) if input bit = 0
        'Else choose #(son[]+1) (input bit = 1)
        While c < T
            c = son(c + bitReader.Read)
        End While
        c -= T
        update(c)
        Return c
    End Function

    Public Shared Function Decompress(ByVal s As IO.Stream) As Byte()
        Dim result As New List(Of Byte)
        Dim dict(&HFFF) As Byte
        Dim bytesLeft As Integer = s.ReadByte * &H100 + s.ReadByte 'First 2 bytes are the compressed size
        Dim writeDictPos As Integer = N - F 'The dictionary starts at &HFC4 for some reason
        Dim readDictPos As Integer
        Dim bitsLeft As Integer = 0
        Dim buffer(bytesLeft) As Byte 'Buffer for compressed data
        bitReader = New BitReader(buffer)
        s.Read(buffer, 0, bytesLeft)

        For i As Integer = 0 To writeDictPos - 1
            dict(i) = 32
        Next
        StartHuff()
        Dim count As Integer = 0
        Dim c As UShort
        Dim j, k As UShort
        While count < bytesLeft
            c = DecodeChar()
            If c < 256 Then
                result.Add(c And &HFF) 'And put it in the result
                dict(writeDictPos) = c 'And the dictionary
                writeDictPos = (writeDictPos + 1) And &HFFF 'Increment the dictionary index, but not over &HFFF
                count += 1
            Else
                readDictPos = (writeDictPos - DecodePosition() - 1) And &HFFF
                j = c - 255 + THRESHOLD
                For k = 0 To j - 1
                    c = dict((readDictPos + k) And &HFFF)
                    result.Add(c)
                    dict(writeDictPos) = c
                    writeDictPos = (writeDictPos + 1) And &HFFF 'Increment the dictionary index, but not over &HFFF
                    count += 1
                Next
            End If
        End While

        Return result.ToArray()
    End Function

    Private Shared Function ReadNext(ByVal s As IO.Stream, ByRef result As Integer, ByRef bytesLeft As Integer) As Boolean
        If bytesLeft = 0 Then Return True
        bytesLeft -= 1
        result = s.ReadByte()
        Return False
    End Function

    Public Shared Function Compress(ByVal data As Byte()) As Byte()
        result = New List(Of Byte)(data.Length \ 2) 'Making the default size be half that of the original data
        Dim dataIndex As Integer = N - F
        Dim fr(Max) As Integer
        Dim p(Max) As Integer
        Dim k, m As Byte
        putbuf = 0
        putlen = 0
        StartHuff()
        ReDim text_buf(data.Length + text_buf.Length)
        data.CopyTo(text_buf, dataIndex)
        For i As Integer = 0 To dataIndex - 1
            text_buf(i) = 32
        Next
        result.Add((data.Length >> 8) And &HFF)
        result.Add(data.Length And &HFF)

        Do Until dataIndex = text_buf.Length
            If FindeMatches(dataIndex) Then
                fr(0) = dataIndex
                fr(1) = dataIndex + 1
                m = 2
                While True
                    p(m - 2) = match_position
                    fr(m) = fr(m - 2) + match_length
                    If (fr(m) < fr(m - 1) + 2) Then
                        Exit While
                    End If
                    m += 1
                    If ((m + 1) = Max) Then
                        Exit While
                    End If
                    dataIndex = fr(m - 2)
                    FindeMatches(dataIndex)
                End While

                If (m And 1) = 0 Then
                    EncodeChar(text_buf(fr(0)))
                    'dataIndex += 1
                End If
                k = (m + 1) And 1
                While k < (m - 1)
                    EncodeChar(255 + (fr(k + 2) - fr(k) - THRESHOLD))
                    EncodePosition(p(k))
                    dataIndex = fr(k + 2)
                    k += 2
                End While
            Else 'Just insert the byte by itself
                EncodeChar(text_buf(dataIndex))
                dataIndex += 1
            End If
        Loop
        EncodeEnd()
        Return result.ToArray()
    End Function

    Private Shared Function FindeMatches(ByVal index As Integer) As Boolean
        Dim BufPos As Integer
        Dim TempLen As Byte

        match_length = THRESHOLD - 1
        If index < N Then
            BufPos = 0
        Else
            BufPos = index - (N - 1)
        End If
        While BufPos < index
            TempLen = 0
            While ((index + TempLen) < text_buf.Length) AndAlso (text_buf(BufPos + TempLen) = text_buf(index + TempLen)) And (TempLen < F)
                TempLen += 1
            End While
            If TempLen >= match_length Then
                match_length = TempLen
                match_position = (index - BufPos - 1) And (N - 1)
            End If
            BufPos += 1
        End While
        If match_length <= THRESHOLD Then
            Return False
        End If
        Return True
    End Function


    'This will find the longest match in the dictionary
    Private Shared Sub FindInDict(ByVal dict As Byte(), ByVal data As Byte(), ByVal index As Integer, ByRef outPos As Integer, ByRef outLen As Integer)
        Dim maxMatchCt As Integer = 0
        Dim maxMatchPos As Integer = -1
        For idict As Integer = 0 To dict.Length - 1
            If dict(idict) = data(index) Then
                Dim i2 As Integer = 0
                For i2 = 0 To F - 1
                    If index + i2 >= data.Length Then Exit For
                    If dict((idict + i2) And &HFFF) <> data(index + i2) Then Exit For
                Next
                If i2 > maxMatchCt And i2 > THRESHOLD Then
                    maxMatchCt = i2
                    maxMatchPos = idict
                End If
            End If
        Next
        outPos = maxMatchPos
        outLen = maxMatchCt
    End Sub
End Class
