Public Class Level
    Public name As String
    Public num As Integer
    Public tileset As Tileset
    Public Tiles As Integer(,)
    Public Width As UShort
    Public Height As UShort
    Public objects As LevelObjList = New LevelObjList
    Public p1Start As Point
    Public p2Start As Point
    Public music As UShort
    Public unknown As UShort
    Public sounds As UShort
    Public unknown3 As UShort
    Public page1 As TitlePage
    Public page2 As TitlePage
    Public bonuses As New List(Of Integer)
    Public spritePal As Integer
    Public GFX As LevelGFX
    Public animation As TileAnimation

    Public Sub New(ByVal s As IO.Stream, ByVal name As String, ByVal num As Integer, Optional ByVal import As Boolean = False, Optional ByVal romStream As IO.Stream = Nothing)
        Me.name = name
        Me.num = num
        ErrorLog.HasError() 'Clear error in case there was one

        If import Then
            s.Seek(14, IO.SeekOrigin.Begin)
        End If

        Dim startAddr As Long = s.Position
        Debug.WriteLine(num & " " & name & ": 0x" + Hex(startAddr))
        If import Then
            Dim tiles As Integer = Pointers.ReadPointer(s)
            s.Seek(4, IO.SeekOrigin.Current)
            tileset = New Tileset(romStream, tiles, Pointers.ReadPointer(s), Pointers.ReadPointer(s), Pointers.ReadPointer(s), Pointers.ReadPointer(s), Pointers.ReadPointer(s))
        Else
            tileset = New Tileset(s)
        End If
        s.Seek(startAddr + &H24, IO.SeekOrigin.Begin)
        Width = s.ReadByte() * &H100 + s.ReadByte()
        Height = s.ReadByte() * &H100 + s.ReadByte()
        ReDim Tiles(Width - 1, Height - 1)
        unknown = s.ReadByte * &H100 + s.ReadByte
        unknown3 = s.ReadByte * &H100 + s.ReadByte
        p1Start = New Point(s.ReadByte * &H100 + s.ReadByte, s.ReadByte * &H100 + s.ReadByte)
        p2Start = New Point(s.ReadByte * &H100 + s.ReadByte, s.ReadByte * &H100 + s.ReadByte)
        music = s.ReadByte * &H100 + s.ReadByte
        sounds = music
        s.Seek(startAddr + &H10, IO.SeekOrigin.Begin) 'Seek to Level Palette
        spritePal = Pointers.ReadPointer(s)
        If import Then
            s.Seek(4, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte * &H100 + s.ReadByte + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + &H20, IO.SeekOrigin.Begin) 'Seek To Item Pointer
            Pointers.GoToPointer(s)
        End If
        If Not ErrorLog.HasError Then
            Do 'Add all items
                Dim v As Integer = s.ReadByte * &H100 + s.ReadByte
                If v = 0 Then Exit Do
                objects.Add(New Item(v - 8, s.ReadByte * &H100 + s.ReadByte - 8, (s.ReadByte * &H100 + s.ReadByte) \ 2))
            Loop
        End If
        If import Then
            s.Seek(2, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte * &H100 + s.ReadByte + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + &H1C, IO.SeekOrigin.Begin) 'Seek to Victim pointer
            Pointers.GoToPointer(s)
        End If
        If Not ErrorLog.HasError Then
            For n As Integer = 1 To 10
                Dim vic As New Victim(s.ReadByte * &H100 + s.ReadByte, s.ReadByte * &H100 + s.ReadByte, s.ReadByte * &H100 + s.ReadByte,
                                      s.ReadByte * &H100 + s.ReadByte, Pointers.ReadPointer(s))
                vic.X -= Pointers.SpriteOffsets(vic.index * 2)
                vic.Y -= Pointers.SpriteOffsets(vic.index * 2 + 1)
                objects.Add(vic)
            Next
            objects.Add(New Victim(p1Start.X - 8, p1Start.Y - 39, 0, 0, 1))
            objects.Add(New Victim(p2Start.X - 16, p2Start.Y - 42, 0, 0, 2))
            Do
                Dim x As Integer = s.ReadByte * &H100 + s.ReadByte
                If x = 0 Then Exit Do
                Dim mon As New NRMonster(x, s.ReadByte * &H100 + s.ReadByte, s.ReadByte * &H100 + s.ReadByte,
                                         s.ReadByte * &H100 + s.ReadByte, Pointers.ReadPointer(s))
                mon.X -= Pointers.SpriteOffsets(mon.index * 2)
                mon.Y -= Pointers.SpriteOffsets(mon.index * 2 + 1)
                objects.Add(mon)
            Loop
        End If
        If import Then
            s.Seek(0, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte * &H100 + s.ReadByte + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + 24, IO.SeekOrigin.Begin) ' seek to respawn monsters pointer
            Pointers.GoToPointer(s)
        End If
        If Not ErrorLog.HasError Then
            Do
                Dim radius As Integer = s.ReadByte * &H100 + s.ReadByte
                Dim x1 As Integer = s.ReadByte * &H100 + s.ReadByte
                If radius = 0 Then Exit Do
                Dim mon As New Monster(radius, x1, s.ReadByte * &H100 + s.ReadByte,
                                       s.ReadByte * &H100 + s.ReadByte, Pointers.ReadPointer(s))
                mon.X -= Pointers.SpriteOffsets(mon.index * 2)
                mon.Y -= Pointers.SpriteOffsets(mon.index * 2 + 1)
                objects.Add(mon)
            Loop
        End If
        If import Then
            s.Seek(6, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte * &H100 + s.ReadByte + 14, IO.SeekOrigin.Begin)
            page1 = New TitlePage(s)
            s.Seek(8, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte * &H100 + s.ReadByte + 14, IO.SeekOrigin.Begin)
            page2 = New TitlePage(s)
        Else
            s.Seek(startAddr + &H36, IO.SeekOrigin.Begin)
            Pointers.GoToPointer(s)
            ErrorLog.CheckError("Level is missing title page 1.")
            page1 = New TitlePage(s)
            s.Seek(startAddr + &H3A, IO.SeekOrigin.Begin)
            Pointers.GoToPointer(s)
            ErrorLog.CheckError("Level is missing title page 2.")
            page2 = New TitlePage(s)
        End If
        If import Then
            s.Seek(10, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte * &H100 + s.ReadByte + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + &H3E, IO.SeekOrigin.Begin) 'Seek To Bonus Level Pointer
            Pointers.GoToPointer(s)
        End If
        If Not ErrorLog.HasError Then
            Do
                Dim n As Integer = (s.ReadByte << 8) + s.ReadByte
                If n = 0 Then Exit Do
                bonuses.Add(n)
            Loop
        End If
        s.Seek(startAddr + &H42, IO.SeekOrigin.Begin) 'seek to BOSS pointers
        Do
            Dim ptr As Integer = Pointers.ReadPointer(s)
            If ptr = 0 Then Exit Do
            If ZAMNEditor.Pointers.SpBossMonsters.Contains(ptr) Then
                Dim curaddr As Integer = s.Position
                If import Then
                    If ptr = Pointers.SpBossMonsters(1) Then
                        Dim bossDataPtr = s.ReadByte() * &H100 + s.ReadByte()
                        If bossDataPtr > 0 Then
                            s.Seek(bossDataPtr + 14, IO.SeekOrigin.Begin)
                        Else
                            'Ignore if it has no pointer (which means it came from an older version of the editor)
                            s.Seek(curaddr + 4, IO.SeekOrigin.Begin)
                            Continue Do
                        End If
                    End If
                Else
                        If ptr = Pointers.SpBossMonsters(1) Then
                        Pointers.GoToPointer(s)
                    End If
                End If
                Select Case ptr
                    Case ZAMNEditor.Pointers.SpBossMonsters(0)
                        objects.Add(New BossMonster(ptr, Pointers.ToArray(Pointers.ReadPointer(s))))
                    Case ZAMNEditor.Pointers.SpBossMonsters(1)
                        Dim value As Integer, count As Integer, passed As Boolean = False
                        Dim newData As New List(Of Byte)
                        If s.Position > curaddr Then
                            Do
                                value = s.ReadByte * &H100 + s.ReadByte
                                If value < 0 Or newData.Count > &H200 Then
                                    newData.Clear()
                                    newData.Add(0)
                                    newData.Add(0)
                                    Exit Do
                                End If
                                If value = 0 Then passed = True
                                If passed And (value = &HFFFF Or value = &HFFFE) Then count -= 2
                                If Not passed Then count += 1
                                newData.Add(value \ &H100)
                                newData.Add(value Mod &H100)
                                If passed And count = 0 Then Exit Do
                            Loop
                            objects.Add(New BossMonster(ptr, newData.ToArray()))
                            Debug.WriteLine("Tile animation size: " + newData.Count.ToString())
                        End If
                End Select
                s.Seek(curaddr + 4, IO.SeekOrigin.Begin)
            Else
                objects.Add(New BossMonster(ptr, s.ReadByte * &H100 + s.ReadByte, s.ReadByte * &H100 + s.ReadByte))
            End If
        Loop
        If import Then
            s.Seek(12, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte * &H100 + s.ReadByte, IO.SeekOrigin.Begin)
            For l As Integer = 0 To Width * Height - 1
                Tiles(l Mod Width, l \ Width) = (s.ReadByte() * &H100 + s.ReadByte()) And &HFF
            Next
        Else
            s.Seek(startAddr + 4, IO.SeekOrigin.Begin)
            Pointers.GoToPointer(s)
            ErrorLog.CheckError("Level has no background data.")
            Dim Map As Byte() = ZAMNCompress.Decompress(s)
            For l As Integer = 0 To Width * Height - 1
                Tiles(l Mod Width, l \ Width) = (Map(l * 2) * &H100 + Map(l * 2 + 1)) And &HFF
            Next
        End If

        If import Then
            GFX = New LevelGFX(romStream, spritePal)
        Else
            GFX = New LevelGFX(s, spritePal)
        End If
        s.Close()

        LoadTileAnimation()
    End Sub


    Public Sub New(ByVal s As IO.Stream, ByVal name As String, ByVal num As Integer, Optional ByVal import As Boolean = False, Optional ByVal romStream As IO.Stream = Nothing, ByVal Optional snesimport As Boolean = False)
        Me.name = name
        Me.num = num

        ErrorLog.HasError() 'Clear error in case there was one

        Dim tilesd, BossMonstersd, SpBossMonstersd, SpritePtrsd, Palettesd, Collisiond, Graphicsd, Unknownd, PltAnimd, Bossd As New Dictionary(Of Integer, Integer)
        Pointers.FillDictionary(PointersSNES.Tilesets, Pointers.Tilesets, tilesd)
        Pointers.FillDictionary(PointersSNES.BossMonsters, Pointers.BossMonsters, BossMonstersd)
        Pointers.FillDictionary(PointersSNES.SpBossMonsters, Pointers.SpBossMonsters, SpBossMonstersd)
        'Pointers.FillDictionary(PointersSNES.SuggestTilesets, Pointers.SuggestTilesets, SuggestTilesetd)
        Pointers.FillDictionary(PointersSNES.SpritePtrs, Pointers.SpritePtrs, SpritePtrsd)
        Pointers.FillDictionary(PointersSNES.Palettes, Pointers.Palettes, Palettesd)
        Pointers.FillDictionary(PointersSNES.Collision, Pointers.Collision, Collisiond)
        Pointers.FillDictionary(PointersSNES.Graphics, Pointers.Graphics, Graphicsd)
        Pointers.FillDictionary(PointersSNES.PltAnim, Pointers.PltAnim, PltAnimd)
        Pointers.FillDictionary(PointersSNES.Boss, Pointers.Boss, Bossd)
        Dim Musicd As New Dictionary(Of UShort, UShort)
        Musicd(2) = 2
        Musicd(3) = 5
        Musicd(4) = &H28
        Musicd(5) = &H2A
        Musicd(6) = &H2C
        Musicd(7) = &H2D
        Musicd(8) = &H2E
        Musicd(9) = &H2F
        Musicd(10) = 6
        Musicd(11) = 3
        Musicd(12) = 1
        Dim SpritePltd As New Dictionary(Of Integer, Integer)
        SpritePltd(PointersSNES.SpritePlt) = Pointers.SpritePlt

        If import Then
            s.Seek(14, IO.SeekOrigin.Begin)
        End If

        Dim startAddr As Long = s.Position
        Debug.WriteLine(num & " " & name & ": 0x" + Hex(startAddr))
        If import Then
            Dim tiles As Integer = tilesd(PointersSNES.ReadPointer(s))
            s.Seek(4, IO.SeekOrigin.Current)
            tileset = New Tileset(romStream, tiles, Collisiond(PointersSNES.ReadPointer(s)), Graphicsd(PointersSNES.ReadPointer(s)), Palettesd(PointersSNES.ReadPointer(s)), SpritePltd(PointersSNES.ReadPointer(s)), PltAnimd(PointersSNES.ReadPointer(s)))
        Else
            tileset = New Tileset(s)
        End If
        s.Seek(startAddr + &H22, IO.SeekOrigin.Begin)
        Width = s.ReadByte() + s.ReadByte() * &H100
        Height = s.ReadByte() + s.ReadByte() * &H100
        ReDim Tiles(Width - 1, Height - 1)
        unknown = s.ReadByte + s.ReadByte * &H100
        unknown3 = s.ReadByte + s.ReadByte * &H100
        p1Start = New Point(s.ReadByte + s.ReadByte * &H100, s.ReadByte + s.ReadByte * &H100)
        p2Start = New Point(s.ReadByte + s.ReadByte * &H100, s.ReadByte + s.ReadByte * &H100)
        music = Musicd(s.ReadByte + s.ReadByte * &H100)
        sounds = s.ReadByte + s.ReadByte * &H100
        sounds = music
        s.Seek(startAddr + &H14, IO.SeekOrigin.Begin)
        spritePal = SpritePltd(PointersSNES.ReadPointer(s))
        If import Then
            s.Seek(4, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte + s.ReadByte * &H100 + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + &H20, IO.SeekOrigin.Begin)
            PointersSNES.GoToRelativePointer(s, &H9F)
        End If
        If Not ErrorLog.HasError Then
            Do
                Dim v As Integer = s.ReadByte + s.ReadByte * &H100
                If v = 0 Then Exit Do
                objects.Add(New Item(v - 8, s.ReadByte + s.ReadByte * &H100 - 8, s.ReadByte \ 2))
            Loop
        End If
        If import Then
            s.Seek(2, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte + s.ReadByte * &H100 + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + &H1E, IO.SeekOrigin.Begin)
            PointersSNES.GoToRelativePointer(s, &H9F)
        End If
        If Not ErrorLog.HasError Then
            For n As Integer = 1 To 10
                Dim vic As New Victim(s.ReadByte + s.ReadByte * &H100, s.ReadByte + s.ReadByte * &H100, s.ReadByte + s.ReadByte * &H100,
                                      s.ReadByte + s.ReadByte * &H100, SpritePtrsd(PointersSNES.ReadPointer(s)))
                vic.X -= PointersSNES.SpriteOffsets(vic.index * 2)
                vic.Y -= PointersSNES.SpriteOffsets(vic.index * 2 + 1)
                objects.Add(vic)
            Next
            objects.Add(New Victim(p1Start.X - 8, p1Start.Y - 39, 0, 0, 1))
            objects.Add(New Victim(p2Start.X - 16, p2Start.Y - 42, 0, 0, 2))
            Do
                Dim x As Integer = s.ReadByte + s.ReadByte * &H100
                If x = 0 Then Exit Do
                Dim mon As New NRMonster(x, s.ReadByte + s.ReadByte * &H100, s.ReadByte + s.ReadByte * &H100,
                                         s.ReadByte + s.ReadByte * &H100, SpritePtrsd(PointersSNES.ReadPointer(s)))
                mon.X -= PointersSNES.SpriteOffsets(mon.index * 2)
                mon.Y -= PointersSNES.SpriteOffsets(mon.index * 2 + 1)
                objects.Add(mon)
            Loop
        End If
        If import Then
            s.Seek(0, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte + s.ReadByte * &H100 + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + 28, IO.SeekOrigin.Begin)
            PointersSNES.GoToRelativePointer(s, &H9F)
        End If
        If Not ErrorLog.HasError Then
            Do
                Dim radius As Integer = s.ReadByte
                Dim x1 As Integer = s.ReadByte
                If x1 = 0 And radius = 0 Then Exit Do
                Dim mon As New Monster(radius, x1 + s.ReadByte * &H100, s.ReadByte + s.ReadByte * &H100,
                                       s.ReadByte, SpritePtrsd(PointersSNES.ReadPointer(s)))
                mon.X -= PointersSNES.SpriteOffsets(mon.index * 2)
                mon.Y -= PointersSNES.SpriteOffsets(mon.index * 2 + 1)
                objects.Add(mon)
            Loop
        End If
        If import Then
            s.Seek(6, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte + s.ReadByte * &H100 + 14, IO.SeekOrigin.Begin)
            page1 = New TitlePage(s)
            s.Seek(8, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte + s.ReadByte * &H100 + 14, IO.SeekOrigin.Begin)
            page2 = New TitlePage(s)
        Else
            s.Seek(startAddr + &H36, IO.SeekOrigin.Begin)
            PointersSNES.GoToRelativePointer(s, &H9F)
            ErrorLog.CheckError("Level is missing title page 1.")
            page1 = New TitlePage(s)
            s.Seek(startAddr + &H38, IO.SeekOrigin.Begin)
            PointersSNES.GoToRelativePointer(s, &H9F)
            ErrorLog.CheckError("Level is missing title page 2.")
            page2 = New TitlePage(s)
        End If
        If import Then
            s.Seek(10, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte + s.ReadByte * &H100 + 14, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + &H3A, IO.SeekOrigin.Begin)
            PointersSNES.GoToRelativePointer(s, &H9F)
        End If
        If Not ErrorLog.HasError Then
            Do
                Dim n As Integer = s.ReadByte + s.ReadByte * &H100
                If n = 0 Then Exit Do
                n = ((n / 2) - 2) * 4 + 8
                bonuses.Add(n)
            Loop
        End If
        s.Seek(startAddr + &H3C, IO.SeekOrigin.Begin)
        Do
            Dim ptr As Integer = PointersSNES.ReadPointer(s)
            If ptr = -1 Then Exit Do
            If ZAMNEditor.PointersSNES.SpBossMonsters.Contains(ptr) Then
                Dim curaddr As Integer = s.Position
                If import Then
                    Dim bossDataPtr = s.ReadByte() + s.ReadByte() * &H100
                    If bossDataPtr > 0 Then
                        s.Seek(bossDataPtr + 14, IO.SeekOrigin.Begin)
                    Else
                        'Ignore if it has no pointer (which means it came from an older version of the editor)
                        s.Seek(curaddr + 4, IO.SeekOrigin.Begin)
                        Continue Do
                    End If
                Else
                    PointersSNES.GoToPointer(s)
                End If
                Select Case ptr
                    Case ZAMNEditor.PointersSNES.SpBossMonsters(0)
                        Dim SNESPal As Integer = PointersSNES.ReadPointer(s)
                        objects.Add(New BossMonster(SpBossMonstersd(ptr), Pointers.ToArray(Palettesd(SNESPal))))
                    Case ZAMNEditor.PointersSNES.SpBossMonsters(1)
                        Dim value As Integer, count As Integer, passed As Boolean = False
                        Dim newData As New List(Of Byte)
                        Dim index As Integer = 0
                        Dim savecount As Integer = 0
                        If s.Position > curaddr Then
                            index = newData.Count
                            Do
                                value = s.ReadByte + s.ReadByte * &H100
                                If value < 0 Or newData.Count > &H200 Then
                                    newData.Clear()
                                    newData.Add(0)
                                    newData.Add(0)

                                    Exit Do
                                End If
                                If value = 0 Then
                                    newData.Add(0)
                                    newData.Add(0)
                                    passed = True
                                    savecount = count
                                End If
                                If passed And (value = &HFFFF Or value = &HFFFE) Then count -= 1
                                If Not passed Then count += 1
                                newData.Add(value \ &H100)
                                newData.Add(value Mod &H100)
                                If passed And count = 0 Then
                                    For index = 0 To savecount - 1
                                        newData.Insert(0, 1)
                                        newData.Insert(0, 2)
                                    Next
                                    Exit Do
                                End If
                            Loop
                            objects.Add(New BossMonster(SpBossMonstersd(ptr), newData.ToArray()))
                            Debug.WriteLine("Tile animation size: " + newData.Count.ToString())
                        End If
                End Select
                s.Seek(curaddr + 4, IO.SeekOrigin.Begin)
            Else
                objects.Add(New BossMonster(BossMonstersd(ptr), s.ReadByte + s.ReadByte * &H100, s.ReadByte + s.ReadByte * &H100))
            End If
        Loop
        If import Then
            s.Seek(12, IO.SeekOrigin.Begin)
            s.Seek(s.ReadByte + s.ReadByte * &H100, IO.SeekOrigin.Begin)
        Else
            s.Seek(startAddr + 4, IO.SeekOrigin.Begin)
            PointersSNES.GoToPointer(s)
            ErrorLog.CheckError("Level has no background data.")
        End If
        For l As Integer = 0 To Width * Height - 1
            Tiles(l Mod Width, l \ Width) = (s.ReadByte() + s.ReadByte() * &H100) And &HFF
        Next
        If import Then
            GFX = New LevelGFX(romStream, spritePal)
        Else
            GFX = New LevelGFX(s, spritePal)
        End If
        s.Close()

        LoadTileAnimation()
    End Sub



    Public Function GetWriteData() As LevelWriteData
        p1Start = New Point(objects.Victims(10).X + 8, objects.Victims(10).Y + 39)
        p2Start = New Point(objects.Victims(11).X + 16, objects.Victims(11).Y + 42)
        Dim file As New List(Of Byte)
        Dim addrOffsets(5) As Integer
        Dim bossPtrs As New List(Of Integer)
        file.AddRange(Pointers.ToArray(tileset.address))
        file.AddRange(New Byte() {0, 0, 0, 0}) 'LVLDATAPTR
        file.AddRange(Pointers.ToArray(tileset.collisionAddr)) 'TLSTCOLPTR
        file.AddRange(Pointers.ToArray(tileset.gfxAddr)) 'TLSTGFXPTR
        file.AddRange(Pointers.ToArray(tileset.paletteAddr)) 'PALETTEPTR
        file.AddRange(Pointers.ToArray(tileset.pltAnimAddr)) 'PLTANIMPTR
        file.AddRange(New Byte() {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,'MNSTPTR, VICTIMPTR, ITEMSPTR
                                  Width \ &H100, Width Mod &H100,'LVLWIDTH
                                  Height \ &H100, Height Mod &H100,'LVLHEIGHT
                                  unknown \ &H100, unknown Mod &H100,
                                  unknown3 \ &H100, unknown3 Mod &H100,
                                  p1Start.X \ &H100, p1Start.X Mod &H100, p1Start.Y \ &H100, p1Start.Y Mod &H100,
                                  p2Start.X \ &H100, p2Start.X Mod &H100, p2Start.Y \ &H100, p2Start.Y Mod &H100,
                                  music \ &H100, music Mod &H100,'MUSIC SONG NUMBER
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}) 'TITLPAGE1PTR, TITLPAGE2PTR, BONUSPTR
        For Each m As BossMonster In objects.BossMonsters
            file.AddRange(Pointers.ToArray(m.ptr))
            If m.ptr = Pointers.SpBossMonsters(0) Then
                file.AddRange(Pointers.ToArray(m.GetBGPalette))
            Else
                file.AddRange(New Byte() {m.X \ &H100, m.X Mod &H100, m.Y \ &H100, m.Y Mod &H100})
            End If
        Next
        file.AddRange(New Byte() {0, 0, 0, 0}) 'End of Header
        For Each m As BossMonster In objects.BossMonsters
            bossPtrs.Add(file.Count)
            If m.ptr = Pointers.SpBossMonsters(1) Then
                file.AddRange(m.exData)
            End If
        Next
        'Level titles
        addrOffsets(3) = file.Count
        page1.Write(file, 1)
        addrOffsets(4) = file.Count
        page2.Write(file, 0)
        If (file.Count And 1) <> 0 Then 'Aling to even address
            file.Add(0)
        End If

        Dim x As UShort, y As UShort
        'Monster data
        addrOffsets(0) = file.Count
        For Each m As Monster In objects.Monsters
            x = m.X + Pointers.SpriteOffsets(m.index * 2)
            y = m.Y + Pointers.SpriteOffsets(m.index * 2 + 1)
            file.AddRange(New Byte() {0, m.radius, x \ &H100, x Mod &H100, y \ &H100, y Mod &H100, 0, m.delay})
            file.AddRange(Pointers.ToArray(m.ptr))
        Next
        file.AddRange(New Byte() {0, 0})
        'Victim data
        addrOffsets(1) = file.Count
        For Each v As Victim In objects.Victims
            If v.ptr > 2 Then
                x = v.X + Pointers.SpriteOffsets(v.index * 2)
                y = v.Y + Pointers.SpriteOffsets(v.index * 2 + 1)
                file.AddRange(New Byte() {x \ &H100, x Mod &H100, y \ &H100, y Mod &H100, v.unused \ &H100, v.unused Mod &H100,
                                          v.num \ &H100, v.num Mod &H100})
                file.AddRange(Pointers.ToArray(v.ptr))
            End If
        Next
        'NRMonster data
        For Each m As NRMonster In objects.NRMonsters
            x = m.X + Pointers.SpriteOffsets(m.index * 2)
            y = m.Y + Pointers.SpriteOffsets(m.index * 2 + 1)
            file.AddRange(New Byte() {x \ &H100, x Mod &H100, y \ &H100, y Mod &H100, m.extra \ &H100, m.extra Mod &H100,
                                      m.unused \ &H100, m.unused Mod &H100})
            file.AddRange(Pointers.ToArray(m.ptr))
        Next
        file.AddRange(New Byte() {0, 0})
        'Item data
        addrOffsets(2) = file.Count
        For Each i As Item In objects.Items
            x = i.X + 8
            y = i.Y + 8
            file.AddRange(New Byte() {x \ &H100, x Mod &H100, y \ &H100, y Mod &H100, 0, i.Type * 2})
        Next
        file.AddRange(New Byte() {0, 0})
        'Bonuses
        addrOffsets(5) = file.Count
        For Each b As Integer In bonuses
            file.AddRange(New Byte() {b \ &H100, b Mod &H100})
        Next
        file.AddRange(New Byte() {0, 0})
        Return New LevelWriteData(file, addrOffsets, bossPtrs)
    End Function

    Public Function ToFile() As Byte()
        Dim data As LevelWriteData = GetWriteData()
        Dim file(0) As Byte
        Dim fs As New ByteArrayStream(file)
        For l As Integer = 0 To 5
            fs.WriteWord(data.addrOffsets(l))
        Next
        fs.WriteWord(0)
        fs.Write(data.data, 0, data.data.Length)
        fs.Seek(12, IO.SeekOrigin.Begin) '12 - 6 Word Pointers
        fs.WriteWord(fs.Length)
        fs.Seek(0, IO.SeekOrigin.End)
        For y As Integer = 0 To Height - 1
            For x As Integer = 0 To Width - 1
                fs.WriteByte(0)
                fs.WriteByte(Tiles(x, y))
            Next
        Next
        fs.Seek(14 + &H42, IO.SeekOrigin.Begin) '14 - 4 Word Pointers and final 00 00
        Dim tempptr As Integer
        Dim bossIndex As Integer = 0
        Do 'set pointers for special boss monsters
            tempptr = fs.ReadDWord() 'Pointers.ReadPointer(fs)
            If Pointers.SpBossMonsters.Contains(tempptr) And bossIndex < data.bossDataPtr.Count Then
                If tempptr = Pointers.SpBossMonsters(1) Then
                    fs.WriteWord(data.bossDataPtr(bossIndex)) 'вместо координаты Х
                    fs.Seek(2, IO.SeekOrigin.Current) 'пропустить координату Y
                Else
                    fs.Seek(4, IO.SeekOrigin.Current) 'пропустить изменение адреса палитры
                End If
                bossIndex += 1
            ElseIf tempptr = 0 Then
                Exit Do
            Else
                fs.Seek(4, IO.SeekOrigin.Current)
            End If
        Loop
        Return fs.array
    End Function

    Public Sub ClearTileAnimation()
        If animation IsNot Nothing Then
            animation.Reset()
            animation = Nothing
        End If
    End Sub
    Public Sub LoadTileAnimation()
        animation = Nothing
        For Each boss As BossMonster In objects.BossMonsters
            If boss.Type = ZAMNEditor.Pointers.SpBossMonsters(1) Then
                If boss.exData.Length <= 4 Then
                    Return
                End If
                animation = New TileAnimation(tileset, boss)
                Return
            End If
        Next
    End Sub
End Class
