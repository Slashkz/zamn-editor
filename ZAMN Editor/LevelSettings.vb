Public Class LevelSettings

    Public lvl As Level
    Public ed As Editor
    Private tileAnim As Byte() = Nothing
    Private tileAnimChanged As Boolean

    Public Overloads Function ShowDialog(ByVal ed As Editor) As DialogResult
        Me.lvl = ed.EdControl.lvl
        Me.ed = ed
        tileAnim = Nothing
        tileAnimChanged = False
        'Tiles
        addrTiles.Value = lvl.tileset.address
        cboTiles.SelectedIndex = Array.IndexOf(Pointers.Tilesets, lvl.tileset.address)
        If cboTiles.SelectedIndex <> -1 Then
            radTilesAuto.Checked = True
        Else
            radTilesMan.Checked = True
        End If
        'Palette
        addrPal.Value = lvl.tileset.paletteAddr
        UpdatePals()
        Dim palIdx As Integer = Array.IndexOf(Pointers.Palettes, lvl.tileset.paletteAddr)
        If palIdx >= cboTiles.SelectedIndex * 5 And palIdx <= cboTiles.SelectedIndex * 5 + 4 Then
            cboPal.SelectedIndex = palIdx Mod 5
        End If
        If cboPal.Items.Count > 0 And cboPal.SelectedIndex > -1 Then
            radPalAuto.Checked = True
        Else
            radPalMan.Checked = True
        End If
        'Graphics
        addrGFX.Value = lvl.tileset.gfxAddr
        If Array.IndexOf(Pointers.Graphics, lvl.tileset.gfxAddr) = cboTiles.SelectedIndex Then
            radGFXAuto.Checked = True
        Else
            radGFXMan.Checked = True
        End If
        'Collision
        addrCol.Value = lvl.tileset.collisionAddr
        If Array.IndexOf(Pointers.Collision, lvl.tileset.collisionAddr) = cboTiles.SelectedIndex Then
            radColAuto.Checked = True
        Else
            radColMan.Checked = True
        End If
        'Unknown
        nudUnk.Value = lvl.unknown
        If Array.IndexOf(Pointers.Unknown, lvl.unknown) = cboTiles.SelectedIndex Or Array.LastIndexOf(Pointers.Unknown, lvl.unknown) = cboTiles.SelectedIndex Then
            radUnkAuto.Checked = True
        Else
            radUnkMan.Checked = True
        End If
        'Sprite palette
        addrSPal.Value = lvl.spritePal
        If lvl.spritePal = Pointers.SpritePlt Then
            radSPalAuto.Checked = True
        Else
            radSPalMan.Checked = True
        End If
        'Palette Animation
        addrPAnim.Value = lvl.tileset.pltAnimAddr
        cboPltAnim.SelectedIndex = Array.IndexOf(Pointers.PltAnim, lvl.tileset.pltAnimAddr)
        If cboPltAnim.SelectedIndex <> -1 Then
            radPAnimAuto.Checked = True
        Else
            radPAnimMan.Checked = True
        End If
        'Music
        nudMusic.Value = lvl.music
        If lvl.music >= 1 And lvl.music <= &H2F Then
            cboMusic.SelectedIndex = Array.IndexOf(Pointers.Musics, lvl.music)
        End If
        If cboMusic.SelectedIndex <> -1 Then
            radMusicAuto.Checked = True
        Else
            radMusicMan.Checked = True
        End If

        'Unknown3
        nudUnk3.Value = lvl.unknown3
        If lvl.unknown3 = &H1FF Then
            radUnk3Auto.Checked = True
        Else
            radUnk3Man.Checked = True
        End If
        'Bonuses
        lvl.bonuses.Sort()

        For l As Integer = 0 To lstBonuses.Items.Count - 1
            lstBonuses.SetItemChecked(l, False)
        Next
        For Each b As Integer In lvl.bonuses
            If b >= 8 And b <= 64 And b Mod 4 = 0 Then
                lstBonuses.SetItemChecked(b \ 4 - 2, True)
            End If
        Next
        chkPltFade.Checked = False
        cboTileAnim.SelectedIndex = -1
        For Each m As BossMonster In lvl.objects.BossMonsters
            'Palette fade
            If m.ptr = Pointers.SpBossMonsters(0) Then
                chkPltFade.Checked = True
                addrPalF.Value = m.GetBGPalette
                Dim address As Integer = addrPalF.Value
                palIdx = Array.IndexOf(Pointers.Palettes, address)
                If palIdx >= cboTiles.SelectedIndex * 5 And palIdx <= cboTiles.SelectedIndex * 5 + 4 Then
                    cboPalF.SelectedIndex = If((palIdx Mod 5) >= cboPalF.Items.Count, -1, palIdx Mod 5)
                End If
                If cboPalF.Items.Count > 0 And cboPalF.SelectedIndex > -1 Then
                    radPalAutoF.Checked = True
                Else
                    radPalManF.Checked = True
                End If
            End If
            'Tile Animation
            If m.ptr = Pointers.SpBossMonsters(1) Then
                tileAnim = m.exData
            End If
        Next

        btnDeleteTileAnim.Enabled = tileAnim IsNot Nothing
        btnExportTileAnim.Enabled = tileAnim IsNot Nothing

        Return Me.ShowDialog()
    End Function

    Private Sub UpdatePals()
        cboPal.Items.Clear()
        cboPalF.Items.Clear()
        If cboTiles.SelectedIndex = -1 Then Return
        Dim startIdx As Integer = cboTiles.SelectedIndex * 5
        For l As Integer = startIdx To startIdx + 4
            If Pointers.Palettes(l) <> 0 Then
                cboPal.Items.Add(Pointers.PalNames(l))
                cboPalF.Items.Add(Pointers.PalNames(l))
            End If
        Next
    End Sub

    Private Sub radAuto_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles radColAuto.CheckedChanged, radPalAutoF.CheckedChanged,
        radGFXAuto.CheckedChanged, radMusicAuto.CheckedChanged, radPalAuto.CheckedChanged, radSPalAuto.CheckedChanged, radPAnimAuto.CheckedChanged,
        radTilesAuto.CheckedChanged, radUnk3Auto.CheckedChanged, radUnkAuto.CheckedChanged
        Dim rad As Control = sender
        For Each ctrl As Control In rad.Parent.Controls
            If TypeOf ctrl Is AddressUpDown Or TypeOf ctrl Is NumericUpDown Then
                ctrl.Enabled = False
            End If
            If TypeOf ctrl Is ComboBox Then
                ctrl.Enabled = True
            End If
        Next
        'Update values
        If cboTiles.SelectedIndex > -1 Then
            If radTilesAuto.Checked Then addrTiles.Value = Pointers.Tilesets(cboTiles.SelectedIndex)
            If cboPal.SelectedIndex > -1 And radPalAuto.Checked Then addrPal.Value = Pointers.Palettes(cboTiles.SelectedIndex * 5 + cboPal.SelectedIndex)
            If radGFXAuto.Checked Then addrGFX.Value = Pointers.Graphics(cboTiles.SelectedIndex)
            If radColAuto.Checked Then addrCol.Value = Pointers.Collision(cboTiles.SelectedIndex)
            If radUnkAuto.Checked Then nudUnk.Value = Pointers.Unknown(cboTiles.SelectedIndex)
            If cboPalF.SelectedIndex > -1 And radPalAutoF.Checked Then addrPalF.Value = Pointers.Palettes(cboTiles.SelectedIndex * 5 + cboPalF.SelectedIndex)
        End If
        If radSPalAuto.Checked Then addrSPal.Value = Pointers.SpritePlt
        If cboPltAnim.SelectedIndex > -1 And radPAnimAuto.Checked Then addrPAnim.Value = Pointers.PltAnim(cboPltAnim.SelectedIndex)
        If cboMusic.SelectedIndex > -1 And radMusicAuto.Checked Then nudMusic.Value = Pointers.Musics(cboMusic.SelectedIndex)
        If radUnk3Auto.Checked Then nudUnk3.Value = 511
    End Sub

    Private Sub radMan_CheckedChanged(ByVal sender As Object, ByVal e As EventArgs) Handles radColMan.CheckedChanged, radPalManF.CheckedChanged,
        radGFXMan.CheckedChanged, radMusicMan.CheckedChanged, radPalMan.CheckedChanged, radPAnimMan.CheckedChanged, radSPalMan.CheckedChanged,
        radTilesMan.CheckedChanged, radUnk3Man.CheckedChanged, radUnkMan.CheckedChanged
        Dim rad As Control = sender
        For Each ctrl As Control In rad.Parent.Controls
            If TypeOf ctrl Is ComboBox Then
                ctrl.Enabled = False
            End If
            If TypeOf ctrl Is AddressUpDown Or TypeOf ctrl Is NumericUpDown Then
                ctrl.Enabled = True
            End If
        Next
    End Sub

    Private Sub cboMusic_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboMusic.SelectedIndexChanged
        If cboMusic.SelectedIndex = -1 Then Return
        nudMusic.Value = Pointers.Musics(cboMusic.SelectedIndex)
    End Sub


    Private Sub cboPal_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboPal.SelectedIndexChanged
        If cboPal.SelectedIndex = -1 Then Return
        addrPal.Value = Pointers.Palettes(cboTiles.SelectedIndex * 5 + cboPal.SelectedIndex)
    End Sub

    Private Sub cboPalF_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboPalF.SelectedIndexChanged
        If cboPalF.SelectedIndex = -1 Then Return
        addrPalF.Value = Pointers.Palettes(cboTiles.SelectedIndex * 5 + cboPalF.SelectedIndex)
    End Sub

    Private Sub cboPltAnim_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboPltAnim.SelectedIndexChanged
        If cboPltAnim.SelectedIndex = -1 Then Return
        addrPAnim.Value = Pointers.PltAnim(cboPltAnim.SelectedIndex)
    End Sub

    Private Sub cboTiles_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboTiles.SelectedIndexChanged
        If cboTiles.SelectedIndex = -1 Then Return
        If radGFXAuto.Checked Then addrGFX.Value = Pointers.Graphics(cboTiles.SelectedIndex)
        If radColAuto.Checked Then addrCol.Value = Pointers.Collision(cboTiles.SelectedIndex)
        If radUnkAuto.Checked Then nudUnk.Value = Pointers.Unknown(cboTiles.SelectedIndex)
        UpdatePals()
        If radPalMan.Checked = True Then Return
        cboPal.SelectedIndex = 0
        addrTiles.Value = Pointers.Tilesets(cboTiles.SelectedIndex)
    End Sub

    Private Sub chkPltFade_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chkPltFade.CheckedChanged
        grpPltFade.Enabled = chkPltFade.Checked
    End Sub

    Private Sub btnCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub

    Private Sub btnApply_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnApply.Click
        Dim reloadTileset As Boolean = False
        Dim reloadSprites As Boolean = False
        If lvl.tileset.address <> addrTiles.Value Or lvl.tileset.paletteAddr <> addrPal.Value Or lvl.tileset.gfxAddr <> addrGFX.Value Or lvl.tileset.collisionAddr <> addrCol.Value Then
            reloadTileset = True
        End If
        If lvl.spritePal <> addrSPal.Value Then
            reloadSprites = True
        End If
        lvl.tileset.address = addrTiles.Value
        lvl.tileset.paletteAddr = addrPal.Value
        lvl.tileset.gfxAddr = addrGFX.Value
        lvl.tileset.collisionAddr = addrCol.Value
        lvl.unknown = nudUnk.Value
        lvl.spritePal = addrSPal.Value
        lvl.tileset.pltAnimAddr = addrPAnim.Value
        If cboPltAnim.SelectedIndex = 0 And radPAnimAuto.Checked Then lvl.tileset.pltAnimAddr = 0
        lvl.music = nudMusic.Value
        lvl.sounds = nudMusic.Value
        lvl.unknown3 = nudUnk3.Value
        lvl.bonuses.Clear()
        For l As Integer = 0 To lstBonuses.Items.Count - 1
            If lstBonuses.GetItemChecked(l) Then
                lvl.bonuses.Add(l * 4 + 8)
            End If
        Next

        Dim m As Integer = 0
        Do Until m = lvl.objects.BossMonsters.Count
            If lvl.objects.BossMonsters(m).ptr = Pointers.SpBossMonsters(0) Then
                lvl.objects.BossMonsters.RemoveAt(m)
            ElseIf lvl.objects.BossMonsters(m).ptr = Pointers.SpBossMonsters(1) Then
                lvl.objects.BossMonsters.RemoveAt(m)
            Else
                m += 1
            End If
        Loop
        If chkPltFade.Checked Then
            Dim exData(3) As Byte
            Array.Copy(Pointers.ToArray(addrPalF.Value), 0, exData, 0, 4)
            lvl.objects.BossMonsters.Add(New BossMonster(Pointers.SpBossMonsters(0), exData))
        End If
        If tileAnim IsNot Nothing Then
            lvl.objects.BossMonsters.Add(New BossMonster(Pointers.SpBossMonsters(1), tileAnim))
        End If


        If reloadTileset Or tileAnimChanged Then
            lvl.ClearTileAnimation()
            If reloadTileset Then
                Dim s As New IO.FileStream(ed.r.path, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read)
                lvl.tileset.Reload(s)
                ed.EdControl.TilePicker.LoadTileset(ed.EdControl.lvl.tileset)
                s.Close()
            End If
            lvl.LoadTileAnimation()
            ed.EdControl.TilePicker.Invalidate(True)
        End If
        If reloadSprites Then
            Dim s As New IO.FileStream(ed.r.path, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read)
            lvl.GFX.Reload(s, lvl.spritePal)
            s.Close()
        End If
        ed.EdControl.UndoMgr.ForceDirty()
        ed.EdControl.Repaint()
    End Sub

    Private Sub btnOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnOK.Click
        btnApply_Click(sender, e)
        Me.Close()
    End Sub


    Private Sub btnDeleteTileAnim_Click(sender As System.Object, e As System.EventArgs) Handles btnDeleteTileAnim.Click
        btnDeleteTileAnim.Enabled = False
        btnExportTileAnim.Enabled = False
        tileAnim = Nothing
        tileAnimChanged = True
    End Sub

    Private Sub btnExportTileAnim_Click(sender As System.Object, e As System.EventArgs) Handles btnExportTileAnim.Click
        If saveTileAnim.ShowDialog() = Windows.Forms.DialogResult.OK Then
            For i As Integer = 0 To tileAnim.Length - 2 Step 2
                If tileAnim(i) = 0 And tileAnim(i + 1) = 0 And tileAnim(i + 2) = 0 And tileAnim(i + 3) = 0 Then
                    Try
                        Dim fs As New IO.FileStream(saveTileAnim.FileName, IO.FileMode.Create, IO.FileAccess.Write)
                        fs.Write(tileAnim, i + 4, tileAnim.Length - (i + 4))
                        fs.Close()
                    Catch ex As Exception
                        MsgBox("Failed to export:" & Environment.NewLine & ex.Message)
                    End Try
                    Return
                End If
            Next
            MsgBox("Tile animation was empty so nothing was exported.")
        End If
    End Sub

    Dim tileAnimPresets As String() = {"01D9000700F9000700E0000701D9FFFF01D1000701D300070175000701D1FFFF01CB000701DE0007018E000701CBFFFF01A80007018D000701A3000701A8FFFF",
                                       "01D9000700F9000700E0000701D9FFFF01E5000701E6000701D2000701E5FFFF01DE0007018E000701CB000701DEFFFF01DD000701BE000701DDFFFF01ED000701EE000701EF000701EDFFFF",
                                       "0123006401210029012300090123FFFF0122006700F30027012200080122FFFF001F006A00560003007B001E00560003001F0008001FFFFF0054006D0056001E0054000B0054FFFF01340064005500320134FFFF",
                                       "01C500030065005001C6000301CB000301C5000301C6000301CC000301CB000301C5000301C6000301CC000301C6FFFF01C800030065003C01C8000301C9000301C8000301C9000301C8000301C9000301C8000301C9000301C8000301C9FFFF01C700030065005001C7001B01C7FFFF01C900030166003C01C9000301C8000301C9000301C8000301C9000301C8000301C9000301C8000301C9000301C8FFFF01CC00030065000301CB000301C5000301C6000301CC000301CB000301C5000301C600030065006401CB000301CCFFFF01CD001B0065006401CD000301CDFFFF",
                                       "01C0005A01C3000501C4000501E5005A01C4000501C3000501C0006401C0FFFF01C1005A01C4000501C3000501C1006401C1005A01C3000501C4000501E5FFFF01C2006401C2005A01C3000501C4000501E5005A01C4000501C3000501C2FFFF005E0064005E0064001800640018FFFF005F006400180064005F00640018FFFF",
                                       "01C0005A01C3000501C4000501E5005A01C4000501C3000501C0006401C0FFFF01C1005A01C4000501C3000501C1002801C1002801C3000501C4000501E5FFFF01C2006401C2005A01C3000501C4000501E5005A01C4000501C3000501C2FFFF005E0064005E0064001800640018FFFF005F006400180064005F00640018FFFF",
                                       "01F302BC01FF01F401FD00C801F8FFFE01F407D001FC018E01F9FFFE01F50AED01FD006401FAFFFE01F60C1C01FBFFFE00740B8601FF003201FD00C801FCFFFE01F70D4801FFFFFE",
                                       "0001F30CE401FF089801FD00C801F8FFFE01F411F801FC044A01F9FFFE01F5151501FD012C01FAFFFE01F6164401FBFFFE007415AE01FF00C801FD00C801FCFFFE01F7177001FFFFFE",
                                       "0001F30DAC01FF089801FD00C801F8FFFE01F412C001FC044A01F9FFFE01F515DD01FD012C01FAFFFE01F6170C01FBFFFE0074167601FF00C801FD00C801FCFFFE01F7183801FFFFFE"}

    Private Sub btnPresetTileAnim_Click(sender As System.Object, e As System.EventArgs) Handles btnPresetTileAnim.Click
        If cboTileAnim.SelectedIndex > -1 Then
            Dim preset As String = tileAnimPresets(cboTileAnim.SelectedIndex)
            Dim presetArray(preset.Length \ 2 - 1) As Byte
            For i As Integer = 0 To presetArray.Length - 1
                presetArray(i) = CByte("&H" & preset.Substring(i * 2, 2))
            Next
            UseTileAnim(presetArray)
            cboTileAnim.SelectedIndex = -1
        End If
    End Sub

    Private Sub btnImportTileAnim_Click(sender As System.Object, e As System.EventArgs) Handles btnImportTileAnim.Click
        If openTileAnim.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Try
                Dim data As Byte() = IO.File.ReadAllBytes(openTileAnim.FileName)
                UseTileAnim(data)
            Catch ex As Exception
                MsgBox("Failed to import:" & Environment.NewLine & ex.Message)
            End Try
        End If
    End Sub

    Private Sub UseTileAnim(ByVal data As Byte())
        Dim count As Integer = 0
        For i As Integer = 0 To data.Length - 2 Step 2
            If data(i) * &H100 + data(i + 1) >= &HFFFE Then
                count += 1
            End If
        Next
        tileAnim = New Byte(count * 4 + 4 + data.Length - 1) {}
        For i As Integer = 0 To count - 1
            tileAnim(i * 4) = &H11 'Any value other than 0 would work here
            tileAnim(i * 4 + 1) = &H11
            tileAnim(i * 4 + 2) = &H11
            tileAnim(i * 4 + 3) = &H11
        Next
        tileAnim(count * 4) = 0
        tileAnim(count * 4 + 1) = 0
        tileAnim(count * 4 + 2) = 0
        tileAnim(count * 4 + 3) = 0
        Array.Copy(data, 0, tileAnim, count * 4 + 4, data.Length)

        btnDeleteTileAnim.Enabled = True
        btnExportTileAnim.Enabled = True
    End Sub

    Private Sub addrTiles_Load(sender As Object, e As EventArgs)

    End Sub

    Private Sub AddressUpDown3_Load(sender As Object, e As EventArgs) Handles AddressUpDown3.Load

    End Sub
End Class
