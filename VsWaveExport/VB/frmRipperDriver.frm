VERSION 5.00
Begin VB.Form frmRipperDriver 
   BorderStyle     =   1  'Fest Einfach
   Caption         =   "Launch Bears Ripper Software"
   ClientHeight    =   1320
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4785
   Icon            =   "frmRipperDriver.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1320
   ScaleWidth      =   4785
   StartUpPosition =   3  'Windows-Standard
   Begin VB.CommandButton cmdStart 
      Caption         =   "Start"
      Height          =   495
      Left            =   2400
      TabIndex        =   2
      Top             =   720
      Width           =   2295
   End
   Begin VB.CheckBox chkOverwrite 
      Caption         =   "Overwrite files each time"
      Height          =   255
      Left            =   60
      TabIndex        =   1
      Top             =   840
      Value           =   1  'Aktiviert
      Width           =   2235
   End
   Begin VB.TextBox txtCmd 
      Height          =   315
      Left            =   60
      TabIndex        =   0
      Text            =   "cd2roland D [path]"
      Top             =   300
      Width           =   4635
   End
   Begin VB.Label Label1 
      Caption         =   "Commandline:"
      Height          =   195
      Left            =   60
      TabIndex        =   3
      Top             =   60
      Width           =   3135
   End
End
Attribute VB_Name = "frmRipperDriver"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public RipperSourcePath As String
Public RipperDestPath As String
Public RipperDestFolder As String

Public Sub StartDialog()
    Dim sExistingDirs() As String
    Dim x As Long
    
    If chkOverwrite.Value = vbChecked Then
        RipperDestFolder = "Rip0000"
    Else
        sExistingDirs = ListDir(RipperDestPath, "Rip????", False)
        If IsArrayDimensioned(sExistingDirs) Then
            For x = 0 To &HFFFF&
                If Not DirExists(EnsureBackSlash(RipperDestPath) & "Rip" & Right("000" & Hex(x), 4)) Then
                    Exit For
                End If
            Next
        End If
        RipperDestFolder = "Rip" & Right("000" & Hex(x), 4)
    End If
    
    txtCmd.Text = "cd2roland " & RipperSourcePath & " """ & EnsureBackSlash(RipperDestPath) & RipperDestFolder & """"
    Me.Show vbModal
End Sub


Private Sub cmdStart_Click()
    Dim sSongFolders() As String
    Dim sSongFiles() As String
    Dim lSong As Long
    Dim oProject As clsVSProject
    Dim ndProject As Node
    Dim iFNr As Integer
    Dim vTemp As Variant
    Dim sBatchName As String

    'read paths from textbox
    vTemp = Split(txtCmd, """")
    If UBound(vTemp) Mod 2 = 1 Then
        'odd number of quotes
        MsgBox "Error in Command String!", vbExclamation, "Error:"
        Exit Sub
    ElseIf UBound(vTemp) = 4 Then
        RipperSourcePath = vTemp(1)
        RipperDestPath = Left(vTemp(3), InStrRev(vTemp(3), "\") - 1)
        RipperDestFolder = Replace(vTemp(3), RipperDestPath, "")
    ElseIf UBound(vTemp) = 2 Then
        RipperSourcePath = Split(vTemp(0), " ")(1)
        RipperDestPath = Left(vTemp(1), InStrRev(vTemp(1), "\") - 1)
        RipperDestFolder = Replace(vTemp(1), RipperDestPath & "\", "")
    Else
        MsgBox "Error in Command String!", vbExclamation, "Error:"
        Exit Sub
    End If
    
    sBatchName = EnsureBackSlash(RipperDestPath) & "Rip.bat"
    If FileExists(sBatchName) Then
        Kill sBatchName
    End If
    
    If Not DirExists(RipperDestPath) Then
        If MsgBox("The target folder does not exist!" & vbCrLf & _
            "This folder is required. Create it?", vbQuestion Or vbYesNo) = vbYes Then
            If Not CreatePath(RipperDestPath) Then
                MsgBox "Can not create path " & RipperDestPath & "!", vbCritical, "Error:"
                Exit Sub
            End If
        Else
            MsgBox "Can not create batch file " & sBatchName & "!", vbCritical, "Error:"
            Exit Sub
        End If
    End If
    
    iFNr = FreeFile
    Open sBatchName For Binary As iFNr
    Put iFNr, , "@echo off" & vbCrLf & txtCmd.Text & vbCrLf & "pause"
    Close iFNr
    
    If DirExists(EnsureBackSlash(RipperDestPath) & RipperDestFolder) And chkOverwrite.Value = True Then
        DeleteDir EnsureBackSlash(RipperDestPath) & RipperDestFolder
    End If
    
    txtCmd.Enabled = False
    cmdStart.Enabled = False
    chkOverwrite.Enabled = False
    ShellWait Environ("ComSpec") & " /c """ & sBatchName & """", True
    
    sSongFolders = ListDir(RipperDestPath & "\" & RipperDestFolder, "SONG*", False)
    If IsArrayDimensioned(sSongFolders) Then
        For lSong = 0 To UBound(sSongFolders)
            sSongFiles = ListDir(sSongFolders(lSong), "SONG.???")
            If IsArrayDimensioned(sSongFiles) Then
                Set oProject = New clsVSProject
                oProject.SongFile = sSongFiles(0)
                With oProject
                    Set ndProject = frmMain.trvDrives.Nodes.Add(, , , _
                        "Project: " & .Name & ", " & .Format & ", " & .Samplerate & "Hz, Folder: " & .FolderName)
                    Set ndProject.Tag = oProject
                End With
            Else
                MsgBox "No SONG file found in folder " & sSongFolders(lSong) & "!", vbExclamation, "Error:"
            End If
        Next
    Else
        MsgBox "No created Song Folders found!", vbExclamation, "Error:"
    End If
    
    txtCmd.Enabled = True
    cmdStart.Enabled = True
    chkOverwrite.Enabled = True
    Me.Hide
End Sub
