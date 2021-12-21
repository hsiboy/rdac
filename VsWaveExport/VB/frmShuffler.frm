VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmShuffler 
   BorderStyle     =   1  'Fest Einfach
   Caption         =   "Partition Shuffler"
   ClientHeight    =   5445
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5070
   Icon            =   "frmShuffler.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5445
   ScaleWidth      =   5070
   StartUpPosition =   3  'Windows-Standard
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   435
      Left            =   1080
      TabIndex        =   1
      Top             =   4860
      Width           =   1035
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Height          =   435
      Left            =   2580
      TabIndex        =   2
      Top             =   4860
      Width           =   1035
   End
   Begin MSComctlLib.ListView lsvPartitions 
      Height          =   4395
      Left            =   60
      TabIndex        =   0
      Top             =   360
      Width           =   4935
      _ExtentX        =   8705
      _ExtentY        =   7752
      View            =   3
      SortOrder       =   -1  'True
      LabelWrap       =   -1  'True
      HideSelection   =   -1  'True
      Checkboxes      =   -1  'True
      FullRowSelect   =   -1  'True
      GridLines       =   -1  'True
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Lucida Console"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      NumItems        =   2
      BeginProperty ColumnHeader(1) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         Text            =   "Partition"
         Object.Width           =   2805
      EndProperty
      BeginProperty ColumnHeader(2) {BDD1F052-858B-11D1-B16A-00C0F0283628} 
         SubItemIndex    =   1
         Text            =   "LBA Start"
         Object.Width           =   2540
      EndProperty
   End
   Begin VB.Label Label2 
      Caption         =   "Select Partitions you want Windows to be able to access:"
      Height          =   255
      Left            =   60
      TabIndex        =   3
      Top             =   60
      Width           =   4335
   End
End
Attribute VB_Name = "frmShuffler"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private sPath As String
Private btMasterBootRecord() As Byte

Public Sub StartDialog(ByVal Path As String)
    'read MBR and populate ListView
    Dim vPartStart As Variant
    Dim oPartition As clsPartition
    Dim liTemp As ListItem
    Dim x As Long
    
    sPath = Path
    
    lsvPartitions.ListItems.Clear
    lsvPartitions.Sorted = False
    lsvPartitions.SortOrder = lvwDescending
    
    btMasterBootRecord = ReadDriveOrFile(sPath, 0, 512) 'Read Partitiontable
    If IsArrayDimensioned(btMasterBootRecord) Then
        For Each vPartStart In Array( _
                446 + 16 * 0, 446 + 16 * 1, 446 + 16 * 2, 446 + 16 * 3, _
                446 - 16 * 4, 446 - 16 * 3, 446 - 16 * 2, 446 - 16 * 1, _
                446 - 16 * 11, 446 - 16 * 10, 446 - 16 * 9, 446 - 16 * 8)
            If MakeLong(btMasterBootRecord(vPartStart + 8), btMasterBootRecord(vPartStart + 9), _
                btMasterBootRecord(vPartStart + 10), btMasterBootRecord(vPartStart + 11)) > 0 Then
                Set oPartition = New clsPartition
                oPartition.InitPartition Path, btMasterBootRecord, vPartStart
                If x < 4 Then
                    oPartition.Visible = True
                End If
                Set liTemp = lsvPartitions.ListItems.Add(, , "Partition " & Right("0" & x, 2))
                liTemp.SubItems(1) = Right("00000000" & Hex(oPartition.LBAStart), 8)
                Set liTemp.Tag = oPartition
                x = x + 1
            End If
        Next
    End If
    For x = 1 To 4
        lsvPartitions.ListItems(x).Checked = True
    Next
    
    Me.Show vbModal
End Sub

Private Sub cmdCancel_Click()
    Me.Hide
End Sub


Private Sub cmdOK_Click()
    Dim x As Long
    Dim vPartStart As Variant
    Dim bModified As Boolean
    Dim oPartitions() As clsPartition
    Dim oPart As clsPartition
    Dim btData() As Byte
    
    ReDim oPartitions(lsvPartitions.ListItems.Count - 1)
    For x = 1 To lsvPartitions.ListItems.Count
        Set oPart = lsvPartitions.ListItems(x).Tag
        Set oPartitions(x - 1) = oPart
        If lsvPartitions.ListItems(x).Checked Then
            If Not oPart.Visible Then
                bModified = True
            End If
            oPart.Tag = "A" & Right("00000000" & Hex(oPart.LBAStart), 8)    'sort Key
        Else
            oPart.Tag = "Z" & Right("00000000" & Hex(oPart.LBAStart), 8)
        End If
    Next
    QuickSort oPartitions, 0, UBound(oPartitions)
    
    If Not oPartitions(3).Tag Like "A*" Or oPartitions(4).Tag Like "A*" Then
        MsgBox "Select 4 Partitions!", vbExclamation, "Error:"
    ElseIf Not bModified Then
        MsgBox "Your selection is identical to the current partitioning.", vbExclamation, "Error:"
    Else
        'build new MBR:
        x = 0
        For Each vPartStart In Array( _
                446 + 16 * 0, 446 + 16 * 1, 446 + 16 * 2, 446 + 16 * 3, _
                446 - 16 * 4, 446 - 16 * 3, 446 - 16 * 2, 446 - 16 * 1, _
                446 - 16 * 11, 446 - 16 * 10, 446 - 16 * 9, 446 - 16 * 8)
            
            If x <= UBound(oPartitions) Then
                btData = oPartitions(x).PartitionDescriptor
                RtlMoveMemory btMasterBootRecord(vPartStart), btData(0), 16
            End If
                
            x = x + 1
        Next
        If MsgBox("This Operation will write to the Master Boot Record of your harddrive." & vbCrLf & _
            "Are you shure you want to continue?", vbYesNo Or vbQuestion, "Shure?") = vbYes Then
                If DirectWriteDrive(sPath, 0, btMasterBootRecord) Then
                    MsgBox "MBR successfuly written." & vbCrLf & _
                        "You have to reboot your computer now to be able to access the selected Partitions.", vbInformation, "Success"
                    frmMain.ScanDrives
                    Me.lsvPartitions.ListItems.Clear
                    Me.Hide
                Else
                    MsgBox "Failed to write MBR!", vbCritical, "Error:"
                End If
        Else
            MsgBox "Operation canceled.", vbInformation, "Canceled"
        End If
    End If
'    Stop
End Sub

Private Sub lsvPartitions_ColumnClick(ByVal ColumnHeader As MSComctlLib.ColumnHeader)
    lsvPartitions.SortKey = ColumnHeader.Index - 1
   
    If lsvPartitions.SortOrder = lvwAscending Then
        lsvPartitions.SortOrder = lvwDescending
    Else
        lsvPartitions.SortOrder = lvwAscending
    End If
   
    lsvPartitions.Sorted = True
End Sub

Private Sub lsvPartitions_ItemCheck(ByVal Item As MSComctlLib.ListItem)
    Dim x As Long
    Dim cChecked As Long, pOldestChecked As Long
    
    Static cCheckClicks As Long
    Dim cMin As Long
    
    'allow only 4 items checked
    cMin = 2 ^ 31 - 1
    For x = 1 To lsvPartitions.ListItems.Count
        If lsvPartitions.ListItems(x).Checked Then
            If lsvPartitions.ListItems(x).Tag.CheckedCount = 0 Or lsvPartitions.ListItems(x) Is Item Then
                cCheckClicks = cCheckClicks + 1
                lsvPartitions.ListItems(x).Tag.CheckedCount = cCheckClicks
            End If
            If lsvPartitions.ListItems(x).Tag.CheckedCount < cMin Then   'im tag ist die clsPartition
                cMin = lsvPartitions.ListItems(x).Tag.CheckedCount
                pOldestChecked = x
            End If
                
            cChecked = cChecked + 1
        End If
    Next
    
    If cChecked > 4 Then
        lsvPartitions.ListItems(pOldestChecked).Checked = False
    End If
End Sub

Private Sub QuickSort(AnArray() As clsPartition, ByVal Low As Long, ByVal High As Long)
    Dim p1 As Long, p2 As Long, sRef As String, oTemp As Object

    p1 = Low
    p2 = High
    sRef = AnArray((p1 + p2) / 2).Tag
    
    Do
        Do While (AnArray(p1).Tag < sRef)
            p1 = p1 + 1
        Loop
 
        Do While (AnArray(p2).Tag > sRef)
            p2 = p2 - 1
        Loop

        If p1 <= p2 Then
            Set oTemp = AnArray(p1)
            Set AnArray(p1) = AnArray(p2)
            Set AnArray(p2) = oTemp
            
            p1 = p1 + 1
            p2 = p2 - 1
        End If
    Loop Until (p1 > p2)

    If Low < p2 Then Call QuickSort(AnArray, Low, p2)
    If p1 < High Then Call QuickSort(AnArray, p1, High)
End Sub


