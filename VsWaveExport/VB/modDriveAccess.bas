Attribute VB_Name = "modDriveAccess"

'Go to frmMain

Option Explicit

Private Declare Function CreateFile Lib "Kernel32" Alias "CreateFileA" ( _
        ByVal lpFileName As String, ByVal dwDesiredAccess As Long, _
        ByVal dwShareMode As Long, ByVal lpSecurityAttributes As Long, _
        ByVal dwCreationDisposition As Long, ByVal dwFlagsAndAttributes As Long, _
        ByVal hTemplateFile As Long) As Long

Private Declare Function CloseHandle Lib "Kernel32" (ByVal hObject As Long) As Long

Private Declare Function SetFilePointer Lib "Kernel32" (ByVal hFile As Long, _
        ByVal lDistanceToMove As Long, lpDistanceToMoveHigh As Long, _
        ByVal dwMoveMethod As Long) As Long

Private Declare Function ReadFile Lib "Kernel32" (ByVal hFile As Long, _
        lpBuffer As Any, ByVal nNumberOfBytesToRead As Long, _
        lpNumberOfBytesRead As Long, ByVal lpOverlapped As Long) As Long

Private Declare Function WriteFile Lib "Kernel32" (ByVal hFile As Long, _
        lpBuffer As Any, ByVal nNumberOfBytesToWrite As Long, _
        lpNumberOfBytesWritten As Long, ByVal lpOverlapped As Long) As Long

Private Const GENERIC_READ = &H80000000
Private Const GENERIC_WRITE = &H40000000
Private Const FILE_SHARE_READ = &H1
Private Const FILE_SHARE_WRITE = &H2
Private Const OPEN_EXISTING = 3
Private Const INVALID_HANDLE_VALUE = -1&

Private Const FILE_BEGIN = 0

Private Const BYTES_PER_SECTOR = 512

Public Function ReadDriveOrFile(ByVal Drive As String, ByVal StartSec As Long, ByVal cBytes As Long) As Byte()
    'Drive can be one of the following:
    ' "PHYSICALDRIVEx", "VirDis:PathtoFile", "Project:PathToSongFile", "PathToFile.exe"
    Dim hDevice As Long
    Dim btResult() As Byte
    Dim nRead As Long
    Dim udtPos As LARGE_INTEGER
    
    udtPos = Double2LargeInt(CDbl(StartSec) * BYTES_PER_SECTOR)
    
    If Drive Like "VirDis:*" Then
        hDevice = CreateFile(Mid(Drive, 8), GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
    ElseIf Drive Like "Project:*" Then
        hDevice = CreateFile(Mid(Drive, 9), GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
    ElseIf UCase(Drive) Like "*.EXE" Then
        hDevice = CreateFile(Drive, GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
    Else
        hDevice = CreateFile("\\.\" & Drive, GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
    End If
    
    If hDevice = INVALID_HANDLE_VALUE Then Exit Function
    ReDim btResult(cBytes - 1)
    Call SetFilePointer(hDevice, udtPos.Lowpart, udtPos.Highpart, FILE_BEGIN)
    Call ReadFile(hDevice, btResult(0), UBound(btResult) + 1, nRead, 0&)
    CloseHandle hDevice
    If nRead = UBound(btResult) + 1 Then
        ReadDriveOrFile = btResult
    End If
End Function


Public Sub DirectContinuousRead(ByVal Drive As String, ByVal StartSec As Long, _
        ByVal cBytes As Long, Out() As Byte, ByVal OutOffset As Long)
    Dim hDevice As Long
    Dim nRead As Long
    Dim udtPos As LARGE_INTEGER
    
    udtPos = Double2LargeInt(CDbl(StartSec) * BYTES_PER_SECTOR)
    
    If Drive Like "VirDis:*" Then
        hDevice = CreateFile(Mid(Drive, 8), GENERIC_READ Or GENERIC_WRITE, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
    ElseIf Drive Like "Project:*" Then
        hDevice = CreateFile(Mid(Drive, 9), GENERIC_READ Or GENERIC_WRITE, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
    Else
        hDevice = CreateFile("\\.\" & Drive, GENERIC_READ Or GENERIC_WRITE, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
    End If
    
    If hDevice = INVALID_HANDLE_VALUE Then Exit Sub
    Call SetFilePointer(hDevice, udtPos.Lowpart, udtPos.Highpart, FILE_BEGIN)
    Call ReadFile(hDevice, Out(OutOffset), cBytes, nRead, 0&)
    CloseHandle hDevice
End Sub

#If Not IsStub = 1 Then '8<---------------------------------- This Part is only compiled for VSWaveExport.exe

Public Function DirectWriteDrive(ByVal Drive As String, ByVal StartSec As Long, Data() As Byte) As Boolean
    'Only used for Partition Shuffling
    Dim hDevice As Long
    Dim nWritten As Long
    Dim udtPos As LARGE_INTEGER
    
    udtPos = Double2LargeInt(CDbl(StartSec) * BYTES_PER_SECTOR)
'    If Drive Like "VirDis:*" Then
'        hDevice = INVALID_HANDLE_VALUE
'    ElseIf Drive Like "Project:*" Then
'        hDevice = INVALID_HANDLE_VALUE
'    Else
        hDevice = CreateFile("\\.\" & Drive, GENERIC_READ Or GENERIC_WRITE, FILE_SHARE_READ Or FILE_SHARE_WRITE, 0&, OPEN_EXISTING, 0&, 0&)
'    End If
    
    If hDevice = INVALID_HANDLE_VALUE Then Exit Function
    Call SetFilePointer(hDevice, udtPos.Lowpart, udtPos.Highpart, FILE_BEGIN)
    DirectWriteDrive = WriteFile(hDevice, Data(0), UBound(Data) + 1, nWritten, 0&)
    CloseHandle hDevice
    If nWritten = UBound(Data) + 1 Then
        DirectWriteDrive = True
    End If
End Function

Public Sub ExamineDrive(ByVal Path As String)
    'examines drive and adds roland formatted partitions to the treeview
    'path may be in format "PHYSICALDRIVEx" or "VirDis:File"
    Dim btMasterBootRecord() As Byte
    Dim oPartition As clsPartition
    Dim vPartStart As Variant
    Dim ndDrive As Node
    Dim ndPartition As Node
    Dim btData() As Byte
    Dim sPartName As String
    Dim cPart As Long
    
    btMasterBootRecord = ReadDriveOrFile(Path, 0, 512) 'Read Partitiontable
    If IsArrayDimensioned(btMasterBootRecord) Then
        If btMasterBootRecord(510) = &H55 And btMasterBootRecord(511) = &HAA Then
            sPartName = GetString(btMasterBootRecord, 3, 8)
            
            If sPartName = "Roland  " Then
                'single partitioned roland drive
                Set oPartition = New clsPartition
                With oPartition
                    .PhysicalDrive = Path
                    .FSType = 0
                    .LBAStart = 0
                    .Size = 0
                End With
                Set ndDrive = frmMain.trvDrives.Nodes.Add(, , , Path)
                Set ndPartition = frmMain.trvDrives.Nodes.Add(ndDrive, tvwChild, , "Partition")
                Set ndPartition.Tag = oPartition
                frmMain.trvDrives.Nodes.Add ndPartition, tvwChild, , "Dummy"
                ndDrive.Expanded = True
            Else
                'multi partitioned drive or no roland drive->try read ID string from first Partition of the Drive:
                Set oPartition = New clsPartition
                oPartition.InitPartition Path, btMasterBootRecord, 446 + 16 * 0 '1st Primary Partition
                btData = ReadDriveOrFile(Path, oPartition.LBAStart, 512)
                
                If IsArrayDimensioned(btData) Then
                    sPartName = GetString(btData, 3, 8)
                    
                    If sPartName = "Roland  " Then
                        'PHYSICALDRIVE is Roland HD!
                        'add the drive and it's partitions to the treeview
                        Set ndDrive = frmMain.trvDrives.Nodes.Add(, , , Path)
                        cPart = 0
                        For Each vPartStart In Array( _
                                446 + 16 * 0, 446 + 16 * 1, 446 + 16 * 2, 446 + 16 * 3, _
                                446 - 16 * 4, 446 - 16 * 3, 446 - 16 * 2, 446 - 16 * 1, _
                                446 - 16 * 11, 446 - 16 * 10, 446 - 16 * 9, 446 - 16 * 8)
                            If MakeLong(btMasterBootRecord(vPartStart + 8), btMasterBootRecord(vPartStart + 9), _
                                btMasterBootRecord(vPartStart + 10), btMasterBootRecord(vPartStart + 11)) <> 0 Then
                                Set oPartition = New clsPartition
                                oPartition.InitPartition Path, btMasterBootRecord, vPartStart
                                Set ndPartition = frmMain.trvDrives.Nodes.Add(ndDrive, tvwChild, , "Partition" & cPart)
                                Set ndPartition.Tag = oPartition
                                frmMain.trvDrives.Nodes.Add ndPartition, tvwChild, , "Dummy"
                                cPart = cPart + 1
                            End If
                        Next
                        ndDrive.Expanded = True
                    End If
                End If
            End If
        Else
            'Debug.Print "PHYSICALDRIVE" & x & " no EOP Marker"
        End If
    Else
        'Debug.Print "PHYSICALDRIVE" & x & " not present"
    End If
End Sub



#End If '8<----------------------------------------------------------------------------------------------



