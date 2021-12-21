VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsPartition"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private Declare Function CastFatC Lib "RDAC.dll" Alias "CastFat" ( _
        pIn8 As Byte, pOut32 As Long, ByVal cEntries As Long, ByVal FatType As Long) As Long


Public LBAStart As Long
Public Size As Long
Public FSType As Byte
Private sPhysicalDrive As String

Public PartitionName As String
Public FatType As String

Public BytesPerSector As Long
Public SectorsPerCluster As Long
Public NumberOfReservedSectors As Long
Public ClusterStartSector As Long

Private cFATs As Long
Private cSectorsPerFat As Long
Public FATStartSector As Long
Private lRootDirStart As Long

Private lFat() As Long  'FAT32

'-------------------------------------
Public Files As Collection 'only used if BypassDirectDiskAccess=true, holds a Filenames to FirstCluster Translation Table
Public pWrite As Long 'only used if BypassDirectDiskAccess=true, pointer to the next free Cluster

Public Root As clsDirectoryEntry

Private btPartitionDescriptor() As Byte 'comes from the MBR

Public Visible As Boolean   'for Partition shuffler
Public CheckedCount As Long
Public IsSwappedIDE As Boolean

Public Tag As Variant



Public Sub ClearFat() 'only used if BypassDirectDiskAccess=true
    ReDim lFat(349567)
    pWrite = 2
    lFat(0) = &HFFFFFF7: lFat(1) = &HFFFFFF7
    Set Files = New Collection
End Sub

Public Sub ReserveCluster(Value As Long)
    Do While Not lFat(pWrite) = 0
        pWrite = pWrite + 1
    Loop
    lFat(pWrite) = Value
    Do While Not lFat(pWrite) = 0
        pWrite = pWrite + 1
    Loop
End Sub

Public Function GetFirstClusterOfFile(FileName As String) As Long
    Dim lReturn As Long
    On Error Resume Next
    lReturn = Files(FileName)
    If Err.Number > 0 Then
        lReturn = 0
    End If
    On Error GoTo 0
    GetFirstClusterOfFile = lReturn
End Function


Public Function InitPartition(PhysicalDrive As String, AnArray() As Byte, ByVal Offset As Long)
    'legt für jede Partion den Startblock und Größe fest
    sPhysicalDrive = PhysicalDrive
    FSType = AnArray(Offset + 4)
    LBAStart = MakeLong(AnArray(Offset + 8), AnArray(Offset + 9), AnArray(Offset + 10), AnArray(Offset + 11))
    Size = MakeLong(AnArray(Offset + 12), AnArray(Offset + 13), AnArray(Offset + 14), AnArray(Offset + 15))
    ReDim btPartitionDescriptor(15)
    RtlMoveMemory btPartitionDescriptor(0), AnArray(Offset), 16
End Function

Property Get PhysicalDrive() As String
    PhysicalDrive = sPhysicalDrive
End Property

Property Let PhysicalDrive(Value As String)
    sPhysicalDrive = Value
End Property

Property Get PartitionDescriptor() As Byte()
    PartitionDescriptor = btPartitionDescriptor
End Property

Public Sub ReadPartition()
    Dim btData() As Byte
    Dim cTotSec As Long
    Dim cClusters As Long
    Dim cRootDirSectors As Long
    Dim cRootEntries As Long
    
    'read id
    btData = ReadDriveOrFile(sPhysicalDrive, LBAStart, 512)

    If Not IsArrayDimensioned(btData) Then
        Err.Raise 513, , "Error reading Partition."
    End If

    PartitionName = Space(4)
    RtlMoveMemory ByVal StrPtr(PartitionName), btData(3), 8
    PartitionName = StrConv(PartitionName, vbUnicode)
    BytesPerSector = btData(11) + btData(12) * &H100&   'BPB_BytsPerSec
    SectorsPerCluster = btData(13)                      'BPB_SecPerClus
    NumberOfReservedSectors = btData(14) + btData(15) * &H100&  'BPB_RsvdSecCnt
    cFATs = btData(16)                                  'BPB_NumFATs
    cRootEntries = btData(17) + btData(18) * &H100&     'BPB_RootEntCnt
    
    'FAT Partition?
    If BytesPerSector <> 512 Or cFATs <> 2 Or _
         btData(510) <> &H55 Or btData(511) <> &HAA Then
         Err.Raise 513, , "No FAT Partition."
    End If

    'FAT Type gemäss fatgen10.doc by Microsoft ermitteln:
    If btData(22) + btData(23) * &H100& <> 0 Then
        cSectorsPerFat = btData(22) + btData(23) * &H100&
    Else
        cSectorsPerFat = MakeLong(btData(36), btData(37), btData(38), btData(39))
    End If
    
    If btData(19) + btData(20) * &H100& <> 0 Then
        cTotSec = btData(19) + btData(20) * &H100&
    Else
        cTotSec = MakeLong(btData(32), btData(33), btData(34), btData(35))
    End If
    
    cRootDirSectors = ((cRootEntries * 32) + (BytesPerSector - 1)) / BytesPerSector
    cClusters = (cTotSec - (NumberOfReservedSectors + (cFATs * cSectorsPerFat) + cRootDirSectors)) / SectorsPerCluster
    
    If (cClusters < 4085) Then
        FatType = "FAT12"
    ElseIf (cClusters < 65525) Then
        FatType = "FAT16"
    Else
        FatType = "FAT32"
    End If
    'soviel zu microsoft
    
    Select Case FatType
    Case "FAT16", "FAT12"
        FATStartSector = LBAStart + NumberOfReservedSectors
        lRootDirStart = FATStartSector + cFATs * cSectorsPerFat   'Root dir fängt in diesem Sektor an
        'die Cluster fangen in diesem sektor an:
        ClusterStartSector = lRootDirStart + cRootDirSectors - 1
    Case "FAT32"
        'FAT32:
        FATStartSector = LBAStart + NumberOfReservedSectors
        lRootDirStart = MakeLong(btData(44), btData(45), btData(46), btData(47))
        ClusterStartSector = FATStartSector + cFATs * cSectorsPerFat
    End Select
    
    'read 1st fat and convert to virtual fat 32
    ReDim btData(cSectorsPerFat * BytesPerSector - 1)
    btData = ReadDriveOrFile(sPhysicalDrive, FATStartSector, cSectorsPerFat * BytesPerSector)
    Select Case FatType
    Case "FAT12"
        ReDim lFat(Int((cSectorsPerFat * BytesPerSector) / 1.5) - 1)  'FAT auf 32 bit konvertiert
        CastFat btData, lFat, 12
    Case "FAT16"
        ReDim lFat((cSectorsPerFat * BytesPerSector) / 2 - 1)  'FAT auf 32 bit konvertiert
        CastFat btData, lFat, 16
    Case "FAT32"
        ReDim lFat((cSectorsPerFat * BytesPerSector) / 4 - 1)  'FAT auf 32 bit konvertiert
        CastFat btData, lFat, 32
    End Select
    
    'root directory lesen und Fileliste aufbauen
    Set Root = New clsDirectoryEntry
    With Root
        Set .MotherPartition = Me
        .Attributes = 16    'Folder
        .ClusterStart = lRootDirStart
        
        Select Case FatType
        Case "FAT16", "FAT12"
            .ShortFilename = "ROOT16"
            .Size = ClusterStartSector - lRootDirStart

        Case "FAT32"
            .ShortFilename = "ROOT"
            '.Size =?
        Case Else
            Stop
        End Select
        .ReadSubEntries
    End With
End Sub

Property Get Fat() As Long()
    Fat = lFat
End Property

Public Function ReadSecondFAT() As Long()
    'read 2nd fat and convert to virtual fat 32
    Dim btData() As Byte
    Dim lFat2() As Long
    
    ReDim btData(cSectorsPerFat * BytesPerSector - 1)
    btData = ReadDriveOrFile(sPhysicalDrive, FATStartSector + cSectorsPerFat, cSectorsPerFat * BytesPerSector)
    Select Case FatType
    Case "FAT12"
        ReDim lFat2(Int((cSectorsPerFat * BytesPerSector) / 1.5) - 1)  'FAT auf 32 bit konvertiert
        CastFat btData, lFat2, 12
    Case "FAT16"
        ReDim lFat2((cSectorsPerFat * BytesPerSector) / 2 - 1)  'FAT auf 32 bit konvertiert
        CastFat btData, lFat2, 16
    Case "FAT32"
        ReDim lFat2((cSectorsPerFat * BytesPerSector) / 4 - 1)  'FAT auf 32 bit konvertiert
        CastFat btData, lFat2, 32
    End Select
    ReadSecondFAT = lFat2
End Function

Public Sub CleanUp()
    'Zirkelreferenzen auflösen
    If Not Root Is Nothing Then
        Root.CleanUp
    End If
End Sub

Property Get BytesPerCluster() As Long
    BytesPerCluster = BytesPerSector * SectorsPerCluster
End Property



Private Sub CastFat(RawIn() As Byte, Out() As Long, TypeIn As Long)
    Dim x As Long
    
#If True Then
    CastFatC RawIn(0), Out(0), UBound(Out) + 1, TypeIn  'C doping
#Else
    Select Case TypeIn
    Case 12
        For x = 0 To UBound(Out)
            If x Mod 2 = 0 Then
                Out(x) = RawIn(x / 2 * 3) Or ((RawIn(x / 2 * 3 + 1) And &HF&) * &H100&)
            Else
                Out(x) = RawIn((x - 1) / 2 * 3 + 2) * &H10& Or ((RawIn((x - 1) / 2 * 3 + 1) And &HF0&) \ &H10)
            End If
            If Out(x) >= &HFF7& Then
                Out(x) = Out(x) Or &HFFFF000    'invalid (fehlerhaft, ende, ...)
            End If
        Next
    Case 16
        For x = 0 To UBound(Out)
            Out(x) = RawIn(x * 2) + RawIn(x * 2 + 1) * &H100&
            If Out(x) >= &HFFF7& Then
                Out(x) = Out(x) Or &HFFF0000    'invalid (fehlerhaft, ende, ...)
            End If
        Next
    Case 32
        For x = 0 To UBound(Out)
            Out(x) = MakeLong(RawIn(x * 4), RawIn(x * 4 + 1), RawIn(x * 4 + 2), RawIn(x * 4 + 3))
            If Out(x) >= &HFFFFF7 Then
                Out(x) = Out(x) Or &HFFF0000    'invalid (fehlerhaft, ende, ...)
            End If
        Next
    Case Else
        Stop
    End Select
#End If
End Sub
