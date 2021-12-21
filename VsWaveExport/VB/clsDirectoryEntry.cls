VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsDirectoryEntry"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public MotherPartition As clsPartition

Public Path As String
Public ShortFilename As String
Public Attributes As Byte
Public ClusterStart As Long
Public Size As Long
Public DateWritten As Date

Public Deleted As Boolean

Private colClusterChain As New Collection

Private colSubEntries As Collection 'nur bei Folder verwendet

Property Get IsFolder() As Boolean
    IsFolder = (Attributes And 16) = 16
End Property

Public Sub BuildClusterChain()
    Dim lCurrCluster As Long
    Dim lFat() As Long

    If MotherPartition Is Nothing Then
        Err.Raise 513, , "Not initialized!"
    End If
    
    If Not Me.Deleted Then
        Set colClusterChain = New Collection
        lCurrCluster = ClusterStart
        lFat = MotherPartition.Fat
        Do While Not lCurrCluster >= &HFFFFFF7  'end signal
            If lCurrCluster > UBound(lFat) Then
Debug.Print "Invalid ClusterNumber " & lCurrCluster & " in File " & Me.FullName & "!"
                Exit Do
            Else
                colClusterChain.Add lCurrCluster
                lCurrCluster = lFat(lCurrCluster) And &HFFFFFFF
                If colClusterChain.Count > 131072 Then
Debug.Print Me.ShortFilename & " contains cluster loop!"
                    Exit Do
                End If
            End If
        Loop
       
        'sanity check
        If Size > 0 And colClusterChain.Count * CDbl(MotherPartition.BytesPerCluster) < Size Then
            Debug.Print "Stored Size of Entry " & Me.ShortFilename & " exceeds calculated Filesize!"
        End If
    End If
End Sub

Public Sub ReadSubEntries()
    Dim btData() As Byte
    Dim vCluster As Variant
    Dim pData As Long
    Dim x As Long
    Dim oSubEntry As clsDirectoryEntry
    Dim lBytesLeft
    Dim s As String

    If Not Me.IsFolder Then Err.Raise 513, , "Not a Folder!"
    
    If Not Me.Deleted Then
        If Me.ShortFilename = "ROOT16" Then
            btData = ReadDriveOrFile(MotherPartition.PhysicalDrive, Me.ClusterStart, Me.Size * MotherPartition.BytesPerSector)
        Else
            If colClusterChain.Count = 0 Then
                BuildClusterChain
            End If
            
            ReDim btData(colClusterChain.Count * MotherPartition.BytesPerCluster - 1)
            pData = 0
            For Each vCluster In colClusterChain
                DirectContinuousRead MotherPartition.PhysicalDrive, _
                    MotherPartition.ClusterStartSector + (vCluster - 2) * MotherPartition.SectorsPerCluster, _
                    MotherPartition.BytesPerCluster, btData, pData
                pData = pData + MotherPartition.BytesPerCluster
            Next
        End If
        If IsArrayDimensioned(btData) Then
            If MotherPartition.IsSwappedIDE Then
                SwapBytes btData(0), UBound(btData) + 1
            End If
            ParseDirectory btData
        Else
            MsgBox "Error reading Directory!", vbCritical, "Error:"
        End If
    End If
End Sub

Property Get SubEntryByName(ByVal EntryName As String) As clsDirectoryEntry
    Dim oSubEntry As clsDirectoryEntry
    
    If colSubEntries Is Nothing Then
        Me.ReadSubEntries
    End If
    For Each oSubEntry In colSubEntries
        If UCase(oSubEntry.ShortFilename) Like UCase(EntryName) Then
            Set SubEntryByName = oSubEntry
            Exit For
        End If
    Next
End Property

Public Sub CleanUp()
    'Zirkelreferenzen auflösen
    Dim oSubEntry As clsDirectoryEntry
    Set MotherPartition = Nothing
    If Not colSubEntries Is Nothing Then
        For Each oSubEntry In colSubEntries
            oSubEntry.CleanUp
        Next
    End If
End Sub

Property Get SubEntries() As Collection
    If Not Me.IsFolder Then
        Err.Raise 513, , "No folder!"
    End If
    
    If colSubEntries Is Nothing Then ReadSubEntries
    
    Set SubEntries = colSubEntries
    
End Property

Property Get ClusterChain() As Collection
    Set ClusterChain = colClusterChain
End Property

Public Sub SetCorrectedClusterChain(NewClusterChain As Collection)
    Set colClusterChain = NewClusterChain
End Sub

Private Function ParseDirectory(btData() As Byte)
    Dim x As Long
    Dim oSubEntry As clsDirectoryEntry
    Dim s As String
    
    If Not colSubEntries Is Nothing Then
        For Each oSubEntry In colSubEntries
            oSubEntry.CleanUp
        Next
    End If
    Set colSubEntries = New Collection
    
    For x = 0 To (UBound(btData)) Step 32
        If btData(x) = 0 Then
            Exit For
        Else
            Set oSubEntry = New clsDirectoryEntry
            With oSubEntry
                .Path = IIf(Me.Path <> "", Me.Path & "\", "") & Me.ShortFilename
                If btData(x) = &HE5 Then
                    .Deleted = True
                    btData(x) = Asc("?")
                Else
                    .Deleted = Me.Deleted
                End If
                Set .MotherPartition = Me.MotherPartition
                s = Space(11)
                RtlMoveMemory ByVal StrPtr(s), btData(x), 11
                .ShortFilename = StrConv(s, vbUnicode)
                .ShortFilename = Left(.ShortFilename, 11)
                
                .ShortFilename = Trim(Left(.ShortFilename, 8)) & "." & Trim(Right(.ShortFilename, 3))
                
                Dim lTemp As Long
                lTemp = btData(x + 24) + btData(x + 25) * &H100&
                .DateWritten = DateAdd("YYYY", (lTemp And &HFE00) / 512, DateSerial(1980, 1, 1))
                .DateWritten = DateAdd("M", (lTemp And &H1E0) / 64, .DateWritten)
                .DateWritten = DateAdd("D", (lTemp And &H1F), .DateWritten)
                lTemp = btData(x + 22) + btData(x + 23) * &H100&
                .DateWritten = DateAdd("H", (lTemp And &HF800) / &H800, .DateWritten)
                .DateWritten = DateAdd("n", (lTemp And &H7E0) / 32, .DateWritten)
                .DateWritten = DateAdd("s", (lTemp And &H1F), .DateWritten)
                .Attributes = btData(x + 11)
                .ClusterStart = MakeLong(btData(x + 26), btData(x + 27), btData(x + 20), btData(x + 21))
                .Size = MakeLong(btData(x + 28), btData(x + 29), _
                    btData(x + 30), btData(x + 31))
            End With
            If Not (oSubEntry.Attributes And &HF) = &HF Then  'Long Filename?
                colSubEntries.Add oSubEntry
            Else
'                Stop 'lange filenamen!
            End If
        End If
    Next
End Function

Property Get FullName() As String
    FullName = IIf(Path <> "", Path & "\", "") & ShortFilename
End Property

Property Get Parent() As clsDirectoryEntry
    Dim sFolders() As String
    Dim oEntry As clsDirectoryEntry
    Dim x As Long
    
    sFolders = Split(Path, "\")
    Set oEntry = MotherPartition.Root
    For x = 1 To UBound(sFolders)
        Set oEntry = oEntry.SubEntryByName(sFolders(x))
    Next
    Set Parent = oEntry
End Property

Property Get FileData() As Byte()
    'Take care! Only for short files! Trys to return the _whole_ file!
    'Could be alot in case of a huge file, and therefore may cause deadlock!
    Dim vCluster As Variant
    Dim btData() As Byte
    Dim pData As Long
    
    If Me.IsFolder Then
        Err.Raise 513, , "Not a File!"
    ElseIf Me.Deleted Then
        Err.Raise 513, , "Deleted File!"
    End If
    
    If colClusterChain.Count = 0 Then
        BuildClusterChain
    End If

    ReDim btData(colClusterChain.Count * MotherPartition.BytesPerCluster - 1)
    pData = 0
    For Each vCluster In colClusterChain
        DirectContinuousRead MotherPartition.PhysicalDrive, _
            MotherPartition.ClusterStartSector + (vCluster - 2) * MotherPartition.SectorsPerCluster, _
            MotherPartition.BytesPerCluster, btData, pData
        pData = pData + MotherPartition.BytesPerCluster
    Next
    If MotherPartition.IsSwappedIDE Then
        SwapBytes btData(0), UBound(btData) + 1
    End If

    ReDim Preserve btData(Me.Size - 1)
    FileData = btData
End Property

Property Get ClusterData(ClusterIndex As Long) As Byte()
    'this is the better way to access large files.
    Dim btData() As Byte
    
    If colClusterChain.Count = 0 Then
        BuildClusterChain
    End If
    If ClusterIndex > colClusterChain.Count Or ClusterIndex < 1 Then
        Err.Raise 9
    End If
    btData = ReadDriveOrFile(MotherPartition.PhysicalDrive, _
        MotherPartition.ClusterStartSector + (colClusterChain(ClusterIndex) - 2) * MotherPartition.SectorsPerCluster, _
        MotherPartition.BytesPerCluster)
    If MotherPartition.IsSwappedIDE Then
        SwapBytes btData(0), UBound(btData) + 1
    End If
    ClusterData = btData
End Property
