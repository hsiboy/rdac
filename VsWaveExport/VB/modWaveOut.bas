Attribute VB_Name = "modWaveOut"
Option Explicit

Private Declare Function waveOutGetNumDevs Lib "winmm.dll" () As Long

Private Declare Function waveOutGetDevCaps Lib "winmm.dll" Alias "waveOutGetDevCapsA" ( _
        ByVal uDeviceID As Long, lpCaps As Any, ByVal uSize As Long) As Long

Private Declare Function waveOutOpen Lib "winmm.dll" (hWaveOut As Long, _
        ByVal uDeviceID As Long, Format As Any, ByVal dwCallback As Long, _
        ByVal dwInstance As Long, ByVal dwFlags As Long) As Long

Private Declare Function waveOutPrepareHeader Lib "winmm.dll" ( _
        ByVal hWaveOut As Long, lpWaveInHdr As Any, ByVal uSize As Long) As Long

Private Declare Function waveOutWrite Lib "winmm.dll" (ByVal hWaveOut As Long, _
        lpWaveOutHdr As Any, ByVal uSize As Long) As Long

Private Declare Function waveOutRestart Lib "winmm.dll" (ByVal hWaveOut As Long) As Long

Private Declare Function waveOutReset Lib "winmm.dll" (ByVal hWaveOut As Long) As Long

Private Declare Function waveOutUnprepareHeader Lib "winmm.dll" ( _
        ByVal hWaveOut As Long, lpWaveInHdr As Any, ByVal uSize As Long) As Long

Private Declare Function waveOutClose Lib "winmm.dll" (ByVal hWaveOut As Long) As Long

Private Declare Sub Sleep Lib "kernel32.dll" (ByVal dwMilliseconds As Long)

Private Const MAXPNAMELEN As Long = 32
Private Const WAVE_FORMAT_PCM As Long = 1
Private Const CALLBACK_FUNCTION As Long = &H30000
Private Const INVALID_HANDLE_VALUE As Long = -1
Private Const WAVE_MAPPER As Long = -1
Private Const WOM_DONE As Long = &H3BD

Private Type WAVEOUTCAPS
    wMid                As Integer
    wPid                As Integer
    vDriverVersion      As Long
    szPname(MAXPNAMELEN - 1) As Byte
    dwFormats           As Long
    wChannels           As Integer
    dwSupport           As Long
End Type

Private Type WAVEFORMATEX
    wFormatTag          As Integer
    nChannels           As Integer
    nSamplesPerSec      As Long
    nAvgBytesPerSec     As Long
    nBlockAlign         As Integer
    wBitsPerSample      As Integer
    cbSize              As Integer
End Type

Private Type WAVEHDR
    lpData              As Long
    dwBufferLength      As Long
    dwBytesRecorded     As Long
    dwUser              As Long
    dwFlags             As Long
    dwLoops             As Long
    lpNext              As Long
    Reserved            As Long
End Type

Private hDevice As Long

Private udtBuffer1Hdr As WAVEHDR
Private btWaveOutBuffer1() As Byte

Private udtBuffer2Hdr As WAVEHDR
Private btWaveOutBuffer2() As Byte


Private lBufferFinished As Long

Private bStopFlag As Boolean

Private cbUsedInCluster As Long
Private cSamplesPerRdacFrame As Long
Private cBytesPerRdacFrame As Long
Private lBitDepth As Long
Private cSamplesPerCluster As Long
Private lFormatCode As Long

Private lWaveData() As Long

Public Sub PlayClusters(DevID As Long, Samplerate As Long, Format As String, _
        Partition As clsPartition, ClusterChain As Collection)
    Dim pRead As Long
    Dim btData() As Byte
    
    cSamplesPerRdacFrame = 16
    Select Case Format
    Case "M16"
        lFormatCode = 3
        cBytesPerRdacFrame = 32
        lBitDepth = 16
    Case "MTP"
        lFormatCode = 5
        cBytesPerRdacFrame = 16
        lBitDepth = 24
    Case "M24"
        lFormatCode = 8
        cBytesPerRdacFrame = 48
        lBitDepth = 24
    Case "MT1"
        lFormatCode = 0
        cBytesPerRdacFrame = 16
        lBitDepth = 16
    Case "MT2"
        lFormatCode = 1
        cBytesPerRdacFrame = 12
        lBitDepth = 16
    Case Else
        MsgBox Format & ": unsupported Format!", vbExclamation, "Error:"
        Exit Sub
    End Select
    cbUsedInCluster = ((Partition.BytesPerCluster \ cBytesPerRdacFrame) And Not 1) * cBytesPerRdacFrame
    cSamplesPerCluster = cbUsedInCluster / cBytesPerRdacFrame * cSamplesPerRdacFrame
    ReDim lWaveData(cSamplesPerCluster - 1)
    
    OpenOutputDevice DevID, Samplerate
    If Not hDevice = INVALID_HANDLE_VALUE Then
        bStopFlag = False
        lBufferFinished = 0
        pRead = 1
        Do While Not bStopFlag
            If (lBufferFinished And 1) = 0 Then
                If pRead <= ClusterChain.Count Then
                    ReadClusterAs16bitWave Partition, ClusterChain(pRead), btWaveOutBuffer1
                    waveOutWrite hDevice, udtBuffer1Hdr, LenB(udtBuffer1Hdr)
                    lBufferFinished = lBufferFinished Or udtBuffer1Hdr.dwUser
                    If pRead > 1 Then
                        frmAnalysis.PlayCallback ClusterChain(pRead - 1) 'currently playing:2
                    End If
                    pRead = pRead + 1
                Else
                    frmAnalysis.PlayCallback ClusterChain(pRead - 1) 'currently playing:2
                End If
            End If
            If (lBufferFinished And 2) = 0 Then
                If pRead <= ClusterChain.Count Then
                    ReadClusterAs16bitWave Partition, ClusterChain(pRead), btWaveOutBuffer2
                    waveOutWrite hDevice, udtBuffer2Hdr, LenB(udtBuffer2Hdr)
                    lBufferFinished = lBufferFinished Or udtBuffer2Hdr.dwUser
                    If pRead > 1 Then
                        frmAnalysis.PlayCallback ClusterChain(pRead - 1) 'currently playing:1
                    End If
                    pRead = pRead + 1
                Else
                    frmAnalysis.PlayCallback ClusterChain(pRead - 1) 'currently playing:2
                End If
            End If
            If lBufferFinished = 0 Then
                bStopFlag = True 'all clusters played
            Else
                Sleep 100
                DoEvents
            End If
        Loop
        CloseOutputDevice
    Else
        MsgBox "Unable to open output device!", vbExclamation, "Error:"
    End If
    frmAnalysis.PlayCallback 0
    Erase lWaveData
    Erase btWaveOutBuffer1
    Erase btWaveOutBuffer2
End Sub

Public Sub StopClusterPlayback()
    bStopFlag = True
End Sub

Private Sub ReadClusterAs16bitWave(Partition As clsPartition, Index As Long, Target() As Byte)
    Dim btData() As Byte
    Static d0 As Long
    
    btData = ReadDriveOrFile(Partition.PhysicalDrive, _
        (Partition.ClusterStartSector + (Index - 2) * Partition.SectorsPerCluster), _
        Partition.BytesPerCluster)
    If Not IsArrayDimensioned(btData) Then
        Out "Error accessing Cluster " & Index & "!"
        ReDim btData(Partition.BytesPerCluster - 1)
    ElseIf Partition.IsSwappedIDE Then
        SwapBytes btData(0), UBound(btData) + 1
    End If
    
    Decode d0, btData(0), lWaveData(0), UBound(lWaveData) + 1, lFormatCode
    d0 = lWaveData(UBound(lWaveData))
    Select Case lBitDepth
    Case 16
        ShrinkTo16 lWaveData(0), Target(0), cSamplesPerCluster
    Case 24
        'convert to 16 bit for playback
        ShrinkTo16 ByVal VarPtr(lWaveData(0)) + 1, Target(0), cSamplesPerCluster
    End Select
End Sub

Public Function GetOutDeviceNames() As String()
    Dim cDevices As Long
    Dim udtCapabilities As WAVEOUTCAPS
    Dim x As Long
    Dim sReturn As String
    
    cDevices = waveOutGetNumDevs
    sReturn = "Default"
    For x = 0 To cDevices - 1
        If waveOutGetDevCaps(x, udtCapabilities, LenB(udtCapabilities)) <> 0 Then
            MsgBox "Error accessing audio device ID: " & x, vbExclamation, "Error:"
        Else
            sReturn = sReturn & vbCrLf & Left(StrConv(udtCapabilities.szPname, vbUnicode), InStrB(udtCapabilities.szPname, ChrB(0)) - 1)
        End If
    Next
    GetOutDeviceNames = Split(sReturn, vbCrLf)
End Function

Private Sub OpenOutputDevice(DevID As Long, Samplerate As Long)
    Dim udtWaveFormat As WAVEFORMATEX
    
    With udtWaveFormat
        .cbSize = 0
        .wFormatTag = WAVE_FORMAT_PCM
        .nChannels = 1
        .nSamplesPerSec = Samplerate
        .wBitsPerSample = 16    'always 16, as some soundcards may not play 24bit
        .nBlockAlign = .wBitsPerSample / 8
        .nAvgBytesPerSec = Samplerate * .nBlockAlign
    End With
    
    If waveOutOpen(hDevice, DevID, udtWaveFormat, AddressOf CallBack, 0, CALLBACK_FUNCTION) <> 0 Then
        hDevice = INVALID_HANDLE_VALUE
    Else
        ReDim btWaveOutBuffer1(cSamplesPerCluster * (lBitDepth \ 8) - 1)
        ReDim btWaveOutBuffer2(cSamplesPerCluster * (lBitDepth \ 8) - 1)
        With udtBuffer1Hdr
            .lpData = VarPtr(btWaveOutBuffer1(0))
            .dwBufferLength = UBound(btWaveOutBuffer1) + 1
            .dwUser = 1
        End With
        With udtBuffer2Hdr
            .lpData = VarPtr(btWaveOutBuffer2(0))
            .dwBufferLength = UBound(btWaveOutBuffer2) + 1
            .dwUser = 2
        End With
        If waveOutPrepareHeader(hDevice, udtBuffer1Hdr, LenB(udtBuffer1Hdr)) Or _
                waveOutPrepareHeader(hDevice, udtBuffer2Hdr, LenB(udtBuffer2Hdr)) > 0 Then
            waveOutUnprepareHeader hDevice, udtBuffer1Hdr, LenB(udtBuffer1Hdr)
            waveOutUnprepareHeader hDevice, udtBuffer2Hdr, LenB(udtBuffer2Hdr)
            waveOutClose hDevice
            hDevice = INVALID_HANDLE_VALUE
        End If
    End If
End Sub

Private Function CloseOutputDevice()
    waveOutReset hDevice
    waveOutUnprepareHeader hDevice, udtBuffer1Hdr, LenB(udtBuffer1Hdr)
    waveOutUnprepareHeader hDevice, udtBuffer2Hdr, LenB(udtBuffer2Hdr)
    waveOutClose hDevice
    hDevice = INVALID_HANDLE_VALUE
End Function

Private Sub CallBack(ByVal hDev As Long, ByVal uMsg As Long, dwInstance As Long, _
        dwParam1 As WAVEHDR, dwParam2 As Long)
    If uMsg = WOM_DONE Then
        lBufferFinished = lBufferFinished And (Not dwParam1.dwUser)
    End If
End Sub


