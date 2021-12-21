# VS Wave Export Software

This Software is designed to convert RDAC (Roland Digital Audio Coding) encoded audio tracks as created by Roland VS multitrack machines to simple wave files.

Features:
* supports MT1/MT2/MTP/M16/M24 formats
* supports 2480, 2400, 1880, 1680, 880EX, 880VX, 880 and 890
* supports direct harddisk access
* supports VirDis: simply drag .bin or .hdd files into the list
* supports files ripped from backup CDR/DVD: drag SONG files into the list
* Partition Analysis function for data recovery
* create self extracting RDAC files

## Installation:
Simple answer: there is no installation. Just unzip the downloaded file, you will get a folder conatining several files, double click the 'VS Wave Export.exe' file to start the program. 
Upon first startup/closedown the program will write your last used settings like the export path into the registry under `HKCU\Software\VB and VBA Program Settings\VSWaveExport`.
At any time you can delete the folder for 'deinstallation' of this or older versions of the software (the registry keys will, of course, remain).

## Using a Harddisk:
To operate the tool on a hard disks it is required that a Roland VS formatted media with the projects in playable format is connected to the PC. 
This could be the main VS hard disk, an external SCSI device or a Song Vault disk. 
It will not work with projects copied in backup mode. It might work with 100MB ZIP disks (FAT 12 formatted), due to lack of an SCSI ZIP drive I could not test it. 
Note that you can not directly connect the VS to a PC. Any tries to do so may damage your equipment! 
To connect the internal drive of the VS to a computer, you must take the drive out of the VS and build it into your computer.
If the tool does not list the VS disk, please check that your BIOS has identified the disk correctly (maybe you have to adjust master/slave jumpers) and that Windows shows the first 4 partitions (note that the tool can access all 12 partitions). 
If everything looks correct and the tool keeps ignoring the disk, you can force it to look at a specific partition by dragging the disk icon (the one you find in the 'My Computer' view of the Windows Explorer) onto the list of Found Roland Drives.

## Using VirDis:
To operate on VirDis Files, drag the *.bin or *.hdd file into the list of Found Roland Drives. After storing a file via VS onto the VirDis, hit F5 in the Wave Export App to refresh the content.
You can get more information about VirDis here: http://www.virtualscsi.com/virdis.htm
Using ripped files:
To extract the files from your backup CDR/DVD, bears ripper utility V 022 is included in the distribution
You can start the ripper tool with the 'Rip from CD-R' menu item. Once you set up the commandline for your purpose, VSWE will remember it and you can rip CD-R with a few clicks.

The output will be 24bit waves in case of MTP/M24, if youve got problems opening these, please make shure that your audio software can handle 24bit waves.

## Troubleshooting:
When I try to run the program I get a message 'The file mscomctl.ocx is either missing or incorrectly installed'
mscomctl is Microsofts Common Controls collection used by many programs. 
If you have'nt got it already installed you can install it on your computer, a negative effect to your computer DAW is very unlikely:

1. obtain a copy of MSCOMCTL.OCX.
2. Back up your current copy of MSCOMCTL.OCX (If you have it?) and copy the new version to the same file location. This file should be located in the `WINDOWS\SYSTEM` directory on your C: drive (for Windows NT and 2000: `\WINNT\system32`).
3. Using your mouse, click on the "Start" button and then click on "Run".
4. According to your Operating System, type the following command to register this library file:

Windows 95, 98, or Me:
`regsvr32 \windows\system\MSCOMCTL.OCX`

Windows NT or 2000:
`regsvr32 \WINNT\system32\MSCOMCTL.OCX`

Windows XP:
`regsvr32 \windows\system32\MSCOMCTL.OCX`

5. You should see a message saying "DllRegisterServer ... succeeded".

If you are getting an error telling you that the file is missing, then you are not typing the command in correctly. You will need to type it in *exactly* as as shown above for your operating system. * Please note that there is a SPACE after "regsvr32" and before the rest of the command. You should probably copy and paste the command so that you are unable to mistype it
(Thanks to Paul Miller for the detailed description)

An external SCSI harddisk is connected to the PC, the drive shows up in windows correctly, driveletters like E: F: are displayed, but the tool reports "No Roland Device found!"
For some reasons SCSI devices might not be accessable as physical drives. In this case try dragging one of the driveletters into the white box underneath 'Found Roland drives:' The tool should now display the content of the VS formated drive.

A window named ‘Output:’ appears with strange messages
In this case some entries in the EVENTLST are not as they usually should be. The EVENTLST format is not 100% understood, i would be interested to look into these messages.

## History:
V1.24a: tested and fixed 880EX, 890, fixed a bug that caused one cluster of silence written if a take was exactly a multiple of 32768 samples long. Fixed length display and estimate calculation. Released 01.11.2007
V1.23a: added untested support for 880VX and 890 assuming the file format is identical to 880/880EX. Released 16.Aug.2007. Click here for the source code (rename to zip)
V1.22a: bugfixed: Partition Analysis Wave Recovery, SFX Mode, added check for cluster loops causing potential deadlock, Samplerate code in the Songfile now only uses lower 4 Bits. Released 04.July 2007
V1.21a: fixed a bug in CD-R ripped file mode. Released 16.May 2007
V1.2a: released 12.May 2007 added: Partition Analysis, Partition Shuffler, Self Extracting executeable, Bears CD ripper. Several improvements including Eventlist interpretation, RDAC decoding etc. V1.15b: fixed a bug in CD-R ripped file mode
V1.14b: added drag and drop support for CD-R ripped Files, tested filebased 1680, added untested 880EX
V1.13b: added, but not tested(!) FAT12 support; added and tested VS1880 support
V1.12b: added, but not tested(!) 1680 family support
V1.11b: removed a bug in export start/stop time, and added another GUI gimick

If you want to find out more about the good library, click here: www.thegoodlibrary.com
