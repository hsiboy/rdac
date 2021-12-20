// This is the main project file for VC++ application project 
// generated using an Application Wizard.

#include "stdafx.h"
#using <mscorlib.dll>
using namespace System;
#include <stdio.h>
#include <sys/stat.h>

/*
    VS1680 Backup Disc TAKE stripper
	Don Speirs 11/15/06

	Must work on an .iso image of a 1680 roland backup disc 

	Rev 0.0.0
*/

long fsize(const char *const name);
unsigned int FindTAKE0(unsigned int SRIndex, char* SR);
void MakeTakeFileName(char* outFileName,unsigned int InIndex,char* ShiftReg);
unsigned int FindVS1680ET25D(unsigned int SRIndex,char* SR);

//*****************************************************************************
int _tmain(int argc, char **argv)
{

	long WhereAmI;
	unsigned int State = 0;
	unsigned int i;
	char ShiftReg[32];
	unsigned int InIndex = 0;	
    	char* inFileName  = NULL;
	char outFileName[30];
	long FileSize;

	FILE* fin; 
	FILE* fout; 

// Process input file name

    if(inFileName == NULL) 
		inFileName  = argv[1];

	if(inFileName  == NULL &#0124;&#0124; strlen(inFileName)  == 0)
	{
		printf("Usage: rdr <input_file> \n");
		exit(0);
	}

// Open the input file or error out

	if((fin = fopen(inFileName,  "rb")) == NULL)
	{
		printf("INPUT FILE NOT FOUND\n");
		exit(0);
	}

// Binary file so need to know length to check for EOF

	if((FileSize  = fsize(inFileName)) == -1)
	{
		printf("ERROR in deteriming file size\n");
		exit(0);
	}

// When looking for TAKExxxx need to be 5 ahead of store in the shift register

	fread(ShiftReg, 1, 5, fin);
	InIndex = 5;

// Run all the way to file end

	while((WhereAmI = ftell(fin)) < FileSize)
	{

// Stuff incoming data bytes into circular buffer

		ShiftReg[InIndex++] = getc(fin);
		if(InIndex == 32)
			InIndex = 0;

// Run State Machine

		switch(State)
		{
		case 0:

// State0 is looking for a START TAKE delimiter

			State = FindTAKE0(InIndex,ShiftReg);
			break;

		case 1:

// State1 is building the filename for this TAKE (<Songname (12bytes)><TAKExxxx>)

			for(i=0;i<3;i++)
			{
				ShiftReg[InIndex++] = getc(fin);
				if(InIndex == 32)
					InIndex = 0;
			}

			MakeTakeFileName(outFileName,InIndex,ShiftReg);
			if((fout = fopen(outFileName,	"wb")) == NULL)
			{
				printf("ERROR creating output file\n");
				exit(0);
			}

// Don't know why data is so far after the delimiter but it seems to be consistant over several backup discs....

			fseek(fin, 0x5BBD, 1);

// Stay 16 ahead of file write for finding the END OF TAKE delimiter

			fread(ShiftReg, 1, 16, fin);
			InIndex = 16;			

			State = 2;
			break;

		case 2:

// State2 is write the data until the END OF TAKE delimiter found
// When delimiter found close file and start looking for another TAKE until all input bytes processed
// This is 1680 specific??? what do other VS machines look like???

			if(FindVS1680ET25D(InIndex,ShiftReg))
			{
				fclose(fout);
				State = 0;

// When looking for the next TAKExxxx need to be 5 ahead of store in the shift register

				fread(ShiftReg, 1, 5, fin);
				InIndex = 5;
			}
			else			
				putc(ShiftReg[(32 + InIndex - 11)%32],fout);			
			
			break;
		}
	}

    fclose(fin);
    fclose(fout);
}
//*****************************************************************************
unsigned int FindTAKE0(unsigned int SRIndex, char* SR)
{
// Find START OF TAKE delimiter
// Get enough data to build the file name from the buffer while doing it

	unsigned int Index;
	char* Datapntr;

	Index = (32 + SRIndex - 5)%32;

	Datapntr = SR + Index;
	if(*Datapntr != 'T')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != 'A')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != 'K')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != 'E')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

// The word TAKE exists other places but always followed by a space

	Datapntr = SR + Index;
	if(*Datapntr == ' ')
		return(0);

// We have a full TAKEx string in the circular buffer...time to start saving the data

	return(1);
}

unsigned int FindVS1680ET25D(unsigned int SRIndex,char* SR)
{
// END OF TAKE delimiter in a 1680 backup

	unsigned int Index;
	char* Datapntr;

	Index = (32 + SRIndex - 11)%32;

	Datapntr = SR + Index;
	if(*Datapntr != 'V')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != 'S')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != '1')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != '6')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != '8')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != '0')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != 'E')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != 'T')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != '2')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != '5')
		return(0);

	Index++;
	if(Index == 32)
		Index = 0;

	Datapntr = SR + Index;
	if(*Datapntr != 'D')
		return(0);

	return(1);

}
void MakeTakeFileName(char* outFileName,unsigned int InIndex,char* SR)
{
// Use song name and take# to build a file name (<Songname (12bytes)><TAKExxxx>)

	unsigned int Index,i;
	char* DataPntr;

	Index = (32+InIndex-31)%32;
	DataPntr = SR + Index;

	for(i=0;i<12;i++)
	{
		*outFileName++ = *DataPntr;
		Index++;
		DataPntr++;

		if(Index == 32)
		{
			Index = 0;
			DataPntr -= 32;
		}
	}

	for(i=0;i<10;i++)
	{
		Index++;
		DataPntr++;
		if(Index == 32)
		{
			Index = 0;
			DataPntr -= 32;
		}
	}	

	for(i=0;i<8;i++)
	{
		*outFileName++ = *DataPntr;

		Index++;
		DataPntr++;

		if(Index == 32)
		{
			Index = 0;
			DataPntr -= 32;
		}
	}

	*outFileName++ = '.';
	*outFileName++ = 'V';
	*outFileName++ = 'R';
	*outFileName++ = '6';
}

long fsize(const char *const name)
{
// Find the length of the file

    struct stat stbuf;
    if(stat(name, &stbuf) == -1)
        return -1; // The file could not be accessed.
 
    return stbuf.st_size;
}
