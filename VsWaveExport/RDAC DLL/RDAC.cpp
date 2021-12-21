/*
    RDAC.dll 1.00
    designed to work with VS Wave Export V1.2
	by Daniel Aue
    released on 12.May 2007 not under any licence at all

	as the attentive observer can see, source code for RDAC encoding is available, but not yet
	released as the juristic situation is uncertain.

	Version 1.00 - based on work by Randy Gordon
				   everything started on VS-Planet:
				   http://www.vsplanet.com/ubb/ultimatebb.php?ubb=get_topic;f=1;t=022118#000001
*/




#include "stdafx.h"
#include "Winbase.h"

#include "decodeMT2.c"
#include "decodeMTP.c"
//#include "encodeMTP.c"
//#include "encodeMT2.c"


BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}


int _stdcall Decode(int d0, unsigned char *inRDAC, int *outWave, int cSamples, int Format)
{
	int x;

	if (IsBadWritePtr(outWave,cSamples*4)) {

		return -2;

	} else {

		switch (Format) {
		case 0:	//MT1
			if (IsBadReadPtr(inRDAC,cSamples)) {
				return -1;
			} else { 
				// MT1 is like MTP but only uses the top 16 bits of the 24bit output
				d0=d0<<8;
				for (x=0;x < cSamples/16;x++) {
					decodeMTP(d0, inRDAC, outWave);
					d0=outWave[15];
					inRDAC=inRDAC+16;
					outWave=outWave+16;
				}
				inRDAC=inRDAC-cSamples;
				outWave=outWave-cSamples;
				for (x=0;x < cSamples;x++) {
					outWave[x]=outWave[x]>>8;
					outWave[x]=-(outWave[x] & 0x00008000) | outWave[x];
				}
				return 0;
			}
			break;
		case 1:	//MT2
			if (IsBadReadPtr(inRDAC,cSamples/16*12)) {
				return -1;
			} else { 
				for (x=0;x < cSamples/16;x++) {
					decodeMT2(d0, inRDAC, outWave);
					d0=outWave[15];
					inRDAC=inRDAC+12;
					outWave=outWave+16;
				}
				return 0;
			}
		case 3: //M16
			if (IsBadReadPtr(inRDAC,cSamples*2)) {
				return -1;
			} else { 
				for (x=0;x < cSamples;x++) {
					outWave[0]=inRDAC[0]<<8 | inRDAC[1];
					outWave[0]=-(outWave[0] & 0x00008000) | outWave[0];
					inRDAC=inRDAC+2;
					outWave=outWave+1;
				}
				return 0;
			}
		case 5:	//MTP
			if (IsBadReadPtr(inRDAC,cSamples)) {
				return -1;
			} else { 
				for (x=0;x < cSamples/16;x++) {
					decodeMTP(d0, inRDAC, outWave);
					d0=outWave[15];
					inRDAC=inRDAC+16;
					outWave=outWave+16;
				}
				return 0;
			}
		case 8:	//M24
			if (IsBadReadPtr(inRDAC,cSamples*3)) {
				return -1;
			} else { 
				for (x=0;x < cSamples;x++) {
					outWave[0]=inRDAC[0]<<16 | inRDAC[1]<<8 | inRDAC[2];
					outWave[0]=-(outWave[0] & 0x00800000) | outWave[0];
					inRDAC=inRDAC+3;
					outWave=outWave+1;
				}
				return 0;
			}
		default:
			return -3;
		}
	}
}


/*
int _stdcall Encode(int d0, int *inWave, unsigned char *outRDAC, int cSamples, int Format)
{
	int x, d1;

	if (IsBadReadPtr(inWave,cSamples*4)) {
		return -2;

	} else {

		switch (Format) {
		case 0:	//MT1
			if (IsBadWritePtr(outRDAC,cSamples)) {
				return -1;
			} else { 
				for (x=0;x < cSamples;x++) {
					inWave[x]=inWave[x]<<8;
				}
				d0 = d0<<8;
				for (x=0;x < cSamples/16;x++) {
					d1 = inWave[15];
					encodeMTP(d0, inWave, outRDAC);
					d0 = d1;
					outRDAC=outRDAC+16;
					inWave=inWave+16;
				}
				return 0;
			}

		case 1:	//MT2
			if (IsBadWritePtr(outRDAC,cSamples/16*12)) {
				return -1;
			} else { 
				for (x=0;x < cSamples/16;x++) {
					d1 = inWave[15];
					encodeMT2(d0, inWave, outRDAC);
					d0 = d1;
					outRDAC=outRDAC+16;
					inWave=inWave+16;
				}
				return 0;
			}
		case 5:	//MTP
			if (IsBadWritePtr(outRDAC,cSamples)) {
				return -1;
			} else { 
				for (x=0;x < cSamples/16;x++) {
					d1 = inWave[15];
					encodeMTP(d0, inWave, outRDAC);
					d0 = d1;
					outRDAC=outRDAC+16;
					inWave=inWave+16;
				}
				return 0;
			}
		default:
			return -3;
		}
	}
}
*/

// ---------------------------------------------------------------------------------
// other Functions used by VSWaveExport, implemented in C for speed reasons
// ---------------------------------------------------------------------------------


int _stdcall ShrinkTo24(int *In32, unsigned char *Out24,int cSamples)
{
	int x;
	for (x=0;x < cSamples;x++) {
        Out24[x * 3]     = unsigned char (In32[x]  & 0x0000FF);
        Out24[x * 3 + 1] = unsigned char ((In32[x] & 0x00FF00) >> 8);
        Out24[x * 3 + 2] = unsigned char ((In32[x] & 0xFF0000) >> 16);
	}
	return 0;
}

int _stdcall ShrinkTo16(int *In32, unsigned char *Out16,int cSamples)

{
	int x;
	for (x=0;x < cSamples;x++) {
        Out16[x * 2]     = unsigned char (In32[x] & 0x0000FF);
        Out16[x * 2 + 1] = unsigned char ((In32[x] & 0x00FF00) >> 8);
	}
	return 0;
}



int _stdcall CastFat(unsigned char *In8, unsigned int *Out32,int cEntries,int Type)
{
	int x;

	switch (Type) {
	case 12:
		for (x=0;x < cEntries;x++) {
			if ((x & 1) == 0) {
				Out32[x] = In8[x / 2 * 3] | ((In8[x / 2 * 3 + 1] & 0x0F) << 8);
			} else {
				Out32[x] = ((In8[(x-1) / 2 * 3 + 2]) << 4) | ((In8[(x-1) / 2 * 3 + 1] & 0xF0) >> 4);
			}
            if (Out32[x] >= 0x00000FF7) Out32[x] |= 0x0FFFF000;    //invalid (fehlerhaft, ende, ...)
		}
		return 0;
		break;

	case 16:
		for (x=0;x < cEntries;x++) {
			Out32[x] = In8[x * 2] | (In8[x * 2 + 1] << 8);
            if (Out32[x] >= 0x0000FFF7) Out32[x] |= 0x0FFFF000;    //invalid (fehlerhaft, ende, ...)
		}
		return 0;
		break;

	case 32:
		for (x=0;x < cEntries;x++) {
			Out32[x] = In8[x * 4] | (In8[x * 4 + 1] << 8) | (In8[x * 4 + 2] << 16) | (In8[x * 4 + 3] << 24);
            if (Out32[x] >= 0x00FFFFF7) Out32[x] |= 0x0FFFF000;    //invalid (fehlerhaft, ende, ...)
		}

	default:
		return -3;
		break;

	}
}


void _stdcall DrawAllClusters(unsigned int *Fat1,unsigned int *Fat2,unsigned int *FatToFile,
							 unsigned int cFatEntries, unsigned int *BitmapData)
{
	unsigned int x, iCol, cRows;

	cRows=cFatEntries/400;

	for (x=0;x < cFatEntries; x++) {
		if (Fat1[x] != Fat2[x]) {
			iCol=0xFF0000;		//FAT discrepancy
		} else if (Fat1[x] == 0) {
			iCol=0x000000;		//not in use
		} else if (Fat1[x] >= 0xFFFFF7) {
			iCol=0xFFFF00;		//eof or bad cluster
		} else if (FatToFile[x] != 0) {
			iCol=0x00FF00;		//used cluster
		} else {
			iCol=0x006464;		//unempty, but unreferenced cluster
		}

		BitmapData[(cRows - (int) x/400) * 1600 +       (x % 400) * 2    ]=iCol;
		BitmapData[(cRows - (int) x/400) * 1600 +       (x % 400) * 2 + 1]=iCol;
		BitmapData[(cRows - (int) x/400) * 1600 + 800 + (x % 400) * 2    ]=iCol;
		BitmapData[(cRows - (int) x/400) * 1600 + 800 + (x % 400) * 2 + 1]=iCol;
	}
}


void _stdcall HighlightClusters(unsigned int *Fat,unsigned int *FatToFile,
							 unsigned int cFatEntries, unsigned int *BitmapData, unsigned int *CurHighlight, unsigned int *NewHighlight)
{
	unsigned int x, iCol, cRows, c, Cluster;


	cRows=cFatEntries/400;
	c=CurHighlight[0];

	for (x=1;x <= c; x++) {
		Cluster=CurHighlight[x];
		if (Fat[Cluster] == 0) {
			iCol=0x000000;		//not in use
		} else if (Fat[Cluster] >= 0xFFFFF7) {
			iCol=0xFFFF00;		//eof or bad cluster
		} else if (FatToFile[Cluster] != 0) {
			iCol=0x00FF00;		//used cluster
		} else {
			iCol=0x006464;		//unempty, but unreferenced cluster
		}

		BitmapData[(cRows - (int) Cluster/400) * 1600 +       (Cluster % 400) * 2    ]=iCol;
		BitmapData[(cRows - (int) Cluster/400) * 1600 +       (Cluster % 400) * 2 + 1]=iCol;
		BitmapData[(cRows - (int) Cluster/400) * 1600 + 800 + (Cluster % 400) * 2    ]=iCol;
		BitmapData[(cRows - (int) Cluster/400) * 1600 + 800 + (Cluster % 400) * 2 + 1]=iCol;
	}

	c=NewHighlight[0];
	iCol=0xFF00FF;

	for (x=1;x <= c; x++) {
		Cluster=NewHighlight[x];
		BitmapData[(cRows - (int) Cluster/400) * 1600 +       (Cluster % 400) * 2    ]=iCol;
		BitmapData[(cRows - (int) Cluster/400) * 1600 +       (Cluster % 400) * 2 + 1]=iCol;
		BitmapData[(cRows - (int) Cluster/400) * 1600 + 800 + (Cluster % 400) * 2    ]=iCol;
		BitmapData[(cRows - (int) Cluster/400) * 1600 + 800 + (Cluster % 400) * 2 + 1]=iCol;
	}
}


int _stdcall SwapBytes(unsigned char *Data,int cBytes) 
{ 
	int x; 
	unsigned char bt; 
	for (x=0;x < cBytes;x+=2) { 
	bt=Data[x]; 
	Data[x]     = Data[x + 1]; 
	Data[x + 1] = bt; 
	} 
	return 0; 
}