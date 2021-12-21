#include "Macros.c"

// Lookup table for relevant pattern
int patternsMT2[] = {
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, /* 0000.... */
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, /* 0001.... */
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, /* 0010.... */
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, /* 0011.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, /* 0100.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, /* 0101.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, /* 0110.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, /* 0111.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11, /* 1000.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11, /* 1001.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11, /* 1010.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11, /* 1011.... */
    12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19, /* 1100.... */
    12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19, /* 1101.... */
    20,20,21,21,22,22,23,23,24,24,25,26,27,28,29,30, /* 1110.... */
    20,20,21,21,22,22,23,23,24,24,31,32,33,34,35,36  /* 1111.... */
};

// MT2 decode Macros:

/*=======================================================================================
	            	PATTERN A       ppp77777 77777776 pppfffff fffffffe
						            66655555 44443333 eeeddddd ccccbbbb
						            33322221 11110000 bbbaaaa9 99998888				   */
#define MT2_EXTRACT_A(ShiftMacroName) \
            *p     = MASK_R(in[9],  0x0f, 0)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x01, 4) | MASK_R(in[9], 0xf0, 4);SIGN_EXTEND_5 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[8],  0x1e, 1)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x0f, 3) | MASK_R(in[8], 0xe0, 5);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[5],  0xf0, 4)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[4],  0x1f, 0)                         ;SIGN_EXTEND_5 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x01, 3) | MASK_R(in[4], 0xe0, 5);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x1f, 7) | MASK_R(in[1], 0xfe, 1);SIGN_EXTEND_12(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[11], 0x0f, 0)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x01, 4) | MASK_R(in[11],0xf0, 4);SIGN_EXTEND_5 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[10], 0x1e, 1)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x0f, 3) | MASK_R(in[10],0xe0, 5);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[7],  0xf0, 4)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[6],  0x1f, 0)                         ;SIGN_EXTEND_5 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x01, 3) | MASK_R(in[6], 0xe0, 5);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x1f, 7) | MASK_R(in[3], 0xfe, 1);SIGN_EXTEND_12(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
	            	PATTERN B       pp777777 77777666 ppffffff fffffeee
						            65555554 44433333 eddddddc cccbbbbb
						            33222211 11110000 bbaaaa99 99998888				   */
#define MT2_EXTRACT_B(ShiftMacroName) \
            *p     = MASK_R(in[9],  0x0f, 0)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x03, 4) | MASK_R(in[9], 0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[8],  0x3c, 2)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x1f, 2) | MASK_R(in[8], 0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x01, 3) | MASK_R(in[5], 0xe0, 5);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[4],  0x7e, 1)                         ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x07, 1) | MASK_R(in[4], 0x80, 7);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x3f, 5) | MASK_R(in[1], 0xf8, 3);SIGN_EXTEND_11(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[11], 0x0f, 0)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x03, 4) | MASK_R(in[11],0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[10], 0x3c, 2)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x1f, 2) | MASK_R(in[10],0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x01, 3) | MASK_R(in[7], 0xe0, 5);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[6],  0x7e, 1)                         ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x07, 1) | MASK_R(in[6], 0x80, 7);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x3f, 5) | MASK_R(in[3], 0xf8, 3);SIGN_EXTEND_11(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
	            	PATTERN B2      ppp77777 77777666 pppfffff fffffeee
						            65555554 44433333 eddddddc cccbbbbb
						            33222211 11110000 bbaaaa99 99998888				   */
#define MT2_EXTRACT_B2(ShiftMacroName) \
            *p     = MASK_L(in[9],  0x0f, 0)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x03, 4) | MASK_R(in[9], 0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[8],  0x3c, 2)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x1f, 2) | MASK_R(in[8], 0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x01, 3) | MASK_R(in[5], 0xe0, 5);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[4],  0x7e, 1)                         ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x07, 1) | MASK_R(in[4], 0x80, 7);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x1f, 5) | MASK_R(in[1], 0xf8, 3);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x0f, 0)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x03, 4) | MASK_R(in[11],0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[10], 0x3c, 2)                         ;SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x1f, 2) | MASK_R(in[10],0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x01, 3) | MASK_R(in[7], 0xe0, 5);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[6],  0x7e, 1)                         ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x07, 1) | MASK_R(in[6], 0x80, 7);SIGN_EXTEND_4 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x1f, 5) | MASK_R(in[3], 0xf8, 3);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
	            	PATTERN C       ppp77777 77776666 pppfffff ffffeeee
						            55555544 44333333 ddddddcc ccbbbbbb
						            33222211 11110000 bbaaaa99 99998888				   */
#define MT2_EXTRACT_C(ShiftMacroName) \
            *p     = MASK_L(in[9],  0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x03, 4) | MASK_R(in[9], 0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[8],  0x3c, 2)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x3f, 2) | MASK_R(in[8], 0xc0, 6);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x03, 2) | MASK_R(in[5], 0xc0, 6);SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[4],  0xfc, 2)                         ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[1],  0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x1f, 4) | MASK_R(in[1], 0xf0, 4);SIGN_EXTEND_9(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x03, 4) | MASK_R(in[11],0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[10], 0x3c, 2)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x3f, 2) | MASK_R(in[10],0xc0, 6);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x03, 2) | MASK_R(in[7], 0xc0, 6);SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[6],  0xfc, 2)                         ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[3],  0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x1f, 4) | MASK_R(in[3], 0xf0, 4);SIGN_EXTEND_9(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
	            	PATTERN C2      pppp7777 77776666 ppppffff ffffeeee
						            55555544 44333333 ddddddcc ccbbbbbb
						            33222211 11110000 bbaaaa99 99998888				   */
#define MT2_EXTRACT_C2(ShiftMacroName) \
            *p     = MASK_L(in[9],  0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x03, 4) | MASK_R(in[9], 0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[8],  0x3c, 2)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x3f, 2) | MASK_R(in[8], 0xc0, 6);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x03, 2) | MASK_R(in[5], 0xc0, 6);SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[4],  0xfc, 2)                         ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[1],  0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x0f, 4) | MASK_R(in[1], 0xf0, 4);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x03, 4) | MASK_R(in[11],0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[10], 0x3c, 2)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x3f, 2) | MASK_R(in[10],0xc0, 6);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x03, 2) | MASK_R(in[7], 0xc0, 6);SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[6],  0xfc, 2)                         ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[3],  0x0f, 0)                         ;SIGN_EXTEND_4(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x0f, 4) | MASK_R(in[3], 0xf0, 4);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_4

/*=======================================================================================
	            	PATTERN D       pp777777 76666655 ppffffff feeeeedd
						            55554444 43333333 ddddcccc cbbbbbbb
						            22222111 11100000 aaaaa999 99988888				   */
#define MT2_EXTRACT_D(ShiftMacroName) \
            *p     = MASK_R(in[9],  0x1f, 0)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x07, 3) | MASK_R(in[9], 0xe0, 5);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[8],  0xf8, 3)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[5],  0x7f, 0)                         ;SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x0f, 1) | MASK_R(in[5], 0x80, 7);SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x03, 4) | MASK_R(in[4], 0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[1],  0x7c, 2)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x3f, 1) | MASK_R(in[1], 0x80, 7);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[11], 0x1f, 0)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x07, 3) | MASK_R(in[11],0xe0, 5);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[10], 0xf8, 3)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[7],  0x7f, 0)                         ;SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x0f, 1) | MASK_R(in[7], 0x80, 7);SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x03, 4) | MASK_R(in[6], 0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[3],  0x7c, 2)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x3f, 1) | MASK_R(in[3], 0x80, 7);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_4

/*=======================================================================================
	            	PATTERN F       pppp7777 77666665 ppppffff ffeeeeed
						            55555444 44333333 dddddccc ccbbbbbb
						            22222111 11100000 aaaaa999 99988888				   */
#define MT2_EXTRACT_F(ShiftMacroName) \
            *p     = MASK_L(in[9],  0x1f, 0)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x07, 3) | MASK_R(in[9], 0xe0, 5);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[8],  0xf8, 3)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[5],  0x3f, 0)                         ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x07, 2) | MASK_R(in[5], 0xc0, 6);SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x01, 5) | MASK_R(in[4], 0xf8, 3);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[1],  0x3e, 1)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x0f, 2) | MASK_R(in[1], 0xc0, 6);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x1f, 0)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x07, 3) | MASK_R(in[11],0xe0, 5);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[10], 0xf8, 3)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[7],  0x3f, 0)                         ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x07, 2) | MASK_R(in[7], 0xc0, 6);SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x01, 5) | MASK_R(in[6], 0xf8, 3);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[3],  0x3e, 1)                         ;SIGN_EXTEND_5(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x0f, 2) | MASK_R(in[3], 0xc0, 6);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_8

/*=======================================================================================
	            	PATTERN F2      pppp7777 77666665 ppppffff ffeeeeed
						            55555444 44333333 dddddccc ccbbbbbb
						            22222111 11100000 aaaaa999 99988888				   */
#define MT2_EXTRACT_F2() \
            *p     = MASK_L(in[9],  0x1f, 0)                         ;SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_L(in[8],  0x07, 3) | MASK_R(in[9], 0xe0, 5);SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
            *(++p) = MASK_R(in[8],  0xf8, 3)                         ;SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_R(in[5],  0x3f, 0)                         ;SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
            *(++p) = MASK_L(in[4],  0x07, 2) | MASK_R(in[5], 0xc0, 6);SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_L(in[1],  0x01, 5) | MASK_R(in[4], 0xf8, 3);SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
            *(++p) = MASK_R(in[1],  0x3e, 1)                         ;SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_L(in[0],  0x0f, 2) | MASK_R(in[1], 0xc0, 6);SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
            *(++p) = MASK_L(in[11], 0x1f, 0)                         ;SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_L(in[10], 0x07, 3) | MASK_R(in[11],0xe0, 5);SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
            *(++p) = MASK_R(in[10], 0xf8, 3)                         ;SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_R(in[7],  0x3f, 0)                         ;SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
            *(++p) = MASK_L(in[6],  0x07, 2) | MASK_R(in[7], 0xc0, 6);SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_L(in[3],  0x01, 5) | MASK_R(in[6], 0xf8, 3);SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
            *(++p) = MASK_R(in[3],  0x3e, 1)                         ;SIGN_EXTEND_5(*p);SHIFT_ROUND_11(*p);\
            *(++p) = MASK_L(in[2],  0x0f, 2) | MASK_R(in[3], 0xc0, 6);SIGN_EXTEND_6(*p);SHIFT_ROUND_10(*p);\
			APPLY_MACRO (DECREMENT)

//*****************************************************************************
void decodeMT2(int d0, unsigned char *in, int *out)
{
    // Decodes a 12-byte MT2 RDAC block into 16-bit samples

    int patternIndex = MASK_L(in[0], 0xf0, 0) | MASK_R(in[2], 0xf0, 4);

    int pattern = patternsMT2[patternIndex];

	int *p = out;
    switch (pattern) {

        /*=====================================================================
            PATTERN B
        */

        case 0:  /* 00..00.. */
            MT2_EXTRACT_B(SHIFT_ROUND_0);
            return;

        case 1:  /* 00..01.. */
            MT2_EXTRACT_B(SHIFT_ROUND_1);
            return;

        case 2:  /* 00..10.. */
            MT2_EXTRACT_B(SHIFT_ROUND_2);
            return;

        case 3:  /* 00..11.. */
            MT2_EXTRACT_B(SHIFT_ROUND_3);
           return;

        case 4:  /* 01..00.. */
            MT2_EXTRACT_B(SHIFT_ROUND_4);
           return;

        case 5:  /* 01..01.. */
            MT2_EXTRACT_B(SHIFT_ROUND_5);
			APPLY_MACRO (LIMIT_16);
            return;

        /*=====================================================================
            PATTERN D
        */

        case 6:  /* 01..10.. */
            MT2_EXTRACT_D(SHIFT_ROUND_4);
            return;

        case 7:  /* 01..11.. */
            MT2_EXTRACT_D(SHIFT_ROUND_5);
            return;

        case 8:  /* 10..00.. */
            MT2_EXTRACT_D(SHIFT_ROUND_6);
            return;

        case 9:  /* 10..01.. */
            MT2_EXTRACT_D(SHIFT_ROUND_7);
            return;

        case 10: /* 10..10.. */
            MT2_EXTRACT_D(SHIFT_ROUND_8);
            return;

        case 11: /* 10..11.. */
            MT2_EXTRACT_D(SHIFT_ROUND_9);
			APPLY_MACRO (LIMIT_16);
            return;

		/*=====================================================================
            PATTERN A
        */

        case 12: /* 110.000. */
			// this one drops a bit?!
			APPLY_MACRO (ZAP);
            return;

        case 13: /* 110.001. */
            MT2_EXTRACT_A(SHIFT_ROUND_0);
            return;

        case 14: /* 110.010. */
            MT2_EXTRACT_A(SHIFT_ROUND_1);
           return;

        case 15: /* 110.011. */
            MT2_EXTRACT_A(SHIFT_ROUND_2);
           return;

        case 16: /* 110.100. */
            MT2_EXTRACT_A(SHIFT_ROUND_3);
            return;

        case 17: /* 110.101. */
            MT2_EXTRACT_A(SHIFT_ROUND_4);
			APPLY_MACRO (LIMIT_16);
            return;

        /*=====================================================================
            PATTERN B2
        */

        case 18: /* 110.110. */
            MT2_EXTRACT_B2(SHIFT_ROUND_6);
			APPLY_MACRO (LIMIT_16);
            return;

        /*=====================================================================
            PATTERN C
        */
        case 19: /* 110.111. */
            MT2_EXTRACT_C(SHIFT_ROUND_2);
            return;

        case 20: /* 111.000. */
            MT2_EXTRACT_C(SHIFT_ROUND_3);
            return;

        case 21: /* 111.001. */
            MT2_EXTRACT_C(SHIFT_ROUND_4);
            return;

        case 22: /* 111.010. */
            MT2_EXTRACT_C(SHIFT_ROUND_5);
            return;

        case 23: /* 111.011. */
            MT2_EXTRACT_C(SHIFT_ROUND_6);
			APPLY_MACRO (LIMIT_16);
            return;

        case 24: /* 111.100. */
            MT2_EXTRACT_C(SHIFT_ROUND_7);
			APPLY_MACRO (LIMIT_16);
            return;

		/*=====================================================================
            PATTERN F
        */

        case 25: /* 11101010 */
            MT2_EXTRACT_F(SHIFT_ROUND_6);
            return;

        case 26: /* 11101011 */
            MT2_EXTRACT_F(SHIFT_ROUND_7);
            return;

        case 27: /* 11101100 */
            MT2_EXTRACT_F(SHIFT_ROUND_8);
            return;

        case 28: /* 11101101 */
            MT2_EXTRACT_F(SHIFT_ROUND_9);
            return;

        case 29: /* 11101110 */
            MT2_EXTRACT_F(SHIFT_ROUND_10);
			APPLY_MACRO (LIMIT_16);
            return;

		/*=====================================================================
            PATTERN F2
        */

        case 30: /* 11101111 */
            MT2_EXTRACT_F2();
			APPLY_MACRO (LIMIT_16);
            return;

        /*=====================================================================
            PATTERN C2
        */

        case 31: /* 11111010 */
            MT2_EXTRACT_C2(SHIFT_ROUND_8);
			APPLY_MACRO (LIMIT_16);
            return;

        /*=====================================================================
            PATTERN E unused
			pppp8888 88888777
			77666665 55554444
			43333322 22211111
        */

        case 32: /* 11111011 */
			// unused
			APPLY_MACRO (ZAP);
            return;

        case 33: /* 11111100 */
			// unused
			APPLY_MACRO (ZAP);
            return;

        case 34: /* 11111101 */
			// unused
			APPLY_MACRO (ZAP);
            return;

		case 35: /* 11111110 */
			// unused
			APPLY_MACRO (ZAP);
            return;
 
		case 36: /* 11111111 */
			// unused
			APPLY_MACRO (ZAP);
            return;

        default: break;
    }
}
