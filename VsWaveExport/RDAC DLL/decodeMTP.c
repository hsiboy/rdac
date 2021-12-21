#include "Macros.c"

// Lookup table for patterns
int patternsMTP[] = {
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,  /* 0000.... */
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,  /* 0001.... */
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,  /* 0010.... */
     0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,  /* 0011.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,  /* 0100.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,  /* 0101.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,  /* 0110.... */
     4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,  /* 0111.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11,  /* 1000.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11,  /* 1001.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11,  /* 1010.... */
     8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,11,11,11,11,  /* 1011.... */
    12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,  /* 1100.... */
    12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,  /* 1101.... */
    20,20,21,21,22,22,23,23,24,24,25,26,27,28,29,30,  /* 1110.... */
    20,20,21,21,22,22,23,23,24,24,31,32,33,34,35,36   /* 1111.... */
};

// MTP decode Macros:

/*=======================================================================================
	            	PATTERN A       ppp77777 77777777 pppfffff ffffffff
						            76666665 55555544 feeeeeed ddddddcc
						            44443333 33333222 ccccbbbb bbbbbaaa
						            22211111 11000000 aaa99999 99888888				   */
#define MTP_EXTRACT_A(ShiftMacroName) \
            *p     = MASK_L(in[13], 0x3f, 0)                          ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x1f, 2) | MASK_R(in[13], 0xc0, 6);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x07, 3) | MASK_R(in[12], 0xe0, 5);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x0f, 5) | MASK_R(in[9],  0xf8, 3);SIGN_EXTEND_9(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x03, 4) | MASK_R(in[8],  0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x01, 6) | MASK_R(in[5],  0xfc, 2);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[4],  0x7e, 1)                          ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x1f, 9) | MASK_L(in[1],  0xff, 1) | MASK_R(in[4], 0x80, 7);SIGN_EXTEND_14(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x3f, 0)                          ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x1f, 2) | MASK_R(in[15], 0xc0, 6);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x07, 3) | MASK_R(in[14], 0xe0, 5);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x0f, 5) | MASK_R(in[11], 0xf8, 3);SIGN_EXTEND_9(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x03, 4) | MASK_R(in[10], 0xf0, 4);SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x01, 6) | MASK_R(in[7],  0xfc, 2);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_R(in[6],  0x7e, 1)                          ;SIGN_EXTEND_6(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x1f, 9) | MASK_L(in[3],  0xff, 1) | MASK_R(in[6], 0x80, 7);SIGN_EXTEND_14(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
					PATTERN B	    pp777777 77777776 ppffffff fffffffe
									66666555 55555444 eeeeeddd dddddccc
									44433333 33332222 cccbbbbb bbbbaaaa
									22111111 11000000 aa999999 99888888				   */
#define MTP_EXTRACT_B(ShiftMacroName) \
			*p     = MASK_L(in[13], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x3f, 2) | MASK_R(in[13], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x0f, 2) | MASK_R(in[12], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x1f, 4) | MASK_R(in[9],  0xf0, 4);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x07, 3) | MASK_R(in[8],  0xe0, 5);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x07, 5) | MASK_R(in[5],  0xf8, 3);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x01, 5) | MASK_R(in[4],  0xf8, 3);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x3f, 7) | MASK_R(in[1],  0xfe, 1);SIGN_EXTEND_13(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x3f, 2) | MASK_R(in[15], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x0f, 2) | MASK_R(in[14], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x1f, 4) | MASK_R(in[11], 0xf0, 4);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x07, 3) | MASK_R(in[10], 0xe0, 5);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x07, 5) | MASK_R(in[7],  0xf8, 3);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x01, 5) | MASK_R(in[6],  0xf8, 3);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x3f, 7) | MASK_R(in[3],  0xfe, 1);SIGN_EXTEND_13(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
		            PATTERN B2  	ppp77777 77777776 pppfffff fffffffe
						            66666555 55555444 eeeeeddd dddddccc
						            44433333 33332222 cccbbbbb bbbbaaaa
						            22111111 11000000 aa999999 99888888				   */
#define MTP_EXTRACT_B2(ShiftMacroName) \
            *p     = MASK_L(in[13], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x3f, 2) | MASK_R(in[13], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x0f, 2) | MASK_R(in[12], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x1f, 4) | MASK_R(in[9],  0xf0, 4);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x07, 3) | MASK_R(in[8],  0xe0, 5);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x07, 5) | MASK_R(in[5],  0xf8, 3);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x01, 5) | MASK_R(in[4],  0xf8, 3);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x1f, 7) | MASK_R(in[1],  0xfe, 1);SIGN_EXTEND_12(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x3f, 2) | MASK_R(in[15], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x0f, 2) | MASK_R(in[14], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x1f, 4) | MASK_R(in[11], 0xf0, 4);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x07, 3) | MASK_R(in[10], 0xe0, 5);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x07, 5) | MASK_R(in[7],  0xf8, 3);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x01, 5) | MASK_R(in[6],  0xf8, 3);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x1f, 7) | MASK_R(in[3],  0xfe, 1);SIGN_EXTEND_12(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
		            PATTERN C       ppp77777 77777766 pppfffff ffffffee
						            66665555 55554444 eeeedddd ddddcccc
						            44333333 33332222 ccbbbbbb bbbbaaaa
						            22111111 11000000 aa999999 99888888           	   */#
#define MTP_EXTRACT_C(ShiftMacroName) \
            *p     = MASK_L(in[13], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x3f, 2) | MASK_R(in[13], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x0f, 2) | MASK_R(in[12], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x3f, 4) | MASK_R(in[9],  0xf0, 4);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x0f, 2) | MASK_R(in[8],  0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x0f, 4) | MASK_R(in[5],  0xf0, 4);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x03, 4) | MASK_R(in[4],  0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x1f, 6) | MASK_R(in[1],  0xfc, 2);SIGN_EXTEND_11(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x3f, 2) | MASK_R(in[15], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x0f, 2) | MASK_R(in[14], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x3f, 4) | MASK_R(in[11], 0xf0, 4);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x0f, 2) | MASK_R(in[10], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x0f, 4) | MASK_R(in[7],  0xf0, 4);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x03, 4) | MASK_R(in[6],  0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x1f, 6) | MASK_R(in[3],  0xfc, 2);SIGN_EXTEND_11(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
		            PATTERN C2      pppp7777 77777766 ppppffff ffffffee
						            66665555 55554444 eeeedddd ddddcccc
						            44333333 33332222 ccbbbbbb bbbbaaaa
						            22111111 11000000 aa999999 99888888           	   */
#define MTP_EXTRACT_C2(ShiftMacroName) \
            *p     = MASK_L(in[13], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x3f, 2) | MASK_R(in[13], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x0f, 2) | MASK_R(in[12], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x3f, 4) | MASK_R(in[9],  0xf0, 4);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x0f, 2) | MASK_R(in[8],  0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x0f, 4) | MASK_R(in[5],  0xf0, 4);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x03, 4) | MASK_R(in[4],  0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x0f, 6) | MASK_R(in[1],  0xfc, 2);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x3f, 0)                          ;SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x3f, 2) | MASK_R(in[15], 0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x0f, 2) | MASK_R(in[14], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x3f, 4) | MASK_R(in[11], 0xf0, 4);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x0f, 2) | MASK_R(in[10], 0xc0, 6);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x0f, 4) | MASK_R(in[7],  0xf0, 4);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x03, 4) | MASK_R(in[6],  0xf0, 4);SIGN_EXTEND_6 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x0f, 6) | MASK_R(in[3],  0xfc, 2);SIGN_EXTEND_10(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_4

/*=======================================================================================
					PATTERN D   	pp777777 77766666 ppffffff fffeeeee
						            66555555 55444444 eedddddd ddcccccc
						            43333333 33222222 cbbbbbbb bbaaaaaa
						            21111111 10000000 a9999999 98888888           	   */
#define MTP_EXTRACT_D(ShiftMacroName) \
            *p     = MASK_L(in[13], 0x7f, 0)                          ;SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x7f, 1) | MASK_R(in[13], 0x80, 7);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x3f, 1) | MASK_R(in[12], 0x80, 7);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x7f, 2) | MASK_R(in[9],  0xc0, 6);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x3f, 1) | MASK_R(in[8],  0x80, 7);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x3f, 2) | MASK_R(in[5],  0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x1f, 2) | MASK_R(in[4],  0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x3f, 3) | MASK_R(in[1],  0xe0, 5);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x7f, 0)                          ;SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x7f, 1) | MASK_R(in[15], 0x80, 7);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x3f, 1) | MASK_R(in[14], 0x80, 7);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x7f, 2) | MASK_R(in[11], 0xc0, 6);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x3f, 1) | MASK_R(in[10], 0x80, 7);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x3f, 2) | MASK_R(in[7],  0xc0, 6);SIGN_EXTEND_8 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x1f, 2) | MASK_R(in[6],  0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x3f, 3) | MASK_R(in[3],  0xe0, 5);SIGN_EXTEND_9 (*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_4

/*=======================================================================================
		            PATTERN E       pppp7777 77777776 ppppffff fffffffe
						            66666655 55555444 eeeeeedd dddddccc
						            44443333 33322222 ccccbbbb bbbaaaaa
						            22111111 10000000 aa999999 98888888           	   */
#define MTP_EXTRACT_E(ShiftMacroName) \
            *p     = MASK_L(in[13], 0x7f, 0)                          ;SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x3f, 1) | MASK_R(in[13], 0x80, 7);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x1f, 2) | MASK_R(in[12], 0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x0f, 3) | MASK_R(in[9],  0xe0, 5);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x07, 4) | MASK_R(in[8],  0xf0, 4);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x03, 5) | MASK_R(in[5],  0xf8, 3);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x01, 6) | MASK_R(in[4],  0xfc, 2);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x0f, 7) | MASK_R(in[1],  0xfe, 1);SIGN_EXTEND_11(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x7f, 0)                          ;SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x3f, 1) | MASK_R(in[15], 0x80, 7);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x1f, 2) | MASK_R(in[14], 0xc0, 6);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x0f, 3) | MASK_R(in[11], 0xe0, 5);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x07, 4) | MASK_R(in[10], 0xf0, 4);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x03, 5) | MASK_R(in[7],  0xf8, 3);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x01, 6) | MASK_R(in[6],  0xfc, 2);SIGN_EXTEND_7 (*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x0f, 7) | MASK_R(in[3],  0xfe, 1);SIGN_EXTEND_11(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_2

/*=======================================================================================
		            PATTERN F       pppp7777 77776666 ppppffff ffffeeee
						            66655555 55544444 eeeddddd dddccccc
						            44333333 33222222 ccbbbbbb bbaaaaaa
						            21111111 10000000 a9999999 98888888           	   */
#define MTP_EXTRACT_F(ShiftMacroName) \
            *p     = MASK_L(in[13], 0x7f, 0)                          ;SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[12], 0x7f, 1) | MASK_R(in[13], 0x80, 7);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[9],  0x3f, 1) | MASK_R(in[12], 0x80, 7);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[8],  0x3f, 2) | MASK_R(in[9],  0xc0, 6);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[5],  0x1f, 2) | MASK_R(in[8],  0xc0, 6);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[4],  0x1f, 3) | MASK_R(in[5],  0xe0, 5);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[1],  0x0f, 3) | MASK_R(in[4],  0xe0, 5);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[0],  0x0f, 4) | MASK_R(in[1],  0xf0, 4);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[15], 0x7f, 0)                          ;SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[14], 0x7f, 1) | MASK_R(in[15], 0x80, 7);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[11], 0x3f, 1) | MASK_R(in[14], 0x80, 7);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[10], 0x3f, 2) | MASK_R(in[11], 0xc0, 6);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[7],  0x1f, 2) | MASK_R(in[10], 0xc0, 6);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[6],  0x1f, 3) | MASK_R(in[7],  0xe0, 5);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[3],  0x0f, 3) | MASK_R(in[6],  0xe0, 5);SIGN_EXTEND_7(*p);ShiftMacroName(*p);\
            *(++p) = MASK_L(in[2],  0x0f, 4) | MASK_R(in[3],  0xf0, 4);SIGN_EXTEND_8(*p);ShiftMacroName(*p);\
			INTERPOLATE_ADD_8

/*=======================================================================================
		            PATTERN F2      pppp7777 77776666 ppppffff ffffeeee
						            66655555 55544444 eeeddddd dddccccc
						            44333333 33222222 ccbbbbbb bbaaaaaa
						            21111111 10000000 a9999999 98888888           	   */

#define MTP_EXTRACT_F2() \
            *p     = MASK_L(in[13], 0x7f, 0)                          ;SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[12], 0x7f, 1) | MASK_R(in[13], 0x80, 7);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
            *(++p) = MASK_L(in[9],  0x3f, 1) | MASK_R(in[12], 0x80, 7);SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[8],  0x3f, 2) | MASK_R(in[9],  0xc0, 6);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
            *(++p) = MASK_L(in[5],  0x1f, 2) | MASK_R(in[8],  0xc0, 6);SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[4],  0x1f, 3) | MASK_R(in[5],  0xe0, 5);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
            *(++p) = MASK_L(in[1],  0x0f, 3) | MASK_R(in[4],  0xe0, 5);SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[0],  0x0f, 4) | MASK_R(in[1],  0xf0, 4);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
            *(++p) = MASK_L(in[15], 0x7f, 0)                          ;SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[14], 0x7f, 1) | MASK_R(in[15], 0x80, 7);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
            *(++p) = MASK_L(in[11], 0x3f, 1) | MASK_R(in[14], 0x80, 7);SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[10], 0x3f, 2) | MASK_R(in[11], 0xc0, 6);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
            *(++p) = MASK_L(in[7],  0x1f, 2) | MASK_R(in[10], 0xc0, 6);SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[6],  0x1f, 3) | MASK_R(in[7],  0xe0, 5);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
            *(++p) = MASK_L(in[3],  0x0f, 3) | MASK_R(in[6],  0xe0, 5);SIGN_EXTEND_7(*p);SHIFT_ROUND_17(*p);\
            *(++p) = MASK_L(in[2],  0x0f, 4) | MASK_R(in[3],  0xf0, 4);SIGN_EXTEND_8(*p);SHIFT_ROUND_16(*p);\
			APPLY_MACRO (DECREMENT)

//*****************************************************************************
void decodeMTP(int d0, unsigned char *in, int *out)
{
    // Decodes a 16-byte MTP RDAC block into 24-bit samples

    int patternIndex = MASK_L(in[0], 0xf0, 0) | MASK_R(in[2], 0xf0, 4);

    int pattern = patternsMTP[patternIndex];

	int *p = out;
    switch (pattern) {

        /*=====================================================================
            PATTERN B
        */

        case 0:  // 00..00.. //
            MTP_EXTRACT_B(SHIFT_ROUND_6);
            return;

        case 1:  // 00..01.. //
            MTP_EXTRACT_B(SHIFT_ROUND_7);
            return;

        case 2:  // 00..10.. //
            MTP_EXTRACT_B(SHIFT_ROUND_8);
            return;

        case 3:  // 00..11.. //
            MTP_EXTRACT_B(SHIFT_ROUND_9);
            return;

        case 4:  // 01..00.. //
            MTP_EXTRACT_B(SHIFT_ROUND_10);
            return;

        case 5:  // 01..01.. //
            MTP_EXTRACT_B(SHIFT_ROUND_11);
			APPLY_MACRO (LIMIT_24);
            return;

        /*=====================================================================
            PATTERN D
        */

        case 6:  /* 01..10.. */
            MTP_EXTRACT_D(SHIFT_ROUND_10);
            return;

        case 7:  /* 01..11.. */
            MTP_EXTRACT_D(SHIFT_ROUND_11);
            return;

        case 8:  /* 10..00.. */
            MTP_EXTRACT_D(SHIFT_ROUND_12);
            return;

        case 9:  /* 10..01.. */
            MTP_EXTRACT_D(SHIFT_ROUND_13);
            return;

        case 10: /* 10..10.. */
            MTP_EXTRACT_D(SHIFT_ROUND_14);
			APPLY_MACRO (LIMIT_24);
            return;

        case 11: /* 10..11.. */
            MTP_EXTRACT_D(SHIFT_ROUND_15);
			APPLY_MACRO (LIMIT_24);
            return;

		/*=====================================================================
            PATTERN A
        */

        case 12: /* 110.000. */
            MTP_EXTRACT_A(SHIFT_ROUND_5);
            return;

        case 13: /* 110.001. */
            MTP_EXTRACT_A(SHIFT_ROUND_6);
            return;

        case 14: /* 110.010. */
            MTP_EXTRACT_A(SHIFT_ROUND_7);
            return;

        case 15: /* 110.011. */
            MTP_EXTRACT_A(SHIFT_ROUND_8);
            return;

        case 16: /* 110.100. */
            MTP_EXTRACT_A(SHIFT_ROUND_9);
            return;

        case 17: /* 110.101. */
            MTP_EXTRACT_A(SHIFT_ROUND_10);
			APPLY_MACRO (LIMIT_24);
            return;

        /*=====================================================================
            PATTERN B2
        */

        case 18: /* 110.110. */
            MTP_EXTRACT_B2(SHIFT_ROUND_12);
			APPLY_MACRO (LIMIT_24);
            return;

        /*=====================================================================
            PATTERN C
        */

        case 19: /* 110.111. */
            MTP_EXTRACT_C(SHIFT_ROUND_8);
            return;

        case 20: /* 111.000. */
            MTP_EXTRACT_C(SHIFT_ROUND_9);
            return;

        case 21: /* 111.001. */
            MTP_EXTRACT_C(SHIFT_ROUND_10);
            return;

        case 22: /* 111.010. */
            MTP_EXTRACT_C(SHIFT_ROUND_11);
            return;

        case 23: /* 111.011. */
            MTP_EXTRACT_C(SHIFT_ROUND_12);
            return;

        case 24: /* 111.100. */
            MTP_EXTRACT_C(SHIFT_ROUND_13);
			APPLY_MACRO (LIMIT_24);
            return;

        /*=====================================================================
            PATTERN F
        */

        case 25: /* 11101010 */
            MTP_EXTRACT_F(SHIFT_ROUND_12);
            return;

        case 26: /* 11101011 */
            MTP_EXTRACT_F(SHIFT_ROUND_13);
            return;

        case 27: /* 11101100 */
            MTP_EXTRACT_F(SHIFT_ROUND_14);
            return;

        case 28: /* 11101101 */
            MTP_EXTRACT_F(SHIFT_ROUND_15);
            return;

        case 29: /* 11101110 */
            MTP_EXTRACT_F(SHIFT_ROUND_16);
			APPLY_MACRO (LIMIT_24);
            return;

        /*=====================================================================
            PATTERN F2
        */

        case 30: /* 11101111 */
            MTP_EXTRACT_F2();
			APPLY_MACRO (LIMIT_24);
            return;

        /*=====================================================================
            PATTERN C2
        */

        case 31: /* 11111010 */
            MTP_EXTRACT_C2(SHIFT_ROUND_14);
			APPLY_MACRO (LIMIT_24);
            return;

        /*=====================================================================
            PATTERN E
        */

        case 32: /* 11111011 */
            MTP_EXTRACT_E(SHIFT_ROUND_4);
            return;

        case 33: /* 11111100 */
            MTP_EXTRACT_E(SHIFT_ROUND_5);
            return;

        case 34: /* 11111101 */
            MTP_EXTRACT_E(SHIFT_ROUND_1);
            return;

        case 35: /* 11111110 */
            MTP_EXTRACT_E(SHIFT_ROUND_2);
            return;

        case 36: /* 11111111 */
            MTP_EXTRACT_E(SHIFT_ROUND_3);
            return;

        default: break;
    }
}
