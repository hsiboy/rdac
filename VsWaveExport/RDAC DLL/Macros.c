// Macros for decode operations

#define SIGN_EXTEND_1(xx)  xx = -(xx & 0x00000001) | xx
#define SIGN_EXTEND_2(xx)  xx = -(xx & 0x00000002) | xx
#define SIGN_EXTEND_3(xx)  xx = -(xx & 0x00000004) | xx
#define SIGN_EXTEND_4(xx)  xx = -(xx & 0x00000008) | xx
#define SIGN_EXTEND_5(xx)  xx = -(xx & 0x00000010) | xx
#define SIGN_EXTEND_6(xx)  xx = -(xx & 0x00000020) | xx
#define SIGN_EXTEND_7(xx)  xx = -(xx & 0x00000040) | xx
#define SIGN_EXTEND_8(xx)  xx = -(xx & 0x00000080) | xx
#define SIGN_EXTEND_9(xx)  xx = -(xx & 0x00000100) | xx
#define SIGN_EXTEND_10(xx) xx = -(xx & 0x00000200) | xx
#define SIGN_EXTEND_11(xx) xx = -(xx & 0x00000400) | xx
#define SIGN_EXTEND_12(xx) xx = -(xx & 0x00000800) | xx
#define SIGN_EXTEND_13(xx) xx = -(xx & 0x00001000) | xx
#define SIGN_EXTEND_14(xx) xx = -(xx & 0x00002000) | xx

#define SHIFT_ROUND_0(xx)  xx = (xx)
#define SHIFT_ROUND_1(xx)  xx = (xx<<1)
#define SHIFT_ROUND_2(xx)  xx = ((xx<<2)  | 0x00000002)
#define SHIFT_ROUND_3(xx)  xx = ((xx<<3)  | 0x00000004)
#define SHIFT_ROUND_4(xx)  xx = ((xx<<4)  | 0x00000008)
#define SHIFT_ROUND_5(xx)  xx = ((xx<<5)  | 0x00000010)
#define SHIFT_ROUND_6(xx)  xx = ((xx<<6)  | 0x00000020)
#define SHIFT_ROUND_7(xx)  xx = ((xx<<7)  | 0x00000040)
#define SHIFT_ROUND_8(xx)  xx = ((xx<<8)  | 0x00000080)
#define SHIFT_ROUND_9(xx)  xx = ((xx<<9)  | 0x00000100)
#define SHIFT_ROUND_10(xx) xx = ((xx<<10) | 0x00000200)
#define SHIFT_ROUND_11(xx) xx = ((xx<<11) | 0x00000400)
#define SHIFT_ROUND_12(xx) xx = ((xx<<12) | 0x00000800)
#define SHIFT_ROUND_13(xx) xx = ((xx<<13) | 0x00001000)
#define SHIFT_ROUND_14(xx) xx = ((xx<<14) | 0x00002000)
#define SHIFT_ROUND_15(xx) xx = ((xx<<15) | 0x00004000)
#define SHIFT_ROUND_16(xx) xx = ((xx<<16) | 0x00008000)
#define SHIFT_ROUND_17(xx) xx = ((xx<<17) | 0x00010000)

#define SHIFT_RIGHT_6(xx) xx = (xx>>6)

#define APPLY_MACRO(MacroName) \
            MacroName(out[0]);  MacroName(out[1]);  MacroName(out[2]);  MacroName(out[3]);  \
            MacroName(out[4]);  MacroName(out[5]);  MacroName(out[6]);  MacroName(out[7]);  \
            MacroName(out[8]);  MacroName(out[9]);  MacroName(out[10]); MacroName(out[11]); \
            MacroName(out[12]); MacroName(out[13]); MacroName(out[14]); MacroName(out[15])

#define DECREMENT(xx) xx--

#define INTERPOLATE_ADD(xx, aa, bb) xx = xx + ((aa + bb)>>1);
#define INTERPOLATE_SUB(xx, aa, bb) xx = xx - ((aa + bb)>>1);

#define LIMIT_24(xx) xx = xx<-8388608?-8388608:(xx>8388607?8388607:xx)
#define LIMIT_16(xx) xx = xx<-32768?-32768:(xx>32767?32767:xx)

#define MASK_L(xx, mask, shift) ((xx & mask) << shift)
#define MASK_R(xx, mask, shift) ((xx & mask) >> shift)

#define ZAP(xx) xx = 0

#define ABS(xx) (unsigned int) (xx<0? -xx:xx)

#define INTERPOLATE_ADD_2 \
            DECREMENT(out[7]); DECREMENT(out[15]);  \
            INTERPOLATE_ADD(out[3],  d0,      out[7]);  \
            INTERPOLATE_ADD(out[1],  d0,      out[3]);  \
            INTERPOLATE_ADD(out[0],  d0,      out[1]);  \
            INTERPOLATE_ADD(out[2],  out[3],  out[1]);  \
            INTERPOLATE_ADD(out[5],  out[3],  out[7]);  \
            INTERPOLATE_ADD(out[4],  out[3],  out[5]);  \
            INTERPOLATE_ADD(out[6],  out[7],  out[5]);  \
            INTERPOLATE_ADD(out[11], out[7],  out[15]); \
            INTERPOLATE_ADD(out[9],  out[7],  out[11]); \
            INTERPOLATE_ADD(out[8],  out[7],  out[9]);  \
            INTERPOLATE_ADD(out[10], out[11], out[9]);  \
            INTERPOLATE_ADD(out[13], out[11], out[15]); \
            INTERPOLATE_ADD(out[12], out[11], out[13]); \
            INTERPOLATE_ADD(out[14], out[13], out[15])

#define INTERPOLATE_ADD_4 \
            DECREMENT(out[3]); DECREMENT(out[7]); DECREMENT(out[11]); DECREMENT(out[15]); \
            INTERPOLATE_ADD(out[1],  d0,      out[3]);  \
            INTERPOLATE_ADD(out[0],  d0,      out[1]);  \
            INTERPOLATE_ADD(out[2],  out[3],  out[1]);  \
            INTERPOLATE_ADD(out[5],  out[3],  out[7]);  \
            INTERPOLATE_ADD(out[4],  out[3],  out[5]);  \
            INTERPOLATE_ADD(out[6],  out[7],  out[5]);  \
            INTERPOLATE_ADD(out[9],  out[7],  out[11]); \
            INTERPOLATE_ADD(out[8],  out[7],  out[9]);  \
            INTERPOLATE_ADD(out[10], out[11], out[9]);  \
            INTERPOLATE_ADD(out[13], out[11], out[15]); \
            INTERPOLATE_ADD(out[12], out[11], out[13]); \
            INTERPOLATE_ADD(out[14], out[13], out[15])

#define INTERPOLATE_ADD_8 \
            DECREMENT(out[1]); DECREMENT(out[3]);  DECREMENT(out[5]);  DECREMENT(out[7]);  \
            DECREMENT(out[9]); DECREMENT(out[11]); DECREMENT(out[13]); DECREMENT(out[15]); \
            INTERPOLATE_ADD(out[0],  out[1],      d0);  \
            INTERPOLATE_ADD(out[2],  out[1],  out[3]);  \
            INTERPOLATE_ADD(out[4],  out[5],  out[3]);  \
            INTERPOLATE_ADD(out[6],  out[5],  out[7]);  \
            INTERPOLATE_ADD(out[8],  out[9],  out[7]);  \
            INTERPOLATE_ADD(out[10], out[9],  out[11]); \
            INTERPOLATE_ADD(out[12], out[13], out[11]); \
            INTERPOLATE_ADD(out[14], out[13], out[15])

#define INTERPOLATE_SUB_2 \
            INTERPOLATE_SUB(in[ 0],     d0, in[ 1]); \
            INTERPOLATE_SUB(in[ 2], in[ 1], in[ 3]); \
            INTERPOLATE_SUB(in[ 4], in[ 3], in[ 5]); \
            INTERPOLATE_SUB(in[ 6], in[ 5], in[ 7]); \
            INTERPOLATE_SUB(in[ 8], in[ 7], in[ 9]); \
            INTERPOLATE_SUB(in[10], in[ 9], in[11]); \
            INTERPOLATE_SUB(in[12], in[11], in[13]); \
            INTERPOLATE_SUB(in[14], in[13], in[15]); \
            INTERPOLATE_SUB(in[ 1],     d0, in[ 3]); \
            INTERPOLATE_SUB(in[ 5], in[ 3], in[ 7]); \
            INTERPOLATE_SUB(in[ 9], in[ 7], in[11]); \
            INTERPOLATE_SUB(in[13], in[11], in[15]); \
            INTERPOLATE_SUB(in[ 3],     d0, in[ 7]); \
            INTERPOLATE_SUB(in[11], in[ 7], in[15])

#define INTERPOLATE_SUB_4 \
            INTERPOLATE_SUB(in[ 0],     d0, in[ 1]); \
            INTERPOLATE_SUB(in[ 2], in[ 1], in[ 3]); \
            INTERPOLATE_SUB(in[ 4], in[ 3], in[ 5]); \
            INTERPOLATE_SUB(in[ 6], in[ 5], in[ 7]); \
            INTERPOLATE_SUB(in[ 8], in[ 7], in[ 9]); \
            INTERPOLATE_SUB(in[10], in[ 9], in[11]); \
            INTERPOLATE_SUB(in[12], in[11], in[13]); \
            INTERPOLATE_SUB(in[14], in[13], in[15]); \
            INTERPOLATE_SUB(in[ 1],     d0, in[ 3]); \
            INTERPOLATE_SUB(in[ 5], in[ 3], in[ 7]); \
            INTERPOLATE_SUB(in[ 9], in[ 7], in[11]); \
            INTERPOLATE_SUB(in[13], in[11], in[15])

#define INTERPOLATE_SUB_8 \
            INTERPOLATE_SUB(in[ 0], in[ 1],     d0); \
            INTERPOLATE_SUB(in[ 2], in[ 1], in[ 3]); \
            INTERPOLATE_SUB(in[ 4], in[ 5], in[ 3]); \
            INTERPOLATE_SUB(in[ 6], in[ 5], in[ 7]); \
            INTERPOLATE_SUB(in[ 8], in[ 9], in[ 7]); \
            INTERPOLATE_SUB(in[10], in[ 9], in[11]); \
            INTERPOLATE_SUB(in[12], in[13], in[11]); \
            INTERPOLATE_SUB(in[14], in[13], in[15])


