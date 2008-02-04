/*

GCB 5.1.04

The nmrPipe data file format involves a 512-element header.
Each element can be treated as a four-byte floating-point
number.
The following are indices into this array corresponding
to useful quantities.

*/

#ifndef PIPEHDR_H
#define PIPEHDR_H

#define AWOL_HDR_SIZE  512

#define AWOL_NP_Z 		15
#define AWOL_ORIG_Z 	12
#define AWOL_SW_Z 		11
#define AWOL_SF_Z 		10
#define AWOL_TRUNC_Z 	50
#define AWOL_SW_X 	100
#define AWOL_ORIG_X 	101
#define AWOL_NP_X 	99
#define AWOL_SF_X 	119
#define AWOL_ORIG_Y 	249
#define AWOL_SW_Y 	229
#define AWOL_SF_Y 	218
#define AWOL_NP_Y 	219
#define AWOL_TRUNC_Y 	 428
#define AWOL_TRUNC_X 	 95

/*
 * real / complex flags
 * - these are set to 1.0 if the data in that dimension has had the
 * imaginary points deleted.  = 0.0 if imaginary data is present.
 */
#define AWOL_REALFLAG_X 56
#define AWOL_REALFLAG_Y 55
#define AWOL_REALFLAG_Z 51

/* put items in symbol table */
enum awol_items {
	awol_np_y = AWOL_NP_Y,
};

#endif /* PIPEHDR_H */



