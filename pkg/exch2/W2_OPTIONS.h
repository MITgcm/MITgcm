C $Header: /u/gcmpack/MITgcm/pkg/exch2/W2_OPTIONS.h,v 1.2 2005/07/24 01:35:06 jmc Exp $
C $Name:  $
 
C CPP options file for EXCH2 package

#ifndef W2_OPTIONS_H
#define W2_OPTIONS_H

C ... W2_USE_E2_SAFEMODE description ...
#define W2_USE_E2_SAFEMODE

C Debug mode option:
#undef  W2_E2_DEBUG_ON

C Fill null regions (face-corner halo regions) with e2FillValue_RX (=0)
C notes: for testing (allow to check that results are not affected)
#undef W2_FILL_NULL_REGIONS

#endif /* W2_OPTIONS_H */
