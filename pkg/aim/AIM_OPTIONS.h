C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/AIM_OPTIONS.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

C  CPP options file for AIM package 
C
#include "CPP_OPTIONS.h"

#ifdef ALLOW_AIM

C     Macro mapping dynamics vertical indexing (KD) to AIM vertical indexing (KA).
C     ( dynamics puts K=1 at bottom of atmos., AIM puts K=1 at top of atmos. )
#define _KD2KA( KD ) Nr-KD+1

#endif /* ALLOW_AIM */
