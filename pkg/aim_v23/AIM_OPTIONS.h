C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_OPTIONS.h,v 1.2 2003/01/03 03:51:27 jmc Exp $
C $Name:  $

C  CPP options file for AIM package 
C
#include "CPP_OPTIONS.h"

C  restore the old AIM interface (ref: coupled run, summer 2000):
C  - use a simple shift of 1/2 mesh for mapping A.grid <-> C.grid.
C  - do not include minor changes (added to avoid negative Q).
#undef OLD_AIM_INTERFACE

#ifdef ALLOW_AIM

C  allow time average diagnostic:
#define ALLOW_AIM_TAVE

C   Macro mapping dynamics vertical indexing (KD) to AIM vertical indexing (KA).
C   ( dynamics puts K=1 at bottom of atmos., AIM puts K=1 at top of atmos. )
#define _KD2KA( KD ) Nr-KD+1

#endif /* ALLOW_AIM */
