C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_OPTIONS.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $
 
C CPP options file for OBCS package
C
C Use this file for selecting options within the OBCS package
C
C OBCS is enabled with ALLOW_OBCS in CPP_OPTIONS.h

#include "CPP_OPTIONS.h"

#ifdef ALLOW_OBCS

C This include hooks to the Orlanski Open Boundary Radiation code
#define ALLOW_ORLANSKI

#endif /* ALLOW_OBCS */
