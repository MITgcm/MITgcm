C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code_ad_openad/Attic/PTRACERS.h,v 1.1 2007/11/05 19:24:58 jmc Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: PTRACERS.h
C    !INTERFACE:
C #include PTRACERS.h

C    !DESCRIPTION:
C A temporary header file left from before the split in 2 files:
C  PTRACERS_PARAMS.h & PTRACERS_FIELDS.h
C Used to contain passive tracer fields and parameters.
C This allows old customized fortran code to compile and be used
C  during the transition period ; but this file will be removed afterward.
C Otherwise, fortran header file should not includes others fortran header
C  files.

C
CEOP

#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

#endif /* ALLOW_PTRACERS */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
