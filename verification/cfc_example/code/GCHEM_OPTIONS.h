C$Header: /u/gcmpack/MITgcm/verification/cfc_example/code/GCHEM_OPTIONS.h,v 1.1 2004/11/29 22:38:16 mlosch Exp $
C$Name:  $

#include "CPP_OPTIONS.h"
#ifdef ALLOW_PTRACERS

C    !ROUTINE: GCHEM_OPTIONS.h
C    !INTERFACE:

C    !DESCRIPTION:
c options for biogeochemistry package
#undef GCHEM_SEPARATE_FORCING
#define ALLOW_CFC
#undef DIC_BIOTIC
#undef  ALLOW_FE

#undef ALLOW_DIC_COST

#endif
