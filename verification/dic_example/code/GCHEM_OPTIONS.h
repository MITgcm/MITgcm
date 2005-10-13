C $Header: /u/gcmpack/MITgcm/verification/dic_example/code/Attic/GCHEM_OPTIONS.h,v 1.5 2005/10/13 16:26:23 stephd Exp $
C $Name:  $

#ifndef GCHEM_OPTIONS_H
#define GCHEM_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_GCHEM

#include "CPP_OPTIONS.h"

CBOP
C    !ROUTINE: GCHEM_OPTIONS.h
C    !INTERFACE:

C    !DESCRIPTION:
c options for biogeochemistry package
CEOP

#define GCHEM_SEPARATE_FORCING
#define DIC_BIOTIC
#undef  ALLOW_CFC
#undef  ALLOW_FE
#undef READ_PAR
#undef MINFE

#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
