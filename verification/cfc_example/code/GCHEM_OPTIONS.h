C $Header: /u/gcmpack/MITgcm/verification/cfc_example/code/GCHEM_OPTIONS.h,v 1.2 2004/12/05 22:34:51 jmc Exp $
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

#undef  GCHEM_SEPARATE_FORCING
#undef  DIC_BIOTIC
#define ALLOW_CFC
#undef  ALLOW_FE


#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
