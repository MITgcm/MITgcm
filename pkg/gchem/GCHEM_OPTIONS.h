C $Header: /u/gcmpack/MITgcm/pkg/gchem/GCHEM_OPTIONS.h,v 1.5 2004/11/19 21:28:47 mlosch Exp $
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

#define PTRACERS_SEPARATE_FORCING
#define DIC_BIOTIC
#undef  ALLOW_CFC
#undef  ALLOW_FE

#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
