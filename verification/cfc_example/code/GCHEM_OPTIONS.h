C $Header: /u/gcmpack/MITgcm/verification/cfc_example/code/GCHEM_OPTIONS.h,v 1.3 2005/12/06 16:51:54 stephd Exp $
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

#define ALLOW_CFC


#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
