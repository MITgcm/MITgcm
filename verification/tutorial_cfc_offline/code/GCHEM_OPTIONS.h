C $Header: /u/gcmpack/MITgcm/verification/tutorial_cfc_offline/code/Attic/GCHEM_OPTIONS.h,v 1.3 2008/04/07 22:32:47 jmc Exp $
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
C options for biogeochemistry package
CEOP

#undef  GCHEM_SEPARATE_FORCING

#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
