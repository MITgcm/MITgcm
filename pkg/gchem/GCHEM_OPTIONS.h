C $Header: /u/gcmpack/MITgcm/pkg/gchem/GCHEM_OPTIONS.h,v 1.8 2008/04/07 20:19:47 dfer Exp $
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

#define GCHEM_SEPARATE_FORCING

#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
