C $Header: /u/gcmpack/MITgcm/pkg/gchem/GCHEM_OPTIONS.h,v 1.9 2011/12/24 01:04:47 jmc Exp $
C $Name:  $

#ifndef GCHEM_OPTIONS_H
#define GCHEM_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_GCHEM

CBOP
C    !ROUTINE: GCHEM_OPTIONS.h
C    !INTERFACE:

C    !DESCRIPTION:
C options for biogeochemistry package
CEOP

#define GCHEM_SEPARATE_FORCING

#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
