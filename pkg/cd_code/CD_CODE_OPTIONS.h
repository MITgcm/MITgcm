C $Header: /u/gcmpack/MITgcm/pkg/cd_code/CD_CODE_OPTIONS.h,v 1.3 2011/12/24 01:04:45 jmc Exp $
C $Name:  $

C CPP options file for CD_CODE package
C Use this file for selecting CPP options within the cd_code package

#ifndef CD_CODE_OPTIONS_H
#define CD_CODE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_CD_CODE
C     Package-specific Options & Macros go here

C Change defaut to get the same time-stepping for D-grid momentum and
C  C-grid momentum. D-grid velocity used to be stepped forward in time
C  with Adams-Bashforth only on surface pressure term. Tests show that
C  using AB on D-grid coriolis term improves stability (as expected from
C  CD-scheme paper). The following 2 options allow to reproduce old results.
#undef CD_CODE_NO_AB_MOMENTUM
#undef CD_CODE_NO_AB_CORIOLIS

#endif /* ALLOW_CD_CODE */
#endif /* CD_CODE_OPTIONS_H */
