C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_OPTIONS.h,v 1.12 2013/08/11 02:54:51 jmc Exp $
C $Name:  $

C  CPP options file for AIM package

#ifndef AIM_OPTIONS_H
#define AIM_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_AIM
C     Package-specific Options & Macros go here

C  allow dew to form on land (=negative evaporation)
#undef ALLOW_DEW_ON_LAND

C  calculate top-atmosphere insolation using orbital parameters
C   (obliquity, eccentricity ...) provided as run-time params
#undef ALLOW_INSOLATION

C  allow 3D cloud fraction for computation of radiation
#undef ALLOW_CLOUD_3D

C  allow CO2 concentration
#undef ALLOW_AIM_CO2

C  allow Clear-Sky diagnostic:
#define ALLOW_CLR_SKY_DIAG

#ifdef ALLOW_TIMEAVE
C  allow time average diagnostic:
# define ALLOW_AIM_TAVE
#endif

C   Macro mapping dynamics vertical indexing (KD) to AIM vertical indexing (KA).
C   ( dynamics puts K=1 at bottom of atmos., AIM puts K=1 at top of atmos. )
#define _KD2KA( KD ) Nr-KD+1

#endif /* ALLOW_AIM */
#endif /* AIM_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
