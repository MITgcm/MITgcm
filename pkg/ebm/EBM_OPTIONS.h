C $Header: /u/gcmpack/MITgcm/pkg/ebm/EBM_OPTIONS.h,v 1.2 2011/12/24 01:04:46 jmc Exp $
C $Name:  $

C CPP options file for EBM package
C Use this file for selecting CPP options within the EBM package

#ifndef EBM_OPTIONS_H
#define EBM_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_EBM
C     Package-specific Options & Macros go here

#undef EBM_WIND_PERT
#undef EBM_CLIMATE_CHANGE
#undef EBM_VERSION_1BASIN

#endif /* ALLOW_EBM */
#endif /* CD_EBM_OPTIONS_H */
