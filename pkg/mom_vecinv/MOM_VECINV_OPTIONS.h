C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/MOM_VECINV_OPTIONS.h,v 1.4 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

C CPP options file for mom_vecinv package
C Use this file for selecting CPP options within the mom_vecinv package

#ifndef MOM_VECINV_OPTIONS_H
#define MOM_VECINV_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MOM_VECINV
C     Package-specific Options & Macros go here

C use the original discretization (not recommended) for biharmonic viscosity
C   that was in mom_vi_hdissip.F, version 1.1.2.1
#undef MOM_VI_ORIGINAL_VISCA4

#endif /* ALLOW_MOM_VECINV */
#endif /* MOM_VECINV_OPTIONS_H */
