C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/MOM_FLUXFORM_OPTIONS.h,v 1.3 2007/10/28 21:38:21 jmc Exp $
C $Name:  $

C CPP options file for mom_fluxform package
C
C Use this file for selecting CPP options within the mom_fluxform package

#ifndef MOM_FLUXFORM_OPTIONS_H
#define MOM_FLUXFORM_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_MOM_FLUXFORM

#include "CPP_OPTIONS.h"

C CPP macros go here

C A trick to conserve U,V momemtum next to a step (vertical plane)
C  or a coastline edge (horizontal plane).
#undef MOM_BOUNDARY_CONSERVE

#endif /* ALLOW_MOM_FLUXFORM */
#endif /* MOM_FLUXFORM_OPTIONS_H */
