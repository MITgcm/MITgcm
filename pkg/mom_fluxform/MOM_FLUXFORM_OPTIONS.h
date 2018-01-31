C CPP options file for mom_fluxform package
C Use this file for selecting CPP options within the mom_fluxform package

#ifndef MOM_FLUXFORM_OPTIONS_H
#define MOM_FLUXFORM_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MOM_FLUXFORM
C     Package-specific Options & Macros go here

C A trick to conserve U,V momemtum next to a step (vertical plane)
C  or a coastline edge (horizontal plane).
#undef MOM_BOUNDARY_CONSERVE

#endif /* ALLOW_MOM_FLUXFORM */
#endif /* MOM_FLUXFORM_OPTIONS_H */
