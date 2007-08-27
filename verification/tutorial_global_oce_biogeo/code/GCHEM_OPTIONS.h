C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_biogeo/code/Attic/GCHEM_OPTIONS.h,v 1.5 2007/08/27 19:47:51 dfer Exp $
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

#define GCHEM_SEPARATE_FORCING

c------------------------------------
c specifics for dic pkg
#define DIC_BIOTIC
#undef  ALLOW_FE
#define ALLOW_O2
#undef READ_PAR
#undef MINFE
c these all need to be defined for coupling to 
c atmospheric model
#undef USE_QSW
#undef USE_ATMOSCO2
#undef USE_PLOAD

#define ALLOW_OLD_VIRTUALFLUX
c -------------------------------------

#undef  ALLOW_CFC

#endif /* ALLOW_GCHEM */
#endif /* GCHEM_OPTIONS_H */
