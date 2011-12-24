C $Header: /u/gcmpack/MITgcm/verification/tutorial_dic_adjoffline/code_ad/DIC_OPTIONS.h,v 1.4 2011/12/24 01:17:55 jmc Exp $
C $Name:  $

#ifndef DIC_OPTIONS_H
#define DIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_DIC
C     Package-specific Options & Macros go here

#define DIC_BIOTIC
#define ALLOW_FE
#define ALLOW_O2
#undef READ_PAR
#undef MINFE
#undef DIC_NO_NEG
c these all need to be defined for coupling to
c atmospheric model
#undef USE_QSW
#undef USE_QSW_UNDERICE
#undef USE_ATMOSCO2
#undef USE_PLOAD

#undef ALLOW_OLD_VIRTUALFLUX

C put back bugs related to Water-Vapour in carbonate chemistry & air-sea fluxes
#undef WATERVAP_BUG

C dissolution only below saturation horizon following method by Karsten Friis
#undef CAR_DISS

#define AD_SAFE

#endif /* ALLOW_DIC */
#endif /* DIC_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
