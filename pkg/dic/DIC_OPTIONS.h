C $Header: /u/gcmpack/MITgcm/pkg/dic/DIC_OPTIONS.h,v 1.8 2011/10/07 21:36:39 dfer Exp $
C $Name:  $

#ifndef DIC_OPTIONS_H
#define DIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_DIC

#include "CPP_OPTIONS.h"

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

C put back bugs related to Water-Vapour in carbonate chemistry & air-sea fluxes
#undef WATERVAP_BUG

C dissolution only below saturation horizon following method by Karsten Friis
#undef CAR_DISS

#endif /* ALLOW_DIC */
#endif /* DIC_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
