#ifndef DIC_OPTIONS_H
#define DIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_DIC
C     Package-specific Options & Macros go here

C ABIOTIC OPTIONS
C Compile Munhoven (2013)'s "Solvesaphe" package for pH/pCO2
C  can still select Follows et al (2006) solver in data.dic
#undef CARBONCHEM_SOLVESAPHE

C Convert ak1 and ak2 to the total pH scale consistent with other coefficients
C  in S/R CARBON_CHEM (currently on the seawater scale).
#undef CARBONCHEM_TOTALPHSCALE

C BIOTIC OPTIONS
#define DIC_BIOTIC
#define ALLOW_O2
#undef ALLOW_FE
#undef READ_PAR
#undef MINFE
#undef DIC_NO_NEG
#undef DIC_BOUNDS
C these all need to be defined for coupling to atmospheric model:
#undef USE_QSW
#undef USE_QSW_UNDERICE
#undef USE_PLOAD

C use surface salinity forcing (scaled by mean surf value) for DIC & ALK forcing
#undef ALLOW_OLD_VIRTUALFLUX

C put back bugs related to Water-Vapour in carbonate chemistry & air-sea fluxes
#undef WATERVAP_BUG

C dissolution only below saturation horizon following method by Karsten Friis
#undef CAR_DISS

C Include self-shading effect by phytoplankton
#undef LIGHT_CHL

C Include iron sediment source using DOP flux
#undef SEDFE

#endif /* ALLOW_DIC */
#endif /* DIC_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
