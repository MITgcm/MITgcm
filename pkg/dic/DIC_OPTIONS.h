#ifndef DIC_OPTIONS_H
#define DIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_DIC
C     Package-specific Options & Macros go here

C ABIOTIC OPTIONS
C Use default OCMIP2 dissociation constants, or use the constants from 
C  Munhoven (2013)'s "Solvesaphe" package
#undef CARBONCHEM_SOLVESAPHE_CONST

C Munhoven (2013)'s "Solvesaphe" coefficients have several options:
C Select the coefficients for K1 and K2 for the carbonate system:
C   the default is OCMIP2 coefficents from Millero (1995) using the data from Mehrbach
C   i.e. the same as S/R CARBON_COEFFS
C   can also use the Millero (1995) "consensus" coefficients
#undef CARBONCHEM_MILLERO95K1K2COM
C   Coefficients defined by Roy et al. (1993)
#undef CARBONCHEM_ROY93K1K2
C   Coefficients defined by Luecker et al. (2000) valid for 2 < T < 35 and 19 < S < 43
#undef CARBONCHEM_LUECKER00K1K2
C   Coefficients defined by Millero et al. (2010) valid for 0 < T < 50°C and 1 < S < 50
#undef CARBONCHEM_MILLERO10K1K2
C   Coefficients defined by Waters et al. (2014) valid for 0 < T < 50°C and 1 < S < 50
#undef CARBONCHEM_WATERS14K1K2

C Select the coefficient KF for the fluoride system:
C   default is Dickson and Riley (1979) i.e. the same as S/R CARBON_COEFFS
C   or can use Perez and Fraga (1987)
#undef CARBONCHEM_PEREZ87KF

C Select the equation for Borate concentration, BT.
C   default is Uppstrom (1974) i.e. the same as S/R CARBON_COEFFS
C   or can also use Lee et al (2010)
#undef CARBONCHEM_LEE10BT

C Select the equation for Fluoride concentration, FT.
C    default is Riley (1965) i.e. the same as S/R CARBON_COEFFS
C    or can also use Culkin (1965)
#undef CARBONCHEM_CULKIN65FT

C  When using the OCMIP2 coefficients however, make sure they are on the same pH scale
C#define CARBONCHEM_TOTALPHSCALE

C Select the solver to calculate pH and evaluate surface ocean pCO2
C  the default is  Follows et al. (2006) 
C  can also use Munhoven (2013)'s "Solvesaphe" routines
#undef CARBONCHEM_SOLVESAPHE_PCO2
C   Note: If you use "Solvesaphe" to calculate pCO2 and pH, you will use 
C    "Solvesaphe" coefficients too.

C Munhoven (2013)'s "Solvesaphe" actually has three solvers 
C  the default is to use the "general" solver, but can also use the "fast" or "secant" solvers
C#undef USE_SOLVESAPHE_SEC
C#undef USE_SOLVESAPHE_FAST

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
