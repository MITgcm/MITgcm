C $Header: /u/gcmpack/MITgcm/verification/global_with_exf/code/Attic/ECCO_CPPOPTIONS.h,v 1.8 2003/11/25 19:04:17 heimbach Exp $
C $Name:  $

#ifndef ECCO_CPPOPTIONS_H
#define ECCO_CPPOPTIONS_H
#include "PACKAGES_CONFIG.h"

#include "CPP_OPTIONS.h"

C CPP flags controlling which code is included in the files that
C will be compiled.
C
C ********************************************************************
C ***                     Calendar Package                         ***
C ********************************************************************
C 
C CPP flags controlling which code is included in the files that
C will be compiled.

CPH >>>>>> THERE ARE NO MORE CAL OPTIONS TO BE SET <<<<<<

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************

C   Do more printout for the protocol file than usual.
#define EXF_VERBOSE

C   Bulk formulae related flags.
#undef  ALLOW_ATM_TEMP
#undef  ALLOW_ATM_WIND
#if (defined (ALLOW_ATM_TEMP) || \
     defined (ALLOW_ATM_WIND))
# define ALLOW_BULKFORMULAE
#endif

C   Relaxation to monthly climatologies.
#undef ALLOW_CLIM_CYCLIC
#undef  ALLOW_CLIMTEMP_RELAXATION
#undef  ALLOW_CLIMSALT_RELAXATION
#define  ALLOW_CLIMSST_RELAXATION
#define  ALLOW_CLIMSSS_RELAXATION

C   Relaxation to monthly climatologies.
#ifdef ALLOW_CLIMSST_RELAXATION
# define  ALLOW_MONTHLY_CLIMSST_RELAXATION
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
# define  ALLOW_MONTHLY_CLIMSSS_RELAXATION
#endif

C   Relaxation to constant surface fields.
#undef  ALLOW_CONST_SST_RELAXATION
#undef  ALLOW_CONST_SSS_RELAXATION

C   Use spatial interpolation to interpolate
C   forcing files from input grid to model grid.
#define USE_EXF_INTERPOLATION

#endif /* ECCO_CPPOPTIONS_H */
