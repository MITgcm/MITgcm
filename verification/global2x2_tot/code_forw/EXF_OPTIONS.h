C
#ifndef EXF_OPTIONS_H
#define EXF_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_EXF

#include "CPP_OPTIONS.h"

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************
C 
C o Include/exclude the external forcing package. To use this package,
C   you have to include the calendar tool as well. KPP can be switched
C   on or off. The implementation automatically takes care of this.
#define INCLUDE_EXTERNAL_FORCING_PACKAGE

C   Do more printout for the protocol file than usual.
#undef EXF_VERBOSE

C   Bulk formulae related flags.
#undef  ALLOW_ATM_TEMP
#undef  ALLOW_ATM_WIND
#if (defined (ALLOW_ATM_TEMP) || \
     defined (ALLOW_ATM_WIND))
# define ALLOW_BULKFORMULAE
#endif

C   Read in runoff data
#define ALLOW_RUNOFF

C   Relaxation to monthly climatologies.
#define  ALLOW_CLIMSST_RELAXATION
#define  ALLOW_CLIMSSS_RELAXATION

#endif /* ALLOW_EXF */
#endif /* EXF_OPTIONS_H */

