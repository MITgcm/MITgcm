
#include "CPP_OPTIONS.h"

CStartOfInterface
CBOP
C     !ROUTINE: PACKAGES_READPARMS
C     !INTERFACE:
      SUBROUTINE PACKAGES_READPARMS(myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_READPARMS                             
C     | o Read runtime package configuration parameters           
C     *==========================================================*
C     |   Packages can have internal runtime configuration        
C     |   parameters. A package provides a routine                
C     |   ${PKGNAME}_READ_PARMS to read these parameters. In      
C     |   general this routine should read parameters from a file 
C     |   called data.${pkgname}.                                 
C     |   This routine (S/R PACKAGES_READPARMS) calls per-package 
C     |   parameter reading routines.                             
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     PACKAGES_READPARMS
C       |
C       |-- GMREDI_READPARMS
C       |
C       |-- KPP_READPARMS
C       |
C       |-- SBO_READPARMS
C       |
C       |-- SEAICE_READPARMS
C       |
C       |-- SHAP_FILT_READPARMS
C       |
C       |-- ZONAL_FILT_READPARMS
C       |
C       |-- OBCS_READPARMS
C       |
C       |-- COST_READPARMS
C       |
C       |-- CTRL_INIT
C       |
C       |-- OPTIM_READPARMS
C       |
C       |-- GRDCHK_READPARMS
C       |
C       |-- ECCO_READPARMS


C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C--   Initialize GM/Redi parameters
      IF (useGMRedi) CALL GMREDI_READPARMS( myThid )
#endif

#ifdef ALLOW_KPP
C--   Initialize KPP parameters
      IF (useKPP) CALL KPP_READPARMS( myThid )
#endif

#ifdef ALLOW_SBO
C--   Initialize SBO parameters
      IF (useSBO) CALL SBO_READPARMS( myThid )
#endif

#ifdef ALLOW_SEAICE
C--   Initialize SEAICE parameters
      IF (useSEAICE) CALL SEAICE_READPARMS( myThid )
#endif

#ifdef ALLOW_SHAP_FILT 
      IF (useSHAP_FILT) CALL SHAP_FILT_READPARMS( myThid )
#endif

#ifdef ALLOW_ZONAL_FILT 
      IF (useZONAL_FILT) CALL ZONAL_FILT_READPARMS( myThid )
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) CALL OBCS_READPARMS( myThid )
#endif

cswdblk -- add ---
#ifdef ALLOW_BULK_FORCE
C -- initialize Bulkf parameters
      IF (useBulkForce) call BULKF_READPARMS( myThid )
#endif
cswdblk --- end add ---

cswdice -- add ---
#ifdef ALLOW_THERM_SEAICE
C -- initialize Sea Ice parameters
      IF (useThermSeaIce) call ICE_READPARMS( myThid )
#endif
cswdice --- end add ---


#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) CALL PTRACERS_READPARMS( myThid )
#endif

#if (defined (ALLOW_ADJOINT_RUN) || \
     defined (ALLOW_TANGENTLINEAR_RUN) || \
     defined (ALLOW_ECCO_OPTIMIZATION))
C--   Initialise the optim. parameters.
      call optim_readparms( myThid )
C--   Initialise the control parameters
      call ctrl_readparms( myThid )
C--   Initialise the cost parameters
      call cost_readparms( mythid )
      _BARRIER
#endif

#ifdef ALLOW_GRADIENT_CHECK
C--   Initialise gradient check parameters
      if (useGrdchk) call grdchk_readparms( myThid )
#endif

#ifdef ALLOW_CALENDAR
C--   Initialise the calendar package.
      call cal_readparms( myThid )
#endif

C--   Custom routine to set forcing fields.
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
      call exf_readparms  ( mythid )
#endif

#ifdef INCLUDE_ECCO_PACKAGE
c--   Initialize ECCO package parameters
      IF (useECCO) call ecco_readparms ( myThid )
#endif

      END
