C $Header: /u/gcmpack/MITgcm/model/src/packages_readparms.F,v 1.7 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

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
C       |-- SHAP_FILT_READPARMS
C       |
C       |-- OBCS_READPARMS
C       |
C       |-- AIM_READPARMS
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

#ifdef ALLOW_SHAP_FILT 
      IF (useSHAP_FILT) CALL SHAP_FILT_READPARMS( myThid )
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) CALL OBCS_READPARMS( myThid )
#endif

#ifdef ALLOW_AIM
c--   Read in AIM package parameters
C     Currently AIM does not have any runtime parameters
C     IF (useAIM) CALL AIM_READPARMS ( myThid )
#endif

#ifdef ALLOW_ADJOINT_RUN

C--   Initialise the cost function.
      call cost_readparms( mythid )
      _BARRIER

C--   Initialise the control vector.
      CALL ctrl_init( myThid )
      _BARRIER
C--   Initialise the optim. parameters.
      call optim_readparms( myThid )
      _BARRIER

#ifdef ALLOW_GRADIENT_CHECK
C--   Initialise gradient check parameters
      if (useGrdchk) call grdchk_readparms( myThid )
#endif

#endif /* ALLOW_ADJOINT_RUN */

#ifdef INCLUDE_ECCO_PACKAGE
c--   Initialize ECCO package parameters
      IF (useECCO) CALL ECCO_READPARMS ( myThid )
#endif

      END
