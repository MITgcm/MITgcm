C $Header: /u/gcmpack/MITgcm/model/src/packages_check.F,v 1.30 2005/10/01 18:29:07 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_CHECK
C     !INTERFACE:
      SUBROUTINE PACKAGES_CHECK( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_CHECK                                 
C     | o Check runtime activated packages have been built in.    
C     *==========================================================*
C     | All packages can be selected/deselected at build time     
C     | ( when code is compiled ) and activated/deactivated at    
C     | runtime. This routine does a quick check to trap packages 
C     | that were activated at runtime but that were not compiled 
C     | in at build time.                                         
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  Number of this instances
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_RUNCLOCK
      IF (useRunClock) CALL RUNCLOCK_CHECK( myThid )
#else
      IF (useRunClock) CALL PACKAGES_CHECK_ERROR('RUNCLOCK', myThid)
#endif

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CHECK( myThid )
#else
      IF (useKPP) CALL PACKAGES_CHECK_ERROR('KPP', myThid)
#endif

#ifdef ALLOW_PP81
      IF (usePP81) CALL PP81_CHECK( myThid )
#else
      IF (usePP81) CALL PACKAGES_CHECK_ERROR('PP81', myThid)
#endif

#ifdef ALLOW_MY82
      IF (useMY82) CALL MY82_CHECK( myThid )
#else
      IF (useMY82) CALL PACKAGES_CHECK_ERROR('MY82', myThid)
#endif

#ifdef ALLOW_GGL90
      IF (useGGL90) CALL GGL90_CHECK( myThid )
#else
      IF (useGGL90) CALL PACKAGES_CHECK_ERROR('GGL90', myThid)
#endif

#ifdef ALLOW_OPPS
      IF (useOPPS) CALL OPPS_CHECK( myThid )
#else
      IF (useOPPS) CALL PACKAGES_CHECK_ERROR('OPPS', myThid)
#endif

#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_CHECK( myThid )
#else
      IF (useGMRedi) CALL PACKAGES_CHECK_ERROR('GMREDI', myThid)
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) CALL OBCS_CHECK( myThid )
#else
      IF (useOBCS) CALL PACKAGES_CHECK_ERROR('OBCS', myThid)
#endif

#ifndef ALLOW_BULK_FORCE
      IF (useBulkForce) CALL PACKAGES_CHECK_ERROR('BULK?FORCE',myThid)
#endif

#ifndef ALLOW_THSICE
      IF (useThSIce) CALL PACKAGES_CHECK_ERROR('THSICE', myThid)
#endif

#ifndef ALLOW_AIM
      IF (useAIM) CALL PACKAGES_CHECK_ERROR('AIM', myThid)
#endif

#ifndef ALLOW_LAND
      IF (useLand) CALL PACKAGES_CHECK_ERROR('LAND', myThid)
#endif

#ifndef ALLOW_PTRACERS
      IF (usePTRACERS) CALL PACKAGES_CHECK_ERROR('PTRACERS', myThid)
#endif

#ifndef ALLOW_MATRIX
      IF (useMATRIX) CALL PACKAGES_CHECK_ERROR('MATRIX', myThid)
#endif

#ifdef ALLOW_GCHEM
      IF (useGCHEM) CALL GCHEM_CHECK( myThid )
#else
      IF (useGCHEM) CALL PACKAGES_CHECK_ERROR('GCHEM', myThid)
#endif

#ifndef ALLOW_SHAP_FILT
      IF (useSHAP_FILT) CALL PACKAGES_CHECK_ERROR('SHAP_FILT', myThid)
#endif

#ifndef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT) CALL PACKAGES_CHECK_ERROR('ZONAL_FILT',myThid)
#endif

#ifndef ALLOW_FLT
      IF (useFLT) CALL PACKAGES_CHECK_ERROR('FLT', myThid)
#endif

#ifdef ALLOW_SBO
      IF (useSBO) CALL SBO_CHECK( myThid )
#else
      IF (useSBO) CALL PACKAGES_CHECK_ERROR('SBO', myThid)
#endif

#ifdef ALLOW_SEAICE
      IF (useSEAICE) CALL SEAICE_CHECK( myThid )
#else
      IF (useSEAICE) CALL PACKAGES_CHECK_ERROR('SEAICE', myThid)
#endif

#ifndef ALLOW_CD_CODE
      IF (useCDscheme) CALL PACKAGES_CHECK_ERROR('CD_CODE', myThid)
#endif

#ifdef ALLOW_AUTODIFF
      CALL AUTODIFF_CHECK( myThid )
#endif

#ifdef ALLOW_CTRL
      CALL CTRL_CHECK( myThid )
#endif

#ifdef ALLOW_COST
      CALL COST_CHECK( myThid )
#endif

#ifdef ALLOW_GRDCHK
      CALL GRDCHK_CHECK( myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) CALL DIAGNOSTICS_CHECK( myThid )
#else
      IF (useDiagnostics) 
     &   CALL PACKAGES_CHECK_ERROR('DIAGNOSTICS', myThid )
#endif

      RETURN
      END
