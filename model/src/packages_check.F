C $Header: /u/gcmpack/MITgcm/model/src/packages_check.F,v 1.39 2007/03/07 00:00:06 jmc Exp $
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
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C---  Start with non-standard packages (without or with non standard flag)

#ifndef ALLOW_MNC
      IF (useMNC) THEN
        WRITE(msgBuf,'(2A)') '***WARNING***',
     &   ' PACKAGES_CHECK: useMNC is TRUE'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)') '***WARNING***',
     &   ' but pkg/mnc has not been compiled (#undef ALLOW_MNC)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
      ENDIF
#endif

#ifndef ALLOW_MOM_VECINV
      IF ( momStepping .AND. vectorInvariantMomentum ) THEN
        WRITE(msgBuf,'(2A)')
     &   'PACKAGES_CHECK: cannot step forward Momentum',
     &   ' without pkg/mom_vecinv'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'Re-compile with pkg "mom_vecinv" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif
#ifndef ALLOW_MOM_FLUXFORM
      IF ( momStepping .AND. .NOT.vectorInvariantMomentum ) THEN
        WRITE(msgBuf,'(2A)')
     &   'PACKAGES_CHECK: cannot step forward Momentum',
     &   ' without pkg/mom_fluxform'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'Re-compile with pkg "mom_fluxform" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif

#ifdef ALLOW_GENERIC_ADVDIFF
      IF (useGAD) CALL GAD_CHECK( myThid )
#else
      IF ( tempStepping .OR. saltStepping ) THEN
        WRITE(msgBuf,'(2A)')
     &  'PACKAGES_CHECK: cannot step forward Temp or Salt',
     &  ' without pkg/generic_advdiff'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &  'Re-compile with pkg "generic_advdiff" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif

#ifndef ALLOW_CD_CODE
      IF (useCDscheme) THEN
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'useCDscheme is TRUE but pkg/cd_code has not been compiled'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif

#ifndef ALLOW_MONITOR
C     If monitorFreq is finite, make sure the pkg/monitor is being compiled
      IF (monitorFreq.NE.0.) THEN
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'monitorFreq <> 0  but pkg/monitor has not been compiled'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'Re-compile with pkg "monitor" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif

#ifndef ALLOW_TIMEAVE
C     If taveFreq is finite, make sure the pkg/timeave is being compiled
      IF (taveFreq.NE.0.) THEN
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'taveFreq <> 0  but pkg/timeave has been not compiled'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'Re-compile with pkg "timeave" in packages.conf'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif

C---  Continue with standard packages (with standard usePKG flag)

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

#ifndef ALLOW_ATM2D
      IF (useATM2D) CALL PACKAGES_CHECK_ERROR('ATM2D', myThid)
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

#ifndef ALLOW_OFFLINE
      IF (useOffLine) CALL PACKAGES_CHECK_ERROR('OFFLINE', myThid)
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

#ifdef ALLOW_SHELFICE
      IF (useShelfIce) CALL SHELFICE_CHECK( myThid )
#else
      IF (useShelfIce) CALL PACKAGES_CHECK_ERROR('SHELFICE', myThid)
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
      IF (useGRDCHK) CALL GRDCHK_CHECK( myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) CALL DIAGNOSTICS_CHECK( myThid )
#else
      IF (useDiagnostics)
     &   CALL PACKAGES_CHECK_ERROR('DIAGNOSTICS', myThid )
#endif

#ifdef ALLOW_REGRID
      IF (useREGRID) CALL REGRID_CHECK( myThid )
#else
      IF (useREGRID) CALL PACKAGES_CHECK_ERROR('REGRID', myThid)
#endif

#ifdef ALLOW_MYPACKAGE
      IF (useMYPACKAGE) CALL MYPACKAGE_CHECK( myThid )
#else
      IF (useMYPACKAGE) CALL PACKAGES_CHECK_ERROR('MYPACKAGE', myThid)
#endif

      RETURN
      END
