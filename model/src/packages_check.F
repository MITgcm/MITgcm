C $Header: /u/gcmpack/MITgcm/model/src/packages_check.F,v 1.55 2011/08/07 07:08:15 dimitri Exp $
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
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A)')
     &'== Packages configuration : Check & print summary =='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

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
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'Re-compile with pkg "mom_vecinv" in packages.conf'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif
#ifndef ALLOW_MOM_FLUXFORM
      IF ( momStepping .AND. .NOT.vectorInvariantMomentum ) THEN
        WRITE(msgBuf,'(2A)')
     &   'PACKAGES_CHECK: cannot step forward Momentum',
     &   ' without pkg/mom_fluxform'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &   'Re-compile with pkg "mom_fluxform" in packages.conf'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif

#ifndef ALLOW_GENERIC_ADVDIFF
      IF ( tempStepping .OR. saltStepping ) THEN
        WRITE(msgBuf,'(2A)')
     &  'PACKAGES_CHECK: cannot step forward Temp or Salt',
     &  ' without pkg/generic_advdiff'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &  'Re-compile with pkg "generic_advdiff" in packages.conf'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF
#endif

#ifndef ALLOW_MONITOR
C     If monitorFreq is finite, make sure the pkg/monitor is being compiled
      IF (monitorFreq.NE.0.) CALL PACKAGES_ERROR_MSG(
     &                  'MONITOR', 'monitorFreq <> 0', myThid )
#endif

#ifndef ALLOW_TIMEAVE
C     If taveFreq is finite, make sure the pkg/timeave is being compiled
      IF (taveFreq.NE.0.) CALL PACKAGES_ERROR_MSG(
     &                  'TIMEAVE', 'taveFreq <> 0', myThid )
#endif

#ifndef ALLOW_CD_CODE
      IF (useCDscheme) CALL PACKAGES_ERROR_MSG(
     &                  'CD_CODE', 'useCDscheme=.T.', myThid )
#endif

C---  Continue with standard packages (with standard usePKG flag)

#ifdef ALLOW_RUNCLOCK
      IF (useRunClock) CALL RUNCLOCK_CHECK( myThid )
#else
      IF (useRunClock) CALL PACKAGES_ERROR_MSG('RunClock',' ',myThid)
#endif

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CHECK( myThid )
#else
      IF (useKPP) CALL PACKAGES_ERROR_MSG('KPP',' ',myThid)
#endif

#ifdef ALLOW_PP81
      IF (usePP81) CALL PP81_CHECK( myThid )
#else
      IF (usePP81) CALL PACKAGES_ERROR_MSG('PP81',' ',myThid)
#endif

#ifdef ALLOW_MY82
      IF (useMY82) CALL MY82_CHECK( myThid )
#else
      IF (useMY82) CALL PACKAGES_ERROR_MSG('MY82',' ',myThid)
#endif

#ifdef ALLOW_GGL90
      IF (useGGL90) CALL GGL90_CHECK( myThid )
#else
      IF (useGGL90) CALL PACKAGES_ERROR_MSG('GGL90',' ',myThid)
#endif

#ifdef ALLOW_OPPS
      IF (useOPPS) CALL OPPS_CHECK( myThid )
#else
      IF (useOPPS) CALL PACKAGES_ERROR_MSG('OPPS',' ',myThid)
#endif

#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_CHECK( myThid )
#else
      IF (useGMRedi) CALL PACKAGES_ERROR_MSG('GMRedi',' ',myThid)
#endif

#ifndef ALLOW_DOWN_SLOPE
      IF (useDOWN_SLOPE)
     &            CALL PACKAGES_ERROR_MSG('DOWN_SLOPE',' ',myThid)
#endif

#ifdef ALLOW_BBL
      IF (useBBL) CALL BBL_CHECK( myThid )
#else
      IF (useBBL) CALL PACKAGES_ERROR_MSG('BBL',' ',myThid)
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) CALL OBCS_CHECK( myThid )
#else
      IF (useOBCS) CALL PACKAGES_ERROR_MSG('OBCS',' ',myThid)
#endif

#ifdef ALLOW_EXF
      IF (useEXF) CALL EXF_CHECK( myThid )
#else
      IF (useEXF) CALL PACKAGES_ERROR_MSG('EXF',' ',myThid)
#endif

#ifndef ALLOW_BULK_FORCE
      IF (useBulkForce) CALL PACKAGES_ERROR_MSG(
     &                  'BULK_FORCE', 'useBulkForce=.T.', myThid )
#endif

#ifndef ALLOW_EBM
      IF (useEBM) CALL PACKAGES_ERROR_MSG('EBM',' ',myThid)
#endif

#ifndef ALLOW_CHEAPAML
      IF (useCheapAML) CALL PACKAGES_ERROR_MSG('CheapAML',' ',myThid)
#endif

#ifdef ALLOW_THSICE
      IF (useThSIce) CALL THSICE_CHECK( myThid )
#else
      IF (useThSIce) CALL PACKAGES_ERROR_MSG('ThSIce',' ',myThid)
#endif

#ifndef ALLOW_ATM2D
      IF (useATM2D) CALL PACKAGES_ERROR_MSG('ATM2D',' ',myThid)
#endif

#ifndef ALLOW_AIM
      IF (useAIM) CALL PACKAGES_ERROR_MSG('AIM',' ',myThid)
#endif

#ifndef ALLOW_LAND
      IF (useLand) CALL PACKAGES_ERROR_MSG('Land',' ',myThid)
#endif

#ifndef ALLOW_FIZHI
      IF (useFizhi) CALL PACKAGES_ERROR_MSG('Fizhi',' ',myThid)
#endif

#ifndef ALLOW_GRIDALT
      IF (useGridAlt) CALL PACKAGES_ERROR_MSG('GridAlt',' ',myThid)
#endif

#ifndef ALLOW_PTRACERS
      IF (usePTRACERS) CALL PACKAGES_ERROR_MSG('PTRACERS',' ',myThid)
#endif

#ifdef ALLOW_GCHEM
      IF (useGCHEM) CALL GCHEM_CHECK( myThid )
#else
      IF (useGCHEM) CALL PACKAGES_ERROR_MSG('GCHEM',' ',myThid)
#endif

#ifndef ALLOW_RBCS
      IF (useRBCS) CALL PACKAGES_ERROR_MSG('RBCS',' ',myThid)
#endif

#ifndef ALLOW_OFFLINE
      IF (useOffLine) CALL PACKAGES_ERROR_MSG('OffLine',' ',myThid)
#endif

#ifndef ALLOW_MATRIX
      IF (useMATRIX) CALL PACKAGES_ERROR_MSG('MATRIX',' ',myThid)
#endif

#ifndef ALLOW_SHAP_FILT
      IF (useSHAP_FILT)
     &   CALL PACKAGES_ERROR_MSG( 'SHAP_FILT', ' ', myThid )
#endif

#ifndef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT)
     &   CALL PACKAGES_ERROR_MSG( 'ZONAL_FILT', ' ', myThid )
#endif

#ifndef ALLOW_FLT
      IF (useFLT) CALL PACKAGES_ERROR_MSG('FLT',' ',myThid)
#endif

#ifdef ALLOW_SBO
      IF (useSBO) CALL SBO_CHECK( myThid )
#else
      IF (useSBO) CALL PACKAGES_ERROR_MSG('SBO',' ',myThid)
#endif

#ifdef ALLOW_SEAICE
      IF (useSEAICE) CALL SEAICE_CHECK( myThid )
#else
      IF (useSEAICE) CALL PACKAGES_ERROR_MSG('SEAICE',' ',myThid)
#endif

#ifdef ALLOW_SALT_PLUME
      IF (useSALT_PLUME)CALL SALT_PLUME_CHECK( myThid )
#else
      IF (useSALT_PLUME)CALL PACKAGES_ERROR_MSG('SALT_PLUME',' ',myThid)
#endif

#ifdef ALLOW_SHELFICE
      IF (useShelfIce) CALL SHELFICE_CHECK( myThid )
#else
      IF (useShelfIce) CALL PACKAGES_ERROR_MSG('ShelfIce',' ',myThid)
#endif

#ifdef ALLOW_ICEFRONT
      IF (useICEFRONT) CALL ICEFRONT_CHECK( myThid )
#else
      IF (useICEFRONT) CALL PACKAGES_ERROR_MSG('ICEFRONT',' ',myThid)
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

#ifndef ALLOW_SMOOTH
      IF (useSMOOTH) CALL PACKAGES_ERROR_MSG('SMOOTH',' ',myThid)
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) CALL DIAGNOSTICS_CHECK( myThid )
#else
      IF (useDiagnostics)
     &   CALL PACKAGES_ERROR_MSG( 'Diagnostics', ' ', myThid )
#endif

#ifdef ALLOW_REGRID
      IF (useREGRID) CALL REGRID_CHECK( myThid )
#else
      IF (useREGRID) CALL PACKAGES_ERROR_MSG('REGRID',' ',myThid)
#endif

#ifdef ALLOW_LAYERS
      IF ( useLayers ) CALL LAYERS_CHECK( myThid )
#else
      IF ( useLayers ) CALL PACKAGES_ERROR_MSG('LAYERS',' ',myThid)
#endif /* ALLOW_LAYERS */

#ifdef ALLOW_NEST_CHILD
      IF (useNEST_CHILD) CALL NEST_CHILD_CHECK( myThid )
#else
      IF (useNEST_CHILD) CALL PACKAGES_ERROR_MSG(
     & 'NEST_CHILD',' ',myThid)
#endif

#ifdef ALLOW_NEST_PARENT
      IF (useNEST_PARENT) CALL NEST_PARENT_CHECK( myThid )
#else
      IF (useNEST_PARENT) CALL PACKAGES_ERROR_MSG(
     & 'NEST_PARENT',' ',myThid)
#endif

#ifdef ALLOW_OASIS
      IF (useOASIS) CALL OASIS_CHECK( myThid )
#else
      IF (useOASIS) CALL PACKAGES_ERROR_MSG('OASIS',' ',myThid)
#endif

#ifdef ALLOW_ECCO
      CALL ECCO_CHECK( myThid )
#endif

#ifndef ALLOW_EMBED_FILES
      IF (useEMBED_FILES) CALL PACKAGES_ERROR_MSG(
     &                                  'EMBED_FILES',' ',myThid)
#endif

#ifdef ALLOW_MYPACKAGE
      IF (useMYPACKAGE) CALL MYPACKAGE_CHECK( myThid )
#else
      IF (useMYPACKAGE) CALL PACKAGES_ERROR_MSG('MYPACKAGE',' ',myThid)
#endif

#ifdef ALLOW_GENERIC_ADVDIFF
C-    Check generic AdvDiff setting and related overlap minimum size:
C     for this reason, called after other ${pkg}_check S/R
      IF (useGAD) CALL GAD_CHECK( myThid )
#endif

C---  Exclusive packages (which cannot be used together):
      IF ( useEXF .AND. useBulkForce ) THEN
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &  'both useEXF and useBulkForce are set'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &  ' but cannot be used together => need to select only one.'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF

C---  Exclusive packages (which cannot be used together):
      IF ( useDOWN_SLOPE .AND. useBBL ) THEN
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &  'both useDOWN_SLOPE and useBBL are set'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PACKAGES_CHECK: ',
     &  ' but cannot be used together => need to select only one.'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R PACKAGES_CHECK'
      ENDIF

      RETURN
      END
