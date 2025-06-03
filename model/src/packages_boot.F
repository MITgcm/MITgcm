#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_BOOT
C     !INTERFACE:
      SUBROUTINE PACKAGES_BOOT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_BOOT
C     | o Routine to parse runtime package selection file
C     *==========================================================*
C     | Routine reads a flat file which contains a single flag
C     | for each known "package". Flag can enables runtime
C     | activation or deactivation of a package. The data is read
C     | from a file called data.pkg.
C     | Note - If a runtime activation flag is set on but the
C     |        package code was not included in the build then
C     |        the default behavior is to stop with an error.
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
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
C     iUnit    :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      LOGICAL locFlag
CEOP

C     data.pkg namelists
      NAMELIST /PACKAGES/
     &          useOBCS,
     &          useSHAP_FILT,
     &          useZONAL_FILT,
     &          useOPPS,
     &          usePP81,
     &          useKL10,
     &          useMY82,
     &          useGGL90,
     &          useKPP,
     &          useGMRedi,
     &          useDOWN_SLOPE,
     &          useBBL,
     &          useCAL,
     &          useEXF,
     &          useBulkForce,
     &          useEBM,
     &          useCheapAML,
     &          useAUTODIFF,
     &          useGrdchk,
     &          useSMOOTH,
     &          usePROFILES,
     &          useOBSFIT,
     &          useECCO,
     &          useCTRL,
     &          useSBO,
     &          useFLT,
     &          usePTRACERS,
     &          useGCHEM,
     &          useRBCS,
     &          useOffLine,
     &          useMATRIX,
     &          useFRAZIL,
     &          useSEAICE,
     &          useSALT_PLUME,
     &          useShelfIce,
     &          useSTIC,
     &          useStreamIce,
     &          useICEFRONT,
     &          useThSIce,
     &          useLand,
     &          useATM2d,
     &          useAIM,
     &          useAtm_Phys,
     &          useFizhi,
     &          useGridAlt,
     &          useDiagnostics,
     &          useREGRID,
     &          useLayers,
     &          useMNC,
     &          useRunClock,
     &          useEMBED_FILES,
     &          useMYPACKAGE

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: opening data.pkg'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.pkg', 'PACKAGES_BOOT',
     O                          iUnit,
     I                          myThid )

C--   Default package configuration
c     useGAD          =.FALSE.
      useOBCS         =.FALSE.
      useSHAP_FILT    =.FALSE.
      useZONAL_FILT   =.FALSE.
      useOPPS         =.FALSE.
      usePP81         =.FALSE.
      useKL10         =.FALSE.
      useMY82         =.FALSE.
      useGGL90        =.FALSE.
      useKPP          =.FALSE.
      useGMRedi       =.FALSE.
      useDOWN_SLOPE   =.FALSE.
      useBBL          =.FALSE.
      useCAL          =.FALSE.
      useEXF          =.FALSE.
      useBulkForce    =.FALSE.
      useEBM          =.FALSE.
      useCheapAML     =.FALSE.
      useAUTODIFF     =.FALSE.
      useGrdchk       =.FALSE.
      useSMOOTH       =.FALSE.
      usePROFILES     =.FALSE.
      useOBSFIT       =.FALSE.
      useECCO         =.FALSE.
      useCTRL         =.FALSE.
      useSBO          =.FALSE.
      useFLT          =.FALSE.
      usePTRACERS     =.FALSE.
      useGCHEM        =.FALSE.
      useRBCS         =.FALSE.
      useOffLine      =.FALSE.
      useMATRIX       =.FALSE.
      useFRAZIL       =.FALSE.
      useSEAICE       =.FALSE.
      useSALT_PLUME   =.FALSE.
      useShelfIce     =.FALSE.
      useSTIC         =.FALSE.
      useStreamIce    =.FALSE.
      useICEFRONT     =.FALSE.
      useThSIce       =.FALSE.
      useLand         =.FALSE.
      useATM2d        =.FALSE.
      useAIM          =.FALSE.
      useAtm_Phys     =.FALSE.
      useFizhi        =.FALSE.
      useGridAlt      =.FALSE.
      useDiagnostics  =.FALSE.
      useREGRID       =.FALSE.
      useLayers       =.FALSE.
      useMNC          =.FALSE.
      useRunClock     =.FALSE.
      useEMBED_FILES  =.FALSE.
      useMYPACKAGE    =.FALSE.

C-    Adjust default to account for pkgs without run-time On/Off switch
#ifdef ALLOW_AUTODIFF
      useAUTODIFF     =.TRUE.
# ifdef ALLOW_ECCO
      useECCO         =.TRUE.
# endif
# ifdef ALLOW_CTRL
      useCTRL         =.TRUE.
# endif
#endif /* ALLOW_AUTODIFF */

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=PACKAGES)

      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: finished reading data.pkg'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C--   packages with hard-coded switch
#ifndef HAVE_NETCDF
      IF (usePROFILES) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** pkg/profiles uses NetCDF',
     &   ' which is unavailable (genmake2 test failed)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT:',
     &                       ' ==> Reset usePROFILES to FALSE'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') ' PACKAGES_BOOT: undef HAVE_NETCDF',
     &                       ' ==> Reset usePROFILES to FALSE'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        usePROFILES=.FALSE.
      ENDIF
      IF (useOBSFIT) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** pkg/obsfit uses NetCDF',
     &   ' which is unavailable (genmake2 test failed)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT:',
     &                       ' ==> Reset useOBSFIT to FALSE'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') ' PACKAGES_BOOT: undef HAVE_NETCDF',
     &                       ' ==> Reset useOBSFIT to FALSE'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        useOBSFIT=.FALSE.
      ENDIF
#endif

#ifdef ALLOW_CAL
      IF (usePROFILES) useCAL = .TRUE.
      IF (useOBSFIT) useCAL = .TRUE.
      IF (useECCO) useCAL = .TRUE.
#endif

#ifdef ALLOW_CTRL
      IF (useGrdchk) useCTRL = .TRUE.
#endif

C--   packages with an implied switch
      useGAD = tempStepping .OR. saltStepping .OR. usePTRACERS

#ifdef ALLOW_CAL
c     IF (useEXF) useCAL = .TRUE.
C--   Double warning (StdErr & StdOut) after changing useCAL setting
      IF ( useEXF .AND. .NOT. useCAL ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    'useCAL no longer set to T when using EXF (useEXF=T)'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    ' as it used to be before checkpoint66d (2017/02/13)'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    'To continue to use pkg/cal with EXF, need to add:'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** PACKAGES_BOOT: ',
     &    '> useCAL=.TRUE., < in file "data.pkg"'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif /* ALLOW_CAL */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Short summary of which package is compiled and used
      WRITE(msgBuf,'(2A)') ' PACKAGES_BOOT: On/Off package Summary'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C----- pkgs with a standard "usePKG" On/Off switch in "data.pkg":
      WRITE(standardMessageUnit,'(2A)') ' --------  pkgs with a',
     & ' standard "usePKG" On/Off switch in "data.pkg":  --------'
#ifdef ALLOW_OBCS
      CALL PACKAGES_PRINT_MSG( useOBCS,       'OBCS',        ' ' )
#endif
#ifdef ALLOW_SHAP_FILT
      CALL PACKAGES_PRINT_MSG( useSHAP_FILT,  'SHAP_FILT',   ' ' )
#endif
#ifdef ALLOW_ZONAL_FILT
      CALL PACKAGES_PRINT_MSG( useZONAL_FILT, 'ZONAL_FILT',  ' ' )
#endif
#ifdef ALLOW_OPPS
      CALL PACKAGES_PRINT_MSG( useOPPS,       'OPPS',        ' ' )
#endif
#ifdef ALLOW_PP81
      CALL PACKAGES_PRINT_MSG( usePP81,       'PP81',        ' ' )
#endif
#ifdef ALLOW_KL10
      CALL PACKAGES_PRINT_MSG( useKL10,       'KL10',        ' ' )
#endif
#ifdef ALLOW_MY82
      CALL PACKAGES_PRINT_MSG( useMY82,       'MY82',        ' ' )
#endif
#ifdef ALLOW_GGL90
      CALL PACKAGES_PRINT_MSG( useGGL90,      'GGL90',       ' ' )
#endif
#ifdef ALLOW_KPP
      CALL PACKAGES_PRINT_MSG( useKPP,        'KPP',         ' ' )
#endif
#ifdef ALLOW_GMREDI
      CALL PACKAGES_PRINT_MSG( useGMRedi,     'GMRedi',      ' ' )
#endif
#ifdef ALLOW_DOWN_SLOPE
      CALL PACKAGES_PRINT_MSG( useDOWN_SLOPE, 'DOWN_SLOPE',  ' ' )
#endif
#ifdef ALLOW_BBL
      CALL PACKAGES_PRINT_MSG( useBBL,        'BBL',         ' ' )
#endif
#ifdef ALLOW_CAL
      CALL PACKAGES_PRINT_MSG( useCAL,        'CAL',         ' ' )
#endif
#ifdef ALLOW_EXF
      CALL PACKAGES_PRINT_MSG( useEXF,        'EXF',         ' ' )
#endif
#ifdef ALLOW_BULK_FORCE
      CALL PACKAGES_PRINT_MSG(useBulkForce,'BULK_FORCE','useBulkForce')
#endif
#ifdef ALLOW_EBM
      CALL PACKAGES_PRINT_MSG( useEBM,        'EBM',         ' ' )
#endif
#ifdef ALLOW_CHEAPAML
      CALL PACKAGES_PRINT_MSG( useCheapAML,   'CheapAML',    ' ' )
#endif
#ifdef ALLOW_AUTODIFF
      CALL PACKAGES_PRINT_MSG( useAUTODIFF,   'AUTODIFF',    ' ' )
#endif
#ifdef ALLOW_GRDCHK
      CALL PACKAGES_PRINT_MSG( useGrdchk,     'Grdchk',      ' ' )
#endif
#ifdef ALLOW_SMOOTH
      CALL PACKAGES_PRINT_MSG( useSMOOTH,     'SMOOTH',      ' ' )
#endif
#ifdef ALLOW_PROFILES
      CALL PACKAGES_PRINT_MSG( usePROFILES,   'PROFILES',    ' ' )
#endif
#ifdef ALLOW_OBSFIT
      CALL PACKAGES_PRINT_MSG( useOBSFIT,     'OBSFIT',      ' ' )
#endif
#ifdef ALLOW_ECCO
      CALL PACKAGES_PRINT_MSG( useECCO,       'ECCO',        ' ' )
#endif
#ifdef ALLOW_CTRL
      CALL PACKAGES_PRINT_MSG( useCTRL,       'CTRL',        ' ' )
#endif
#ifdef ALLOW_SBO
      CALL PACKAGES_PRINT_MSG( useSBO,        'SBO',         ' ' )
#endif
#ifdef ALLOW_FLT
      CALL PACKAGES_PRINT_MSG( useFLT,        'FLT',         ' ' )
#endif
#ifdef ALLOW_PTRACERS
      CALL PACKAGES_PRINT_MSG( usePTRACERS,   'PTRACERS',    ' ' )
#endif
#ifdef ALLOW_LONGSTEP
      CALL PACKAGES_PRINT_MSG( usePTRACERS,'LONGSTEP','usePTRACERS' )
#endif
#ifdef ALLOW_GCHEM
      CALL PACKAGES_PRINT_MSG( useGCHEM,      'GCHEM',       ' ' )
#endif
#ifdef ALLOW_RBCS
      CALL PACKAGES_PRINT_MSG( useRBCS,       'RBCS',        ' ' )
#endif
#ifdef ALLOW_OFFLINE
      CALL PACKAGES_PRINT_MSG( useOffLine,    'OffLine',     ' ' )
#endif
#ifdef ALLOW_MATRIX
      CALL PACKAGES_PRINT_MSG( useMATRIX,     'MATRIX',      ' ' )
#endif
#ifdef ALLOW_FRAZIL
      CALL PACKAGES_PRINT_MSG( useFRAZIL,     'FRAZIL',      ' ' )
#endif
#ifdef ALLOW_SEAICE
      CALL PACKAGES_PRINT_MSG( useSEAICE,     'SEAICE',      ' ' )
#endif
#ifdef ALLOW_SALT_PLUME
      CALL PACKAGES_PRINT_MSG( useSALT_PLUME, 'SALT_PLUME',  ' ' )
#endif
#ifdef ALLOW_SHELFICE
      CALL PACKAGES_PRINT_MSG( useShelfIce,   'ShelfIce',    ' ' )
#endif
#ifdef ALLOW_STEEP_ICECAVITY
      CALL PACKAGES_PRINT_MSG( useSTIC,   'STEEP_ICECAVITY', ' ' )
#endif
#ifdef ALLOW_STREAMICE
      CALL PACKAGES_PRINT_MSG( useStreamIce,  'StreamIce',   ' ' )
#endif
#ifdef ALLOW_ICEFRONT
      CALL PACKAGES_PRINT_MSG( useICEFRONT,   'ICEFRONT',    ' ' )
#endif
#ifdef ALLOW_THSICE
      CALL PACKAGES_PRINT_MSG( useThSIce,     'ThSIce',      ' ' )
#endif
#ifdef ALLOW_LAND
      CALL PACKAGES_PRINT_MSG( useLand,       'Land',        ' ' )
#endif
#ifdef ALLOW_ATM2D
      CALL PACKAGES_PRINT_MSG( useATM2d,      'ATM2d',       ' ' )
#endif
#ifdef ALLOW_AIM_V23
      CALL PACKAGES_PRINT_MSG( useAIM,        'aim_v23', 'useAIM' )
#endif
#ifdef ALLOW_ATM_PHYS
      CALL PACKAGES_PRINT_MSG( useAtm_Phys,   'Atm_Phys',    ' ' )
#endif
#ifdef ALLOW_FIZHI
      CALL PACKAGES_PRINT_MSG( useFizhi,      'Fizhi',       ' ' )
#endif
#ifdef ALLOW_GRIDALT
      CALL PACKAGES_PRINT_MSG( useGridAlt,    'GridAlt',     ' ' )
#endif
#ifdef ALLOW_DIAGNOSTICS
      CALL PACKAGES_PRINT_MSG( useDiagnostics,'Diagnostics', ' ' )
#endif
#ifdef ALLOW_REGRID
      CALL PACKAGES_PRINT_MSG( useREGRID,     'REGRID',      ' ' )
#endif
#ifdef ALLOW_LAYERS
      CALL PACKAGES_PRINT_MSG( useLayers,     'Layers',      ' ' )
#endif
#ifdef ALLOW_MNC
      CALL PACKAGES_PRINT_MSG( useMNC,        'MNC',         ' ' )
#endif
#ifdef ALLOW_RUNCLOCK
      CALL PACKAGES_PRINT_MSG( useRunClock,   'RunClock',    ' ' )
#endif
#ifdef ALLOW_EMBED_FILES
      CALL PACKAGES_PRINT_MSG( useEMBED_FILES,'EMBED_FILES', ' ' )
#endif
#ifdef ALLOW_MYPACKAGE
      CALL PACKAGES_PRINT_MSG( useMYPACKAGE,  'MYPACKAGE',   ' ' )
#endif

C----- pkgs without standard "usePKG" in "data.pkg":
      WRITE(standardMessageUnit,'(2A)') ' -------- pkgs without',
     & ' standard "usePKG" On/Off switch in "data.pkg":  --------'
#ifdef ALLOW_GENERIC_ADVDIFF
      CALL PACKAGES_PRINT_MSG( useGAD, 'GENERIC_ADVDIFF', 'useGAD' )
#endif
#ifdef ALLOW_MOM_COMMON
      CALL PACKAGES_PRINT_MSG( momStepping,'MOM_COMMON','momStepping' )
#endif
#ifdef ALLOW_MOM_VECINV
      locFlag = momStepping .AND. vectorInvariantMomentum
      CALL PACKAGES_PRINT_MSG( locFlag, 'MOM_VECINV',
     &                                  '+vectorInvariantMomentum' )
#endif
#ifdef ALLOW_MOM_FLUXFORM
      locFlag = momStepping .AND. .NOT.vectorInvariantMomentum
      CALL PACKAGES_PRINT_MSG( locFlag, 'MOM_FLUXFORM',
     &                                  '& not vectorInvariantMom' )
#endif
#ifdef ALLOW_CD_CODE
      CALL PACKAGES_PRINT_MSG( useCDscheme, 'CD_CODE', 'useCDscheme' )
#endif
#ifdef ALLOW_MONITOR
      locFlag = monitorFreq.GT.0.
      CALL PACKAGES_PRINT_MSG( locFlag, 'MONITOR', 'monitorFreq > 0.' )
#endif
#ifdef ALLOW_TIMEAVE
      locFlag = taveFreq.GT.0.
      CALL PACKAGES_PRINT_MSG( locFlag,  'TIMEAVE', 'taveFreq > 0.' )
#endif
#ifdef ALLOW_DEBUG
      CALL PACKAGES_PRINT_MSG( debugMode, 'DEBUG', 'debugMode' )
#endif
#ifdef ALLOW_COMPON_COMMUNIC
      CALL PACKAGES_PRINT_MSG( useCoupler, 'COMPON_COMMUNIC',
     &                         'useCoupler' )
#endif
#ifdef ALLOW_ATM_COMPON_INTERF
      CALL PACKAGES_PRINT_MSG( useCoupler, 'ATM_COMPON_INTERF',
     &                         'useCoupler' )
#endif
#ifdef ALLOW_OCN_COMPON_INTERF
      CALL PACKAGES_PRINT_MSG( useCoupler, 'OCN_COMPON_INTERF',
     &                         'useCoupler' )
#endif
C----- pkgs without a run-time switch:
      locFlag = .TRUE.
#ifdef ALLOW_ATM_OCN_COUPLER
      CALL PACKAGES_PRINT_MSG( locFlag, 'ATM_OCN_COUPLER', 'Used' )
#endif
#ifdef ALLOW_EXCH2
      CALL PACKAGES_PRINT_MSG( locFlag, 'EXCH2',      'Used' )
#endif
#ifdef ALLOW_RW
      CALL PACKAGES_PRINT_MSG( locFlag, 'RW',        'Used' )
#endif
#ifdef ALLOW_MDSIO
      CALL PACKAGES_PRINT_MSG( locFlag, 'MDSIO',     'Used' )
#endif
#ifdef ALLOW_SHOWFLOPS
      CALL PACKAGES_PRINT_MSG( locFlag, 'SHOWFLOPS', 'Used' )
#endif
C----- AD related pkgs:
#ifdef ALLOW_ADMTLM
      CALL PACKAGES_PRINT_MSG( locFlag, 'ADMTLM',     'Used' )
#endif
#ifdef ALLOW_AUTODIFF
      CALL PACKAGES_PRINT_MSG( locFlag, 'AUTODIFF',   'Used' )
#endif
#ifdef ALLOW_OPENAD
      CALL PACKAGES_PRINT_MSG( locFlag, 'OPENAD',     'Used' )
#endif
#ifdef ALLOW_COST
      CALL PACKAGES_PRINT_MSG( locFlag, 'COST',       'Used' )
#endif
C----- mysterious pkgs:
#ifdef ALLOW_SPHERE
      CALL PACKAGES_PRINT_MSG( locFlag, 'SPHERE',     'None' )
#endif
#ifdef ALLOW_CHRONOS
      CALL PACKAGES_PRINT_MSG( locFlag, 'CHRONOS',    'None' )
#endif
#ifdef ALLOW_ATM_COMMON
      CALL PACKAGES_PRINT_MSG( locFlag, 'ATM_COMMON', 'None' )
#endif

      WRITE(msgBuf,'(2A)') ' PACKAGES_BOOT: End of package Summary'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
