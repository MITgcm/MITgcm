C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_readparms.F,v 1.8 2006/05/25 18:03:24 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_READPARMS
C     !INTERFACE:
      SUBROUTINE THSICE_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_READPARMS
C     | o Routine to initialize THSICE parameters and constants
C     *==========================================================*
C     | Initialize Th-Sea-ICE parameters, read in data.ice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     === Local variables ===
C     msgBuf      - Informational/error message buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   Th-Sea-ICE parameter
      NAMELIST /THSICE_CONST/
     &  rhos, rhoi, rhosw, rhofw,
     &  cpice, cpwater,
     &  kice, ksnow,
     &  transcoef, Lfresh, qsnow,
     &  albColdSnow, albWarmSnow, albOldSnow,
     &  hNewSnowAge, snowAgTime,
     &  albIceMax, albIceMin, hAlbIce, hAlbSnow,
     &  i0, ksolar,
     &  saltice, S_winton, mu_Tf,
     &  Tf0kel,
     &  himin, Terrmax, nitMaxTsf, hiMax, hsMax,
     &  iceMaskmax, iceMaskmin, himin0,
     &  frac_energy, hihig

      NAMELIST /THSICE_PARM01/
     &     startIceModel, stepFwd_oceMxL,
     &     thSIce_deltaT, ocean_deltaT, tauRelax_MxL,
     &     hMxL_default, sMxL_default, vMxL_default,
     &     stressReduction,
     &     thSIce_taveFreq, thSIce_diagFreq, thSIce_monFreq,
     &     thSIce_tave_mnc, thSIce_snapshot_mnc, thSIce_mon_mnc,
     &     thSIce_pickup_read_mnc, thSIce_pickup_write_mnc,
     &     thSIceFract_InitFile, thSIceThick_InitFile,
     &     thSIceSnowH_InitFile, thSIceSnowA_InitFile,
     &     thSIceEnthp_InitFile, thSIceTsurf_InitFile

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' THSICE_READPARMS: opening data.ice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.ice', 'THSICE_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values (constants)
      rhos     = 330. _d 0
      rhoi     = 900. _d 0
      rhosw    = rhoConst
      rhofw    = rhoConstFresh
      cpice    = 2106. _d 0
      cpwater  = HeatCapacity_Cp
      kice     = 2.03 _d 0
      ksnow    = 0.30 _d 0
      transcoef=0.006 _d 0
      Lfresh   = 3.34 _d 5
      qsnow    = Lfresh
      albColdSnow= 0.85 _d 0
      albWarmSnow= 0.70 _d 0
      albOldSnow = 0.55 _d 0
      albIceMax  = 0.65 _d 0
      albIceMin  = 0.20 _d 0
      hAlbIce    = 0.50 _d 0
      hAlbSnow   = 0.30 _d 0
      hNewSnowAge= 2. _d -3
      snowAgTime = 50. _d 0 * 86400. _d 0
      i0       = 0.3 _d 0
      ksolar   = 1.5 _d 0
      saltice  = 4. _d 0
      S_winton = 1. _d 0
      mu_Tf    = 0.054 _d 0
      Tf0kel   = celsius2K
      himin    = 0.01 _d 0
      Terrmax  = 5.0 _d -1
      nitMaxTsf= 20
      hiMax      = 10. _d 0
      hsMax      = 10. _d 0
      iceMaskmax = 1.  _d 0
      iceMaskmin =  .1 _d 0
      himin0     = 0.2 _d 0
      frac_energy=  .4 _d 0
      hihig      = 2.5 _d 0

C--   Default values (parameters)
      stepFwd_oceMxL  = .FALSE.
      startIceModel   = 0
      thSIce_deltaT   = dTtracerLev(1)
      ocean_deltaT    = dTtracerLev(1)
      tauRelax_MxL    = 0. _d 0
      hMxL_default    = 50. _d 0
      sMxL_default    = 35. _d 0
      vMxL_default    = 5. _d -2
      stressReduction = 1. _d 0
      thSIce_taveFreq = taveFreq
      thSIce_diagFreq = dumpFreq
      thSIce_monFreq  = monitorFreq
#ifdef ALLOW_MNC
      thSIce_tave_mnc     = timeave_mnc
      thSIce_snapshot_mnc = snapshot_mnc
      thSIce_mon_mnc      = monitor_mnc
      thSIce_pickup_read_mnc  = pickup_read_mnc
      thSIce_pickup_write_mnc = pickup_write_mnc
#else
      thSIce_tave_mnc     = .FALSE.
      thSIce_snapshot_mnc = .FALSE.
      thSIce_mon_mnc      = .FALSE.
      thSIce_pickup_read_mnc  = .FALSE.
      thSIce_pickup_write_mnc = .FALSE.
#endif
      thSIceFract_InitFile = ' '
      thSIceThick_InitFile = ' '
      thSIceSnowH_InitFile = ' '
      thSIceSnowA_InitFile = ' '
      thSIceEnthp_InitFile = ' '
      thSIceTsurf_InitFile = ' '


C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=THSICE_CONST)
      WRITE(msgBuf,'(A)') ' THSICE_READPARMS: read THSICE_CONST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      READ(UNIT=iUnit,NML=THSICE_PARM01)
      WRITE(msgBuf,'(A)') ' THSICE_READPARMS: read THSICE_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)

C-    Define other constants (from previous ones):
      Tmlt1=-mu_Tf*S_winton
      rhoiw = rhosw - rhoi

C     Set I/O parameters
      thSIce_tave_mdsio     = .TRUE.
      thSIce_snapshot_mdsio = .TRUE.
      thSIce_mon_stdio      = .TRUE.
      thSIce_pickup_write_mdsio = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. thSIce_tave_mnc ) thSIce_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. thSIce_snapshot_mnc )
     &       thSIce_snapshot_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. thSIce_mon_mnc  ) thSIce_mon_stdio  = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. thSIce_pickup_write_mnc  )
     &       thSIce_pickup_write_mdsio = .FALSE.
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      iUnit = standardMessageUnit
c     CALL MDSFINDUNIT( iUnit, mythid )
c     OPEN(iUnit,file='thsice_check_params',status='unknown')
      WRITE(iUnit,*) 'ThSI: rhos    =',rhos
      WRITE(iUnit,*) 'ThSI: rhoi    =',rhoi
      WRITE(iUnit,*) 'ThSI: rhosw   =',rhosw
      WRITE(iUnit,*) 'ThSI: rhofw   =',rhofw
      WRITE(iUnit,*) 'ThSI: rhoiw   =',rhoiw
      WRITE(iUnit,*) 'ThSI: cpice   =',cpice
      WRITE(iUnit,*) 'ThSI: cpwater =',cpwater
      WRITE(iUnit,*) 'ThSI: kice    =',kice
      WRITE(iUnit,*) 'ThSI: ksnow   =',ksnow
      WRITE(iUnit,*) 'ThSI: transcoef=',transcoef
      WRITE(iUnit,*) 'ThSI: Lfresh  =',Lfresh
      WRITE(iUnit,*) 'ThSI: qsnow   =',qsnow
      WRITE(iUnit,*) 'ThSI: albColdSnow=',albColdSnow
      WRITE(iUnit,*) 'ThSI: albWarmSnow=',albWarmSnow
      WRITE(iUnit,*) 'ThSI: albOldSnow =',albOldSnow
      WRITE(iUnit,*) 'ThSI: albIceMax =',albIceMax
      WRITE(iUnit,*) 'ThSI: albIceMin =',albIceMin
      WRITE(iUnit,*) 'ThSI: hAlbIce   =',hAlbIce
      WRITE(iUnit,*) 'ThSI: hAlbSnow  =',hAlbSnow
      WRITE(iUnit,*) 'ThSI: hNewSnowAge=',hNewSnowAge
      WRITE(iUnit,*) 'ThSI: snowAgTime =',snowAgTime
      WRITE(iUnit,*) 'ThSI: i0      =',i0
      WRITE(iUnit,*) 'ThSI: ksolar  =',ksolar
      WRITE(iUnit,*) 'ThSI: saltice =',saltice
      WRITE(iUnit,*) 'ThSI: S_winton=',S_winton
      WRITE(iUnit,*) 'ThSI: mu_Tf   =',mu_Tf
      WRITE(iUnit,*) 'ThSI: Tf0kel  =',Tf0kel
      WRITE(iUnit,*) 'ThSI: Tmlt1   =',Tmlt1
      WRITE(iUnit,*) 'ThSI: himin   =',himin
      WRITE(iUnit,*) 'ThSI: Terrmax =',Terrmax
      WRITE(iUnit,*) 'ThSI: nitMaxTsf=',nitMaxTsf
      WRITE(iUnit,*) 'ThSI: hiMax   =',hiMax
      WRITE(iUnit,*) 'ThSI: hsMax   =',hsMax
      WRITE(iUnit,*) 'ThSI: iceMaskmax=',iceMaskmax
      WRITE(iUnit,*) 'ThSI: iceMaskmin=',iceMaskmin
      WRITE(iUnit,*) 'ThSI: himin0  =',himin0
      WRITE(iUnit,*) 'ThSI: frac_energy',frac_energy
      WRITE(iUnit,*) 'ThSI: hihig   =',hihig
      WRITE(iUnit,*) 'ThSI: stressReduction =',stressReduction
      WRITE(iUnit,*) 'ThSI: thSIce_deltaT =',thSIce_deltaT
      WRITE(iUnit,*) 'ThSI: ocean_deltaT  =',ocean_deltaT
      WRITE(iUnit,*) 'ThSI: stepFwd_oceMxL=',stepFwd_oceMxL
      WRITE(iUnit,*) 'ThSI: tauRelax_MxL  =',tauRelax_MxL
      WRITE(iUnit,*) 'ThSI: hMxL_default  =',hMxL_default
      WRITE(iUnit,*) 'ThSI: sMxL_default  =',sMxL_default
      WRITE(iUnit,*) 'ThSI: vMxL_default  =',vMxL_default
      WRITE(iUnit,*) 'ThSI: thSIce_taveFreq=',thSIce_taveFreq
      WRITE(iUnit,*) 'ThSI: thSIce_diagFreq=',thSIce_diagFreq
      WRITE(iUnit,*) 'ThSI: thSIce_monFreq =',thSIce_monFreq
      WRITE(iUnit,*) 'ThSI: startIceModel =',startIceModel
      IF (iUnit.NE.standardMessageUnit) CLOSE(iUnit)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_THSICE */

      RETURN
      END
