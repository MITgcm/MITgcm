C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_readparms.F,v 1.22 2014/05/27 23:41:27 jmc Exp $
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
#ifdef ALLOW_COST
#include "THSICE_COST.h"
#endif
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     iUnit     :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(20) fmtR, fmtI, fmtL, fmtC
      INTEGER iUnit

C--   Th-Sea-ICE parameter
      NAMELIST /THSICE_CONST/
     &  rhos, rhoi, rhosw, rhofw,
     &  cpIce, cpWater,
     &  kIce, kSnow,
     &  bMeltCoef, Lfresh, qsnow,
     &  albColdSnow, albWarmSnow, tempSnowAlb,
     &  albOldSnow, hNewSnowAge, snowAgTime,
     &  albIceMax, albIceMin, hAlbIce, hAlbSnow,
     &  i0swFrac, ksolar, dhSnowLin,
     &  saltIce, S_winton, mu_Tf,
     &  Tf0kel, Terrmax, nitMaxTsf,
     &  hIceMin, hiMax, hsMax, iceMaskMax, iceMaskMin,
     &  fracEnMelt, fracEnFreez, hThinIce, hThickIce, hNewIceMax

      NAMELIST /THSICE_PARM01/
     &     startIceModel, stepFwd_oceMxL, thSIce_calc_albNIR,
     &     thSIce_skipThermo, thSIce_deltaT, thSIce_dtTemp,
     &     ocean_deltaT, tauRelax_MxL, tauRelax_MxL_salt,
     &     hMxL_default, sMxL_default, vMxL_default,
     &     thSIce_diffK, thSIceAdvScheme, stressReduction,
     &     thSIceBalanceAtmFW,
     &     thSIce_taveFreq, thSIce_diagFreq, thSIce_monFreq,
     &     thSIce_tave_mnc, thSIce_snapshot_mnc, thSIce_mon_mnc,
     &     thSIce_pickup_read_mnc, thSIce_pickup_write_mnc,
     &     thSIceFract_InitFile, thSIceThick_InitFile,
     &     thSIceSnowH_InitFile, thSIceSnowA_InitFile,
     &     thSIceEnthp_InitFile, thSIceTsurf_InitFile

#ifdef ALLOW_COST
      NAMELIST /THSICE_COST/
     &     mult_thsice, thsice_cost_ice_flag
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useThSIce ) THEN
C-    pkg THSICE is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ice is found
         CALL PACKAGES_UNUSED_MSG( 'useThSIce', ' ', 'ice' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

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
      cpIce    = 2106. _d 0
      cpWater  = HeatCapacity_Cp
      kIce     = 2.03 _d 0
      kSnow    = 0.30 _d 0
      bMeltCoef=0.006 _d 0
      Lfresh   = 3.34 _d 5
      qsnow    = Lfresh
      albColdSnow= 0.85 _d 0
      albWarmSnow= 0.70 _d 0
      tempSnowAlb= -10. _d 0
      albOldSnow = 0.55 _d 0
      albIceMax  = 0.65 _d 0
      albIceMin  = 0.20 _d 0
      hAlbIce    = 0.50 _d 0
      hAlbSnow   = 0.30 _d 0
      hNewSnowAge= 2. _d -3
      snowAgTime = 50. _d 0 * 86400. _d 0
      i0swFrac = 0.3 _d 0
      ksolar   = 1.5 _d 0
      dhSnowLin= 0. _d 0
      saltIce  = 4. _d 0
      S_winton = 1. _d 0
      mu_Tf    = 0.054 _d 0
      Tf0kel   = celsius2K
      Terrmax  = 5.0 _d -1
      nitMaxTsf= 20
      hIceMin    = 1. _d -2
      hiMax      = 10. _d 0
      hsMax      = 10. _d 0
      iceMaskMax =  1. _d 0
      iceMaskMin = 0.1 _d 0
      fracEnMelt = 0.4 _d 0
      fracEnFreez=  0. _d 0
      hThinIce   = 0.2 _d 0
      hThickIce  = 2.5 _d 0
      hNewIceMax = UNSET_RL

C--   Default values (parameters)
      stepFwd_oceMxL  = .FALSE.
      thSIce_skipThermo  = .FALSE.
      thSIce_calc_albNIR = .FALSE.
      startIceModel   = 0
      thSIce_deltaT   = dTtracerLev(1)
      thSIce_dtTemp   = UNSET_RL
      ocean_deltaT    = dTtracerLev(1)
      tauRelax_MxL    = 0. _d 0
      tauRelax_MxL_salt = UNSET_RL
      hMxL_default    = 50. _d 0
      sMxL_default    = 35. _d 0
      vMxL_default    = 5. _d -2
      thSIce_diffK    = 0. _d 0
      thSIceAdvScheme = 0
      stressReduction = 1. _d 0
      IF ( useSEAICE ) stressReduction = 0. _d 0
      thSIceBalanceAtmFW  = 0
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

#ifdef ALLOW_COST
      thsice_cost_ice_flag = 1
      mult_thsice = 0. _d 0
#endif

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=THSICE_CONST)
      WRITE(msgBuf,'(A)') ' THSICE_READPARMS: read THSICE_CONST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      READ(UNIT=iUnit,NML=THSICE_PARM01)
      WRITE(msgBuf,'(A)') ' THSICE_READPARMS: read THSICE_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#ifdef ALLOW_COST
      READ(UNIT=iUnit,NML=THSICE_COST)
      WRITE(msgBuf,'(A)') ' THSICE_READPARMS: read THSICE_COST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
#endif

C--   Close the open data file
      CLOSE(iUnit)

C-    neutral default:
      IF ( hNewIceMax .EQ. UNSET_RL ) hNewIceMax = hiMax

C     If using the same time step for both icetop temp solver
C     and ice thickness/growth, use thSIce_deltaT value
      IF ( thSIce_dtTemp .EQ. UNSET_RL ) thSIce_dtTemp=thSIce_deltaT

C-    If undef, set salt relax to temperature relax
      IF ( tauRelax_MxL_salt .EQ. UNSET_RL ) THEN
           tauRelax_MxL_salt = tauRelax_MxL
      ENDIF

C-    Define other constants (from previous ones):
      Tmlt1=-mu_Tf*S_winton
      floodFac = (rhosw - rhoi)/rhos

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
C--   Check/reset parameters:
      IF ( useSEAICE .AND. stressReduction.NE.0. _d 0 ) THEN
C--     If useSEAICE=.true., the stress is computed in seaice_model,
C--     so that it does not need any further reduction
        WRITE(msgBuf,'(2A)') '** WARNING ** THSICE_READPARMS:',
     &                       ' reset stressReduction to zero'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') 'THSICE_READPARMS: useSEAICE=T =>',
     &       ' stress is be computed by SEAICE pkg => no reduction'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        stressReduction = 0. _d 0
      ENDIF
      IF ( fluidIsAir .AND. thSIceBalanceAtmFW.NE.0 ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** THSICE_READPARMS:',
     &                       ' reset thSIceBalanceAtmFW to zero'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') 'THSICE_READPARMS:',
     &       ' since it is not available in Atmospheric set-up'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        thSIceBalanceAtmFW = 0
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      iUnit = standardMessageUnit
      fmtR = '(A6,A20,1PE21.13)'
      fmtI = '(A6,A20,I10)'
      fmtL = '(A6,A20,L10)'
      fmtC = '(A26,1X,A)'
c     CALL MDSFINDUNIT( iUnit, myThid )
c     OPEN(iUnit,file='thsice_check_params',status='unknown')
      WRITE(iUnit,fmtR) 'ThSI:', 'rhos        =', rhos
      WRITE(iUnit,fmtR) 'ThSI:', 'rhoi        =', rhoi
      WRITE(iUnit,fmtR) 'ThSI:', 'rhosw       =', rhosw
      WRITE(iUnit,fmtR) 'ThSI:', 'rhofw       =', rhofw
      WRITE(iUnit,fmtR) 'ThSI:', 'floodFac    =', floodFac
      WRITE(iUnit,fmtR) 'ThSI:', 'cpIce       =', cpIce
      WRITE(iUnit,fmtR) 'ThSI:', 'cpWater     =', cpWater
      WRITE(iUnit,fmtR) 'ThSI:', 'kIce        =', kIce
      WRITE(iUnit,fmtR) 'ThSI:', 'kSnow       =', kSnow
      WRITE(iUnit,fmtR) 'ThSI:', 'bMeltCoef   =', bMeltCoef
      WRITE(iUnit,fmtR) 'ThSI:', 'Lfresh      =', Lfresh
      WRITE(iUnit,fmtR) 'ThSI:', 'qsnow       =', qsnow
      WRITE(iUnit,fmtR) 'ThSI:', 'albColdSnow =', albColdSnow
      WRITE(iUnit,fmtR) 'ThSI:', 'albWarmSnow =', albWarmSnow
      WRITE(iUnit,fmtR) 'ThSI:', 'tempSnowAlb =', tempSnowAlb
      WRITE(iUnit,fmtR) 'ThSI:', 'albOldSnow  =', albOldSnow
      WRITE(iUnit,fmtR) 'ThSI:', 'hNewSnowAge =', hNewSnowAge
      WRITE(iUnit,fmtR) 'ThSI:', 'snowAgTime  =', snowAgTime
      WRITE(iUnit,fmtR) 'ThSI:', 'albIceMax   =', albIceMax
      WRITE(iUnit,fmtR) 'ThSI:', 'albIceMin   =', albIceMin
      WRITE(iUnit,fmtR) 'ThSI:', 'hAlbIce     =', hAlbIce
      WRITE(iUnit,fmtR) 'ThSI:', 'hAlbSnow    =', hAlbSnow
      WRITE(iUnit,fmtR) 'ThSI:', 'i0swFrac    =', i0swFrac
      WRITE(iUnit,fmtR) 'ThSI:', 'ksolar      =', ksolar
      WRITE(iUnit,fmtR) 'ThSI:', 'dhSnowLin   =', dhSnowLin
      WRITE(iUnit,fmtR) 'ThSI:', 'saltIce     =', saltIce
      WRITE(iUnit,fmtR) 'ThSI:', 'S_winton    =', S_winton
      WRITE(iUnit,fmtR) 'ThSI:', 'mu_Tf       =', mu_Tf
      WRITE(iUnit,fmtR) 'ThSI:', 'Tf0kel      =', Tf0kel
      WRITE(iUnit,fmtR) 'ThSI:', 'Tmlt1       =', Tmlt1
      WRITE(iUnit,fmtR) 'ThSI:', 'Terrmax     =', Terrmax
      WRITE(iUnit,fmtI) 'ThSI:', 'nitMaxTsf   =', nitMaxTsf
      WRITE(iUnit,fmtR) 'ThSI:', 'hIceMin     =', hIceMin
      WRITE(iUnit,fmtR) 'ThSI:', 'hiMax       =', hiMax
      WRITE(iUnit,fmtR) 'ThSI:', 'hsMax       =', hsMax
      WRITE(iUnit,fmtR) 'ThSI:', 'iceMaskMax  =', iceMaskMax
      WRITE(iUnit,fmtR) 'ThSI:', 'iceMaskMin  =', iceMaskMin
      WRITE(iUnit,fmtR) 'ThSI:', 'fracEnMelt  =', fracEnMelt
      WRITE(iUnit,fmtR) 'ThSI:', 'fracEnFreez =', fracEnFreez
      WRITE(iUnit,fmtR) 'ThSI:', 'hThinIce    =', hThinIce
      WRITE(iUnit,fmtR) 'ThSI:', 'hThickIce   =', hThickIce
      WRITE(iUnit,fmtR) 'ThSI:', 'hNewIceMax  =', hNewIceMax
      WRITE(iUnit,fmtR) 'ThSI:','stressReduction   =',stressReduction
      WRITE(iUnit,fmtL) 'ThSI:','thSIce_skipThermo =',thSIce_skipThermo
      WRITE(iUnit,fmtI) 'ThSI:','thSIceAdvScheme   =',thSIceAdvScheme
      WRITE(iUnit,fmtI) 'ThSI:','thSIceBalanceAtmFW=',thSIceBalanceAtmFW
      WRITE(iUnit,fmtR) 'ThSI:','thSIce_diffK      =',thSIce_diffK
      WRITE(iUnit,fmtR) 'ThSI:','thSIce_deltaT     =',thSIce_deltaT
      WRITE(iUnit,fmtR) 'ThSI:','ocean_deltaT      =',ocean_deltaT
      WRITE(iUnit,fmtL) 'ThSI:','stepFwd_oceMxL    =',stepFwd_oceMxL
      WRITE(iUnit,fmtR) 'ThSI:','tauRelax_MxL      =',tauRelax_MxL
      WRITE(iUnit,fmtR) 'ThSI:','tauRelax_MxL_salt =',tauRelax_MxL_salt
      WRITE(iUnit,fmtR) 'ThSI:','hMxL_default      =',hMxL_default
      WRITE(iUnit,fmtR) 'ThSI:','sMxL_default      =',sMxL_default
      WRITE(iUnit,fmtR) 'ThSI:','vMxL_default      =',vMxL_default
      WRITE(iUnit,fmtR) 'ThSI:','thSIce_taveFreq   =',thSIce_taveFreq
      WRITE(iUnit,fmtR) 'ThSI:','thSIce_diagFreq   =',thSIce_diagFreq
      WRITE(iUnit,fmtR) 'ThSI:','thSIce_monFreq    =',thSIce_monFreq
      WRITE(iUnit,fmtI) 'ThSI:','startIceModel     =',startIceModel
c     WRITE(iUnit,fmtC) 'thSIceFract_InitFile    =',thSIceFract_InitFile
      IF (iUnit.NE.standardMessageUnit) CLOSE(iUnit)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_THSICE */

      RETURN
      END
