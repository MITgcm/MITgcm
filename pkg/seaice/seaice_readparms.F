C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_readparms.F,v 1.28 2005/09/01 05:34:31 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_READPARMS                              |
C     | o Routine to read in file data.seaice                    |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_COST
# include "SEAICE_COST.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instance of SEAICE_READPARMS
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit

C--   SEAICE parameters
      NAMELIST /SEAICE_PARM01/
     & SEAICEwriteState, SEAICEuseDYNAMICS,
     & LAD, IMAX_TICE,
     & SEAICE_deltaTtherm, SEAICE_deltaTdyn,
     & SEAICE_dumpFreq, SEAICE_taveFreq, SEAICE_initialHEFF,
     & SEAICE_drag, SEAICE_waterDrag, SEAICE_dryIceAlb,
     & SEAICE_wetIceAlb, SEAICE_drySnowAlb, SEAICE_wetSnowAlb,
     & SEAICE_waterAlbedo, SEAICE_strength,
     & SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce,
     & SEAICE_iceConduct, SEAICE_snowConduct, SEAICE_emissivity,
     & SEAICE_snowThick, SEAICE_shortwave, SEAICE_freeze, OCEAN_drag,
     & uwindFile, vwindFile, atempFile, aqhFile, lwdownFile,
     & swdownFile, precipFile, evapFile, runoffFile, HeffFile,
     & LSR_ERROR, DIFF1, A22, HO,
     & WindForcingStart, WindForcingEnd, WindForcingPeriod,
     & FluxForcingStart, FluxForcingEnd, FluxForcingPeriod,
     & SSTForcingStart,  SSTForcingEnd,  SSTForcingPeriod,
     & SSSForcingStart,  SSSForcingEnd,  SSSForcingPeriod,
     & StartingYear, EndingYear,
     & MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE,
     & SEAICE_EPS, SEAICE_EPS_SQ, 
     & SEAICE_tave_mnc
c    & SEAICE_tave_mnc, SEAICE_dump_mnc, SEAICE_mon_mnc

#ifdef ALLOW_COST
      NAMELIST /SEAICE_PARM02/
     &          mult_ice, cost_ice_flag,
     &          costIceStart1, costIceStart2,
     &          costIceEnd1, costIceEnd2,
     &          cost_ice_flag,
     &          mult_smrarea, smrareadatfile, smrareabarfile,
     &          wsmrarea0, wmean_smrarea, smrarea_errfile,
     &          smrareastartdate1, smrareastartdate2, smrareaperiod
#endif

      _BEGIN_MASTER(myThid)

      write(msgbuf,'(A)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      WRITE(msgBuf,'(A)') ' SEAICE_READPARMS: opening data.seaice'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.seaice', 'SEAICE_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   set default sea ice parameters
      SEAICEwriteState   = .FALSE.
#ifdef SEAICE_ALLOW_DYNAMICS
      SEAICEuseDYNAMICS  = .TRUE.
#else
      SEAICEuseDYNAMICS  = .FALSE.
#endif
      SEAICE_deltaTtherm = dTtracerLev(1)
      SEAICE_deltaTdyn   = dTtracerLev(1)
      SEAICE_dumpFreq    = dumpFreq
      SEAICE_taveFreq    = taveFreq
#ifdef ALLOW_MNC
      SEAICE_tave_mnc = timeave_mnc
      SEAICE_dump_mnc = snapshot_mnc
      SEAICE_mon_mnc  = monitor_mnc
#else
      SEAICE_tave_mnc = .FALSE.
      SEAICE_dump_mnc = .FALSE.
      SEAICE_mon_mnc  = .FALSE.
#endif
      SEAICE_initialHEFF = ZERO
      SEAICE_drag        = 0.002  _d 0
      OCEAN_drag         = 0.001  _d 0
      SEAICE_waterDrag   = 5.5    _d 0
      SEAICE_dryIceAlb   = 0.75   _d 0
      SEAICE_wetIceAlb   = 0.66   _d 0
      SEAICE_drySnowAlb  = 0.84   _d 0
      SEAICE_wetSnowAlb  = 0.7    _d 0
      SEAICE_waterAlbedo = 0.1    _d +00
      SEAICE_strength    = 2.75   _d +04
      SEAICE_sensHeat    = 2.284  _d +00
      SEAICE_latentWater = 5.6875 _d +03
      SEAICE_latentIce   = 6.4474 _d +03
      SEAICE_iceConduct  = 2.1656 _d +00
      SEAICE_snowConduct = 3.1    _d -01
      SEAICE_emissivity  = 5.5    _d -08
      SEAICE_snowThick   = 0.15   _d 0
      SEAICE_shortwave   = 0.30   _d 0
      SEAICE_freeze      = -1.96  _d 0
      uwindFile  = ' '
      vwindFile  = ' '
      atempFile  = ' '
      aqhFile    = ' '
      lwdownFile = ' '
      swdownFile = ' '
      precipFile = ' '
      evapFile   = ' '
      runoffFile = ' '
      HeffFile   = ' '
      LAD        = 2
      IMAX_TICE  = 10
      LSR_ERROR  = 0.0001    _d 0
      DIFF1      = .002      _d 0
      DIFF1      = 2.0*DIFF1
      A22        = 0.15      _d 0
      HO         = 0.5       _d 0
C DOUBLE HO BECAUSE OF MOD IN GROWTH
      HO=2.0*HO
      WindForcingStart  = -99999.
      WindForcingEnd    = -99999.
      WindForcingPeriod = -99999.
      FluxForcingStart  = -99999.
      FluxForcingEnd    = -99999.
      FluxForcingPeriod = -99999.
      SSTForcingStart   = -99999.
      SSTForcingEnd     = -99999.
      SSTForcingPeriod  = -99999.
      SSSForcingStart   = -99999.
      SSSForcingEnd     = -99999.
      SSSForcingPeriod  = -99999.
      StartingYear      = 1948.
      EndingYear        = 2000.
      MAX_HEFF          = 10.     _d 0
      MIN_ATEMP         = -50.    _d 0
      MIN_LWDOWN        = 60.     _d 0
      MAX_TICE          = 30.     _d 0
      MIN_TICE          = -50.    _d 0
      SEAICE_EPS        = 1.      _d -10
      SEAICE_EPS_SQ     = -99999.

#ifdef ALLOW_COST
      mult_ice          =  0. _d 0
      costIceStart1     =  0
      costIceStart2     =  0
      costIceEnd1       =  0
      costIceEnd2       =  0
      cost_ice_flag     =  1
c
      mult_smrarea      =  0. _d 0
      wsmrarea0         =  0.5 _d 0
      wmean_smrarea     =  0.5 _d 0
      smrareabarfile    =  'smrareabar'
      smrareadatfile    =  ' '
      smrarea_errfile   =  ' '
# ifdef ALLOW_CAL
      smrareastartdate1 = startDate_1
      smrareastartdate2 = startDate_2
# endif
#endif

C--   Read settings from model parameter file "data.seaice".
      READ(UNIT=iUnit,NML=SEAICE_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R SEAICE_READPARMS'
       CALL PRINT_ERROR( msgBuf , mythid)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , mythid)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.seaice"'
       CALL PRINT_ERROR( msgBuf , mythid)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist SEAICE_PARM01'
       CALL PRINT_ERROR( msgBuf , mythid)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF

#ifdef ALLOW_COST
      READ(UNIT=iUnit,NML=SEAICE_PARM02,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R SEAICE_READPARMS'
       CALL PRINT_ERROR( msgBuf , mythid)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , mythid)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.seaice"'
       CALL PRINT_ERROR( msgBuf , mythid)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist SEAICE_PARM02'
       CALL PRINT_ERROR( msgBuf , mythid)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF
#endif

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)')
     &     ' SEAICE_READPARMS: finished reading data.seaice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , mythid)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C     Check that requested time step size is supported.  The combination
C     below is the only one that is supported at this time.  Does not
C     mean that something fancier will not work, just that it has not
C     yet been tried nor thought through.
      IF ( SEAICE_deltaTtherm .NE. dTtracerLev(1)     .OR.
     &     SEAICE_deltaTdyn   .LT. SEAICE_deltaTtherm .OR.
     &     (SEAICE_deltaTdyn/SEAICE_deltaTtherm) .NE.
     &     INT(SEAICE_deltaTdyn/SEAICE_deltaTtherm) ) THEN
         WRITE(msgBuf,'(A)')
     &        'Unsupported combination of SEAICE_deltaTtherm,'
         CALL PRINT_ERROR( msgBuf , mythid)
         WRITE(msgBuf,'(A)')
     &        ' SEAICE_deltaTdyn, and dTtracerLev(1)'
         CALL PRINT_ERROR( msgBuf , mythid)
         STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF


#ifndef SEAICE_EXTERNAL_FORCING
      IF ( FluxForcingStart  .EQ. -99999. .OR.
     &     FluxForcingEnd    .EQ. -99999. .OR.
     &     FluxForcingPeriod .EQ. -99999.      ) THEN
         WRITE(msgBuf,'(A)') 'Specify FluxForcing* in data.seaice'
         CALL PRINT_ERROR( msgBuf , mythid)
         STOP 'ABNORMAL END: S/R SEAICE_READPARMS'
      ENDIF
      IF ( WindForcingStart  .EQ. -99999. )
     &     WindForcingStart  = FluxForcingStart
      IF ( WindForcingEnd    .EQ. -99999. )
     &     WindForcingEnd    = FluxForcingEnd
      IF ( WindForcingPeriod .EQ. -99999. )
     &     WindForcingPeriod = FluxForcingPeriod
      IF ( SSTForcingStart  .EQ. -99999. )
     &     SSTForcingStart  = FluxForcingStart
      IF ( SSTForcingEnd    .EQ. -99999. )
     &     SSTForcingEnd    = FluxForcingEnd
      IF ( SSTForcingPeriod .EQ. -99999. )
     &     SSTForcingPeriod = FluxForcingPeriod
      IF ( SSSForcingStart  .EQ. -99999. )
     &     SSSForcingStart  = FluxForcingStart
      IF ( SSSForcingEnd    .EQ. -99999. )
     &     SSSForcingEnd    = FluxForcingEnd
      IF ( SSSForcingPeriod .EQ. -99999. )
     &     SSSForcingPeriod = FluxForcingPeriod
#endif /* SEAICE_EXTERNAL_FORCING */

      IF ( SEAICE_EPS_SQ .EQ. -99999. )
     &     SEAICE_EPS_SQ = SEAICE_EPS * SEAICE_EPS

C-    Set Output type flags :
      SEAICE_tave_mdsio = .TRUE.
      SEAICE_dump_mdsio = .TRUE.
      SEAICE_mon_stdio  = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
C-------
C-    seaice Monitor is (unfortunately) not independent from the main monitor
C       => turn off MNC flags for now
        SEAICE_mon_mnc  = .FALSE.
C-------
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_tave_mnc ) SEAICE_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_dump_mnc ) SEAICE_dump_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SEAICE_mon_mnc  ) SEAICE_mon_stdio  = .FALSE.
      ENDIF
#endif

C--   Summarise pkg/seaice cofiguration
      CALL SEAICE_SUMMARY( myThid )

C     Initialize MNC variable information for SEAICE
      IF ( useMNC .AND. 
     &    (seaice_tave_mnc.OR.seaice_dump_mnc.OR.SEAICE_mon_mnc)
     &   ) THEN
        CALL SEAICE_MNC_INIT( myThid )
      ENDIF

      RETURN
      END
