C $Header:

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

C     === Routine arguments ===
C     myThid -  Number of this instance of SEAICE_READPARMS
      INTEGER myThid

#ifdef ALLOW_SEAICE

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit

C--   SEAICE parameters
      NAMELIST /SEAICE_PARM01/
     & SEAICEwriteState, SEAICEuseDYNAMICS, SEAICEuseLSR,
     & SEAICEwindOnCgrid,
     & LAD, IMAX_TICE, NPSEUDO,
     & SEAICE_dumpFreq, SEAICE_taveFreq,
     & SEAICE_drag, SEAICE_waterDrag, SEAICE_albedo,
     & SEAICE_drySnowAlb, SEAICE_wetSnowAlb,
     & SEAICE_waterAlbedo, SEAICE_strength,
     & SEAICE_sensHeat, SEAICE_latentWater, SEAICE_latentIce,
     & SEAICE_iceConduct, SEAICE_snowConduct, SEAICE_emissivity,
     & SEAICE_snowThick, SEAICE_shortwave,
     & uwindFile, vwindFile, atempFile, aqhFile, lwdownFile,
     & swdownFile, precipFile, evapFile, runoffFile, HeffFile,
     & LSR_ERROR, DIFF1, A22, HO,
     & WindForcingStart, WindForcingEnd, WindForcingPeriod,
     & FluxForcingStart, FluxForcingEnd, FluxForcingPeriod,
     & SSTForcingStart,  SSTForcingEnd,  SSTForcingPeriod,
     & SSSForcingStart,  SSSForcingEnd,  SSSForcingPeriod,
     & StartingYear, EndingYear,
     & MAX_HEFF, MIN_ATEMP, MIN_LWDOWN, MAX_TICE, MIN_TICE

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' SEAICE_INIT: opening data.seaice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.seaice', 'SEAICE_INIT',
     O                          iUnit,
     I                          myThid )

C--   set default sea ice parameters
      SEAICEwriteState   = .FALSE.
      SEAICEuseDYNAMICS  = .TRUE.
      SEAICEuseLSR       = .FALSE.
      SEAICEwindOnCgrid  = .FALSE.
      SEAICE_dumpFreq    = dumpFreq
      SEAICE_taveFreq    = taveFreq
      SEAICE_drag        = 0.001  _d 0
      SEAICE_waterDrag   = 5.5    _d 0
      SEAICE_albedo      = 0.66   _d 0
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
      NPSEUDO    = 10
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
      MAX_HEFF          = 6.      _d 0
      MIN_ATEMP         = -50.    _d 0
      MIN_LWDOWN        = 60.     _d 0
      MAX_TICE          = 30.     _d 0
      MIN_TICE          = -50.    _d 0

C--   Read settings from model parameter file "data.seaice".
      READ(UNIT=iUnit,NML=SEAICE_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.seaice"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist SEAICE_PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R SEAICE_INIT'
      ENDIF

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' SEAICE_INIT: finished reading data.seaice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#ifndef SEAICE_EXTERNAL_FORCING
      IF ( FluxForcingStart  .EQ. -99999. .OR.
     &     FluxForcingEnd    .EQ. -99999. .OR.
     &     FluxForcingPeriod .EQ. -99999.      ) THEN
         WRITE(msgBuf,'(A)') 'Specify FluxForcing* in data.seaice'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R SEAICE_GET_FORCING'
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

C--   No pseudo time steps are needed for LSR solver.
      IF ( SEAICEuseLSR ) NPSEUDO = 1

#endif /* ALLOW_SEAICE */

      return
      end
