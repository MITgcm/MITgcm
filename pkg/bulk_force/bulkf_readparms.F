C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_readparms.F,v 1.4 2006/01/22 15:51:35 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

      SUBROUTINE BULKF_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE BULKF_READPARMS                               |
C     | o Routine to initialize BULKF variables and constants.   |
C     |==========================================================|
C     | Initialize BULKF    parameters, read in data.blk         |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#ifdef CONSERV_BULKF
#include "BULKF_CONSERV.h"
#endif

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_BULK_FORCE
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   Bulk Formula parameter
      NAMELIST /BULKF_CONST/
     &  rhoa, rhofw,
     &  cpair, cpwv,
     &  Lvap, Lfresh,
     &  Tf0kel,
     &  cdrag_1, cdrag_2, cdrag_3,
     &  stefan, xkar, Rvap,
     &  p0,
     &  humid_fac, saltsat, gamma_blk,
     &  Lvap_ice, Rgas,
     &  atm_emissivity, ocean_emissivity,
     &  snow_emissivity, ice_emissivity,
#ifdef ALLOW_FORMULA_AIM
     &  FWIND0, CHS, VGUST, DTHETA, dTstab, FSTAB,
#endif
     &  ocean_albedo

      NAMELIST /BULKF_PARM01/
     &         useFluxFormula_AIM, calcWindStress,
     &         blk_taveFreq,
     &         AirTempFile, AirHumidityFile, RainFile,
     &         SolarFile, LongwaveFile, UWindFile,
     &         VWindFile,  RunoffFile, WSpeedFile, QnetFile,
     &         EmPFile, CloudFile, airPotTempFile

#ifdef CONSERV_BULKF
c-    conserving qnet, empmr
      NAMELIST /BULKF_PARM02/
     &         qnet_off, empmr_off, conservcycle
#endif


      _BEGIN_MASTER(myThid)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.blk', 'BULKF_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values
C-    Physical constant :
      rhoa   = 1.3 _d 0
      rhofw  = rhoConstFresh
      cpair  = atm_Cp
      cpwv   = 1.81 _d 3
      Lvap   = 2.5 _d 6
      Lfresh = 3.34 _d 5
      Tf0kel = celsius2K
      cdrag_1= 0.0027000 _d 0
      cdrag_2= 0.0001420 _d 0
      cdrag_3= 0.0000764 _d 0
      stefan = 5.67 _d -8
      xkar   = 0.4  _d 0
      Rvap   = 461. _d 0
      p0     = atm_Po / 100.
      humid_fac =  0.606 _d 0
      saltsat   =  0.980 _d 0
      gamma_blk =  0.010 _d 0
      Lvap_ice  = 2.83 _d 6
      Rgas   = atm_Rd
      atm_emissivity  = .90 _d 0
      ocean_emissivity= .985 _d 0
      snow_emissivity = .98 _d 0
      ice_emissivity  = .98 _d 0
      ocean_albedo    = .10 _d 0
#ifdef ALLOW_FORMULA_AIM
      FWIND0 = 0.6 _d 0
      CHS = 0.8 _d -3
      VGUST  = 5. _d 0
      DTHETA = 3. _d 0
      dTstab = 1. _d 0
      FSTAB  = 0.67 _d 0
#endif

C-    bulk-forcing parameters:
      useFluxFormula_AIM = .FALSE.
      calcWindStress = zonalWindFile .EQ. ' '
     &           .AND. meridWindFile .EQ. ' '
      blk_taveFreq = taveFreq

C-    Input data files names :
      AirTempFile=' '
      AirHumidityFile=' '
      RainFile=' '
      SolarFile=' '
      LongwaveFile=' '
      UWindFile=' '
      VWindFile=' '
      WspeedFile=' '
      RunoffFile=' '
      QnetFile=' '
      EmPFile=' '
      CloudFile=' '
      SnowFile=' '
      airPotTempFile=' '

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=BULKF_CONST)
      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_CONST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      READ(UNIT=iUnit,NML=BULKF_PARM01)
      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#ifdef CONSERV_BULKF
c -- default
      qnet_off=0.d0
      empmr_off=0.d0
      READ(UNIT=iUnit,NML=BULKF_PARM02)
      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#endif /* CONSERV_BULKF */

C--   Close the open data file
      CLOSE(iUnit)

C-  check that CPP option is "defined" when running-flag parameter is on:
#ifndef ALLOW_FORMULA_AIM
      IF ( useFluxFormula_AIM ) THEN
        WRITE(msgBuf,'(2A)') ' BULKF_READPARMS: ',
     &   'useFluxFormula_AIM is TRUE and #undef ALLOW_FORMULA_AIM'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)') ' BULKF_READPARMS: => recompile with',
     &   ' #define ALLOW_FORMULA_AIM in BULK_FORCE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R CONFIG_CHECK'
      ENDIF
#endif

C-    Define other constants (from previous ones):
      Qcoef  = 6.11 _d 0 * 0.622 _d 0 / p0
      Sha    = Rgas / .286 _d 0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      iUnit = standardMessageUnit
c     iUnit=88
c     OPEN(iUnit,file='bulkf_check_params',status='unknown')
      WRITE(iUnit,*) 'BlkF: rhoa     =',rhoa
      WRITE(iUnit,*) 'BlkF: rhofw    =',rhofw
      WRITE(iUnit,*) 'BlkF: cpair    =',cpair
      WRITE(iUnit,*) 'BlkF: cpwv     =',cpwv
      WRITE(iUnit,*) 'BlkF: Lvap     =',Lvap
      WRITE(iUnit,*) 'BlkF: Lfresh   =',Lfresh
      WRITE(iUnit,*) 'BlkF: Tf0kel   =',Tf0kel
      WRITE(iUnit,*) 'BlkF: cdrag_1  =',cdrag_1
      WRITE(iUnit,*) 'BlkF: cdrag_2  =',cdrag_2
      WRITE(iUnit,*) 'BlkF: cdrag_3  =',cdrag_3
      WRITE(iUnit,*) 'BlkF: stefan   =',stefan
      WRITE(iUnit,*) 'BlkF: xkar     =',xkar
      WRITE(iUnit,*) 'BlkF: Rvap     =',Rvap
      WRITE(iUnit,*) 'BlkF: p0       =',p0
      WRITE(iUnit,*) 'BlkF: Qcoef    =',Qcoef
      WRITE(iUnit,*) 'BlkF: humid_fac=',humid_fac
      WRITE(iUnit,*) 'BlkF: saltsat  =',saltsat
      WRITE(iUnit,*) 'BlkF: gamma_blk=',gamma_blk
      WRITE(iUnit,*) 'BlkF: Lvap_ice =',Lvap_ice
      WRITE(iUnit,*) 'BlkF: Rgas     =',Rgas
      WRITE(iUnit,*) 'BlkF: Sha      =',Sha
      WRITE(iUnit,*) 'BlkF: atm_emissivity  =',atm_emissivity
      WRITE(iUnit,*) 'BlkF: ocean_emissivity=',ocean_emissivity
      WRITE(iUnit,*) 'BlkF: snow_emissivity =',snow_emissivity
      WRITE(iUnit,*) 'BlkF: ice_emissivity  =',ice_emissivity
      WRITE(iUnit,*) 'BlkF: ocean_albedo    =',ocean_albedo
#ifdef ALLOW_FORMULA_AIM
      WRITE(iUnit,*) 'BlkF: FWIND0   =', FWIND0
      WRITE(iUnit,*) 'BlkF: CHS      =', CHS
      WRITE(iUnit,*) 'BlkF: VGUST    =', VGUST
      WRITE(iUnit,*) 'BlkF: DTHETA   =', DTHETA
      WRITE(iUnit,*) 'BlkF: dTstab   =', dTstab
      WRITE(iUnit,*) 'BlkF: FSTAB    =', FSTAB
#endif
      WRITE(iUnit,*) 'BlkF: useFluxFormula_AIM=',useFluxFormula_AIM
      WRITE(iUnit,*) 'BlkF: calcWindStress    =',calcWindStress
      WRITE(iUnit,*) 'BlkF: blk_taveFreq=', blk_taveFreq
      IF (iUnit.EQ.88) CLOSE(iUnit)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_BULK_FORCE */

      RETURN
      END
