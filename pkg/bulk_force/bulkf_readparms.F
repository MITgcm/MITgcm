C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_readparms.F,v 1.7 2014/05/27 23:41:28 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

      SUBROUTINE BULKF_READPARMS( myThid )
C     *==========================================================*
C     | SUBROUTINE BULKF_READPARMS                               |
C     | o Routine to initialize BULKF variables and constants.   |
C     *==========================================================*
C     | Initialize BULKF    parameters, read in data.blk         |
C     *==========================================================*
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
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   Bulk Formula parameter
      NAMELIST /BULKF_CONST/
     &  rhoA, rhoFW,
     &  cpAir, Lvap, Lfresh,
     &  Tf0kel, Rgas, xkar, stefan,
     &  zref, zwd, zth,
     &  cDrag_1, cDrag_2, cDrag_3,
     &  cStantonS, cStantonU, cDalton,
     &  umin, humid_fac, saltQsFac, gamma_blk,
     &  atm_emissivity, ocean_emissivity,
     &  snow_emissivity, ice_emissivity,
#ifdef ALLOW_FORMULA_AIM
     &  FWIND0, CHS, VGUST, DTHETA, dTstab, FSTAB,
#endif
     &  ocean_albedo

      NAMELIST /BULKF_PARM01/
     &         useFluxFormula_AIM, blk_nIter, calcWindStress,
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useBulkForce ) THEN
C-    pkg BULK_FORCE is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.blk is found
         CALL PACKAGES_UNUSED_MSG(
     I                'useBulkForce', 'BULKF_READPARMS', 'blk' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.blk', 'BULKF_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values
C-    Physical constant :
c     slp0   = atm_Po / 100.  ! reference sea-level atmospheric pressure [mb]
      rhoA   = 1.3 _d 0
      rhoFW  = rhoConstFresh
      cpAir  = atm_Cp
c     cpwv   = 1.81 _d 3
      Lvap   = 2.5 _d 6
      Lfresh = 3.34 _d 5
c     Lvap_ice = 2.83 _d 6
      Tf0kel = celsius2K
      Rgas   = atm_Rd
c     Rvap   = 461. _d 0
      xkar   = 0.4  _d 0
      stefan = 5.67 _d -8
      zref   = 10.0 _d 0
      zwd    = zref
      zth    = zref
      cDrag_1 = 2.70   _d -3
      cDrag_2 = 0.142  _d -3
      cDrag_3 = 0.0764 _d -3
      cStantonS = 18.0 _d -3
      cStantonU = 32.7 _d -3
      cDalton   = 34.6 _d -3
      umin      =  1.0 _d 0
      humid_fac =  0.606 _d 0
      saltQsFac =  0.980 _d 0
      gamma_blk =  0.010 _d 0
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
      blk_nIter = 5
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
      WRITE(msgBuf,'(A)')' BULKF_READPARMS: starts to read BULKF_CONST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      READ(UNIT=iUnit,NML=BULKF_CONST)
      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_CONST : OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)

      WRITE(msgBuf,'(A)')' BULKF_READPARMS: starts to read BULKF_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      READ(UNIT=iUnit,NML=BULKF_PARM01)
      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_PARM01 : OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)

#ifdef CONSERV_BULKF
c -- default
      qnet_off=0.d0
      empmr_off=0.d0
      WRITE(msgBuf,'(A)')' BULKF_READPARMS: starts reading BULKF_PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      READ(UNIT=iUnit,NML=BULKF_PARM02)
      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_PARM02 : OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)

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
c     Qcoef  = 6.11 _d 0 * 0.622 _d 0 / p0
c     Sha    = Rgas / .286 _d 0

      useQnetch = QnetFile .NE. ' '
      useEmPch  = EmPFile  .NE. ' '

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      iUnit = standardMessageUnit
c     iUnit=88
c     OPEN(iUnit,file='bulkf_check_params',status='unknown')
c     WRITE(iUnit,*) 'BlkF: slp0     =',slp0
      WRITE(iUnit,*) 'BlkF: rhoA     =',rhoA
      WRITE(iUnit,*) 'BlkF: rhoFW    =',rhoFW
      WRITE(iUnit,*) 'BlkF: cpAir    =',cpAir
c     WRITE(iUnit,*) 'BlkF: cpwv     =',cpwv
      WRITE(iUnit,*) 'BlkF: Lvap     =',Lvap
      WRITE(iUnit,*) 'BlkF: Lfresh   =',Lfresh
c     WRITE(iUnit,*) 'BlkF: Lvap_ice =',Lvap_ice
c     WRITE(iUnit,*) 'BlkF: Sha      =',Sha
      WRITE(iUnit,*) 'BlkF: Tf0kel   =',Tf0kel
      WRITE(iUnit,*) 'BlkF: Rgas     =',Rgas
c     WRITE(iUnit,*) 'BlkF: Rvap     =',Rvap
      WRITE(iUnit,*) 'BlkF: xkar     =',xkar
      WRITE(iUnit,*) 'BlkF: stefan   =',stefan
      WRITE(iUnit,*) 'BlkF: zref     =',zref
      WRITE(iUnit,*) 'BlkF: zwd      =',zwd
      WRITE(iUnit,*) 'BlkF: zth      =',zth
      WRITE(iUnit,*) 'BlkF: cDrag_1  =',cDrag_1
      WRITE(iUnit,*) 'BlkF: cDrag_2  =',cDrag_2
      WRITE(iUnit,*) 'BlkF: cDrag_3  =',cDrag_3
      WRITE(iUnit,*) 'BlkF: cStantonS=',cStantonS
      WRITE(iUnit,*) 'BlkF: cStantonU=',cStantonU
      WRITE(iUnit,*) 'BlkF: cDalton  =',cDalton
      WRITE(iUnit,*) 'BlkF: umin     =',umin
      WRITE(iUnit,*) 'BlkF: humid_fac=',humid_fac
      WRITE(iUnit,*) 'BlkF: saltQsFac=',saltQsFac
      WRITE(iUnit,*) 'BlkF: gamma_blk=',gamma_blk
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
      WRITE(iUnit,*) 'BlkF: calcWindStress  =', calcWindStress
      WRITE(iUnit,*) 'BlkF: useQnetch       =', useQnetch
      WRITE(iUnit,*) 'BlkF: useEmPch        =', useEmPch
      WRITE(iUnit,*) 'BlkF: blk_nIter   =',blk_nIter
      WRITE(iUnit,*) 'BlkF: blk_taveFreq=', blk_taveFreq
      IF (iUnit.EQ.88) CLOSE(iUnit)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_BULK_FORCE */

      RETURN
      END
