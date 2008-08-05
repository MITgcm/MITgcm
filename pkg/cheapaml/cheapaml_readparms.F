C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_readparms.F,v 1.1 2008/08/05 21:49:31 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE CHEAPAML_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE CHEAPAML_READPARMS                            |
C     | o Routine to initialize CHEAPAML variables and consts.   |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CHEAPAML.h"

C     === Routine arguments ===
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   Bulk Formula parameter
      NAMELIST /CHEAPAML_CONST/
     &  cheapaml_ntim, cheapaml_h,
c     &  cheapaml_kdiff, cheapaml_taurelax1,
c     &  cheapaml_taurelax2, cheapaml_xf,
     &  cheapaml_kdiff, 
     &  cheapaml_taurelax2, 
     &  cdrag_1,cdrag_2,cdrag_3,
     &  rhoa, cpair, stefan,
     &       dsolms,dsolmn,
     &       xphaseinit
c     &  cheapaml_dtc, rhoa, cpair, stefan
      NAMELIST /CHEAPAML_PARM01/
     &         AirTempFile, 
     &         SolarFile, 
     &  	UWindFile,
     &         VWindFile, 
     &		TRFile
      NAMELIST /CHEAPAML_PARM02/
     &         useFreshwaterFlux 


      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: opening data.cheapaml'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
    

     
      CALL OPEN_COPY_DATA_FILE(
     I                          'data.cheapaml', 'CHEAPAML_READPARMS',
     O                          iUnit,
     I                          myThid )

      WRITE (msgBuf,'(A)') 'Test 1 ********************************'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
               



C--   Default values 
C-    Physical constant :
      cheapaml_ntim = 5
      cheapaml_h=1000.d0
      cheapaml_kdiff=1.d4
c	cheapaml_taurelax1 = 30.d0
      cheapaml_taurelax2 = .1d0
c	cheapaml_xf=2.d0
c	cheapaml_dtc=1.d0
      rhoa   = 1.3 _d 0
      cpair  = 1004.d0
      stefan = 5.67 _d -8
      dsolms = 50.d0
      dsolmn = 25.d0
      xphaseinit = 0.d0
      cdrag_1=0.0027d0
      cdrag_2=0.00142d0
      cdrag_3=0.0000764d0

C-    Input data files names :
      AirTempFile=' '
      SolarFile=' '
      UWindFile=' '
      VWindFile=' '
      TRFile=' '

C-    e-p option turned off by default
        useFreshwaterFlux=.FALSE.

      WRITE (msgBuf,'(A)') 'Test 2 ********************************'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

                 


C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=CHEAPAML_CONST)

      WRITE (msgBuf,'(A)') 'Test 3333333333333333333333333333333333'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           

      
      WRITE(msgBuf,'(A)') ' CHEAPAML_READPARMS: read CHEAPAML_CONST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=CHEAPAML_PARM01)
      WRITE(msgBuf,'(A)') ' CHEAPAML_READPARMS: read CHEAPAML_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


      WRITE (msgBuf,'(A)') 'Test 4444444444444444444444444444444444'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=CHEAPAML_PARM02)

      WRITE (msgBuf,'(A)') 'Test 55555555555555555555555555555555555'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

            
      
      
      WRITE(msgBuf,'(A)') ' CHEAPAML_READPARMS: read CHEAPAML_PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


      WRITE (msgBuf,'(A)') 'Test 6666666666666666666666666666666666'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
                 


C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      iUnit = standardMessageUnit
      WRITE(iUnit,*) 'Caml: ntim     =',cheapaml_ntim
      WRITE(iUnit,*) 'Caml: h        =',cheapaml_h
      WRITE(iUnit,*) 'Caml: kdiff    =',cheapaml_kdiff
c      WRITE(iUnit,*) 'Caml: tau1     =',cheapaml_taurelax1
      WRITE(iUnit,*) 'Caml: tau2     =',cheapaml_taurelax2
c      WRITE(iUnit,*) 'Caml: xf       =',cheapaml_xf
c      WRITE(iUnit,*) 'Caml: dtc      =',cheapaml_dtc
      WRITE(iUnit,*) 'Caml: rhoa     =',rhoa
      WRITE(iUnit,*) 'Caml: cpair    =',cpair
      WRITE(iUnit,*) 'Caml: stefan   =',stefan
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
