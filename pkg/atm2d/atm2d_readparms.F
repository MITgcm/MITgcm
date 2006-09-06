#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C
      SUBROUTINE ATM2D_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM2D_READPARMS                               |
C     |==========================================================|
C     | Initialize ATM2D parameters, read in data.atm2d          |
C     \==========================================================/
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

C     === Routine arguments ===
      INTEGER myThid

      NAMELIST /PARM01_ATM2D/ 
     &        atmosTauuFile, atmosTauvFile,atmosWindFile
  
      NAMELIST /PARM02_ATM2D/
     &        tauuFile, tauvFile, windFile,
     &        qnetFile, evapFile, precipFile

      NAMELIST /PARM03_ATM2D/
     &        thetaRelaxFile, saltRelaxFile,
     &        tauThetaRelax, tauSaltRelax,
     &        nttyperelax, nstyperelax

      NAMELIST /PARM04_ATM2D/
     &        runoffFile, runoffMapFile,
     &        numbands, rband

      NAMELIST /PARM05_ATM2D/
     &        useObsEmP,useObsRunoff,useAltDeriv
 

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER j     ! loop counter


      WRITE(msgBuf,'(A)') ' ATM2D_READPARMS: opening data.atm2d'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.atm2d', 'ATM2D_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values
      atmosTauuFile=' '
      atmosTauvFile=' '
      atmosWindFile=' '
      tauuFile=' '
      tauvFile=' '
      windFile=' '
      qnetFile=' '
      evapFile=' '
      precipFile=' '
      runoffFile=' '
      thetaRelaxFile=' '
      saltRelaxFile=' '
      runoffFile=' '
      runoffMapFile=' '
C
      useObsEmP=.FALSE.
      useObsRunoff=.FALSE. 
      useAltDeriv=.FALSE.    
C
      r_tauThetaRelax=0. _d 0
      r_tauSaltRelax=0. _d 0
      ntTypeRelax=0
      nsTypeRelax=0

      numBands=1
      do j=1,sNy
        rband(j)=0
      enddo

c--   Read parameters from open data file
      READ(UNIT=iUnit,NML=PARM01_ATM2D)
c
      READ(UNIT=iUnit,NML=PARM02_ATM2D)
c
      READ(UNIT=iUnit,NML=PARM03_ATM2D)
c
      READ(UNIT=iUnit,NML=PARM04_ATM2D)
c
      READ(UNIT=iUnit,NML=PARM05_ATM2D)
c
      WRITE(msgBuf,'(A)') ' ATM2D_READPARMS: read PARM01_ATM2D'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)

      IF (tauThetaRelax .NE. 0. _d 0) 
     &                  r_tauThetaRelax= 1. _d 0/tauThetaRelax
      IF (tauSaltRelax .NE. 0. _d 0) 
     &                  r_tauSaltRelax= 1. _d 0/tauSaltRelax

      RETURN
      END

