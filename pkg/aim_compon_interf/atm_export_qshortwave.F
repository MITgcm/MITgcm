C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_export_qshortwave.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_EXPORT_QSHORTWAVE( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_EXPORT_QSHORTWAVE                         |
C     | o Routine for exporting atmos. shortwave heat flux at    |
C     |   lower boundary to coupling layer.                      |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMCPL.h"
#include "ATMIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     recipAvT :: Temp. for holding reciprocal of averaging period.
C     I,J,K,II :: Loop counters
C     bi, bj   ::
      _RL recipAvT
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER II
      INTEGER bi
      INTEGER bj

C     Convert time integrated flux to mean value ready for
C     export.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( ABS(qShortWaveTime(bi,bj) -1. _d 0).GT. 1. _d -12
     &     .AND. qShortWaveTime(bi,bj) .NE. 0. ) THEN
         recipAvT = 1. _d 0/qShortWaveTime(bi,bj)
         DO J=1,sNy
          DO I=1,sNx
           qShortWave(I,J,bi,bj) = qShortWave(I,J,bi,bj)*recipAvT
          ENDDO
         ENDDO
         WRITE(errorMessageUnit,*)'ATM_EXPORT_QSHORTWAVE: 1-CountTime=',
     &                             bi,bj,1.-qShortWaveTime(bi,bj)
        ENDIF
       ENDDO
      ENDDO

C     o Send flux to coupling layer.
      CALL COMPSEND_R8TILES( atmQshortWaveName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, qShortWave )

C     Reset flux to start accumulating again.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        qShortWaveTime(bi,bj) = 0.
        DO J=1,sNy
         DO I=1,sNx
          qShortWave(I,J,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
