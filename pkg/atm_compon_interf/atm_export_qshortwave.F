C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_export_qshortwave.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
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
        IF ( ABS(QshortwaveTime(bi,bj) -1. _d 0).GT. 1. _d -12
     &     .AND. QshortwaveTime(bi,bj) .NE. 0. ) THEN
         recipAvT = 1. _d 0/QshortwaveTime(bi,bj)
         DO J=1,sNy
          DO I=1,sNx
           Qshortwave(I,J,bi,bj) = Qshortwave(I,J,bi,bj)*recipAvT
          ENDDO
         ENDDO
         WRITE(errorMessageUnit,*)'ATM_EXPORT_QSHORTWAVE: 1-CountTime=',
     &                             bi,bj,1.-QshortwaveTime(bi,bj)
        ENDIF
       ENDDO
      ENDDO

C     o Send flux to coupling layer.
      CALL COMPSEND_R8TILES( atmQshortwaveName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, Qshortwave )

C     Reset flux to start accumulating again.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        QshortwaveTime(bi,bj) = 0.
        DO J=1,sNy
         DO I=1,sNx
          Qshortwave(I,J,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
