C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_store_qshortwave.F,v 1.2 2004/04/18 15:38:45 jmc Exp $
C $Name:  $  

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_QSHORTWAVE(bi,bj,
     I                                myCurrentTime, 
     I                                myCurrentIter, 
     I                                myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_QSHORTWAVE                          |
C     | o Routine for saving atmos. lower bdy shortwave heat flux|
C     |   export to coupling layer.                              |
C     |==========================================================|
C     | This version interfaces to the MITgcm AIMPHYS package.   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "AIM_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
C     == Global variables for coupling interface ==
#include "ATMCPL.h"
C     == AIMPHYS specific global data ==
#include "com_physvar.h"


C     == Routine arguments ==
C     bi,bj         - Tile index
C     myThid        - Thread number for this instance of the routine
C     myCurrentIter - Current timestep number
C     myCurrentTime - Current model time
      INTEGER bi, bj
      _RL     myCurrentTime
      INTEGER myCurrentIter
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I,J,K,II - Loop counters
      INTEGER I,J,K,II
      _RL cplTimeFraction

C     o Accumulate heat flux that will be exported to the coupling layer.
C     +ve into atmos., W/m^2.
      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
        QshortwaveTime(bi,bj) = QshortwaveTime(bi,bj) + cplTimeFraction
        DO J=1,sNy
         DO I=1,sNx
          II = (sNx)*(J-1)+I
          Qshortwave(I,J,bi,bj) = 
     &    Qshortwave(I,J,bi,bj) - SSR(II,2,myThid)*cplTimeFraction
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO

      RETURN
      END
