C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_qlatent.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_QLATENT(  bi,bj,
     I                               myCurrentTime, 
     I                               myCurrentIter, 
     I                               myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_QLATENT                             |
C     | o Routine for saving atmos. lower bdy latent   heat flux |
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
#include "com_physcon.h"
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
      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
        QlatentTime(bi,bj) = QlatentTime(bi,bj) + cplTimeFraction
        DO J=1,sNy
         DO I=1,sNx
          II = (sNx)*(J-1)+I
          Qlatent(I,J,bi,bj) = 
     &    Qlatent(I,J,bi,bj) + EVAP(II,2,myThid)*ALHC*cplTimeFraction
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO

      RETURN
      END
