C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_my_data.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_MY_DATA(  bi,bj,
     I                               myIter, 
     I                               myTime, 
     I                               myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_MY_DATA                             |
C     | o Routine for controlling storage of coupling data to    |
C     |   coupler layer.                                         |
C     |==========================================================|
C     | This version interfaces to the MITgcm AIMPHYS package.   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     bi,bj  - Current tile indices
C     myIter - Current timestep number.
C     myTime - Current internal time.
C     myThid        - Thread number for this instance 
C                     of the routine.
      INTEGER bi,bj
      INTEGER myIter
      _RL     myTime
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==

C     Store coupling data ready for export
      CALL ATM_STORE_ATMSLP(     bi,bj, myTime, myIter, myThid )
      CALL ATM_STORE_HEATFLUX(   bi,bj, myTime, myIter, myThid )
      CALL ATM_STORE_QSHORTWAVE( bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_QSENSIBLE(  bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_QLATENT(    bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_QLONGWAVE(  bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_UVELGROUND( bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_VVELGROUND( bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_TAUX(       bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_TAUY(       bi,bj, myTime, myIter, myThid )
      CALL ATM_STORE_EVMPR(      bi,bj, myTime, myIter, myThid )
      IF ( useLand ) THEN
        CALL ATM_STORE_RUNOFF(   bi,bj, myTime, myIter, myThid )
        CALL ATM_STORE_ROENFX(   bi,bj, myTime, myIter, myThid )
      ENDIF
      IF ( useThSIce ) THEN
        CALL ATM_STORE_SALTFX(   bi,bj, myTime, myIter, myThid )
        CALL ATM_STORE_SEAICE(   bi,bj, myTime, myIter, myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
