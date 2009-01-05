C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_store_my_data.F,v 1.4 2009/01/05 15:19:17 dfer Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_MY_DATA
C     !INTERFACE:
      SUBROUTINE ATM_STORE_MY_DATA( bi,bj,
     I                              myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_MY_DATA
C     | o Routine for controlling storage of coupling data to
C     |   coupler layer.
C     *==========================================================*
C     | This version interfaces to the MITgcm AIMPHYS package.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATMIDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj   :: Current tile indices
C     myTime  :: Current internal time.
C     myIter  :: Current timestep number.
C     myThid  :: my Thread Id number
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

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
      IF ( atmCpl_exchange_DIC ) THEN
        CALL ATM_STORE_WSPEED(   bi,bj, myTime, myIter, myThid )
        IF ( useThSIce )
     &  CALL ATM_STORE_FRACICE(  bi,bj, myTime, myIter, myThid )
        CALL ATM_STORE_AIRCO2(   bi,bj, myTime, myIter, myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
