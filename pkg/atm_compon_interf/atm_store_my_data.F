C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_my_data.F,v 1.5 2013/09/11 20:17:27 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_MY_DATA
C     !INTERFACE:
      SUBROUTINE ATM_STORE_MY_DATA(
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
C     myTime    :: Current internal time.
C     myIter    :: Current timestep number.
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     bi, bj    :: Tile indices
      INTEGER bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C     Store coupling data ready for export
        CALL ATM_STORE_DYNVARS(     bi, bj, myTime, myIter, myThid )
        CALL ATM_STORE_SURFFLUX(    bi, bj, myTime, myIter, myThid )
        IF ( useAIM ) THEN
         CALL ATM_STORE_AIM_WNDSTR( bi, bj, myTime, myIter, myThid )
        ENDIF
        IF ( useAIM .AND. atmCpl_exchange_DIC ) THEN
         CALL ATM_STORE_AIM_FIELDS( bi, bj, myTime, myIter, myThid )
        ENDIF
        IF ( useLand ) THEN
         CALL ATM_STORE_LAND(       bi, bj, myTime, myIter, myThid )
        ENDIF
        IF ( useThSIce ) THEN
         CALL ATM_STORE_THSICE(     bi, bj, myTime, myIter, myThid )
        ENDIF

C     Store coupling data ready for export
c     CALL ATM_STORE_ATMSLP(     bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_HEATFLUX(   bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_QSHORTWAVE( bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_TAUX(       bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_TAUY(       bi,bj, myTime, myIter, myThid )
c     CALL ATM_STORE_EVMPR(      bi,bj, myTime, myIter, myThid )
c     IF ( useLand ) THEN
c       CALL ATM_STORE_RUNOFF(   bi,bj, myTime, myIter, myThid )
c       CALL ATM_STORE_ROENFX(   bi,bj, myTime, myIter, myThid )
c     ENDIF
c     IF ( useThSIce ) THEN
c       CALL ATM_STORE_SALTFX(   bi,bj, myTime, myIter, myThid )
c       CALL ATM_STORE_SEAICE(   bi,bj, myTime, myIter, myThid )
c     ENDIF
c     IF ( atmCpl_exchange_DIC ) THEN
c       CALL ATM_STORE_WSPEED(   bi,bj, myTime, myIter, myThid )
c       IF ( useThSIce )
c    &  CALL ATM_STORE_FRACICE(  bi,bj, myTime, myIter, myThid )
c       CALL ATM_STORE_AIRCO2(   bi,bj, myTime, myIter, myThid )
c     ENDIF

C--   end bi,bj loops.
       ENDDO
      ENDDO

#endif /* COMPONENT_MODULE */

      RETURN
      END
