C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_evmpr.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_EVMPR( bi,bj,
     I                             myCurrentTime, 
     I                             myCurrentIter, 
     I                             myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_EVMPR                               |
C     | o Routine for saving atmos. fresh water flux             |
C     |   (=Evap-Precip) for export to coupling layer.           |
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
C     conv_precip  :: conversion factor for precip: from g.m-2.s-1 to m/s
      INTEGER I,J,K,II
      _RL conv_precip
      _RL cplTimeFraction

C     from g/(m^2.s) to m/s :
      conv_Precip = 1. _d -3 / rhoConstFresh

C     o Accumulate fresh water flux that will be exported to the 
C       coupling layer. Fresh-water flux is in units of g/m^2/s. 
C       To convert to meters per second divide by density in 
C       g/m^3. Positive flux is into the atmosphere (E-P).
      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
        EvMPrTime(bi,bj) = EvMPrTime(bi,bj) + cplTimeFraction
        DO J=1,sNy
         DO I=1,sNx
          II = (sNx)*(J-1)+I
          EvMPrFlux(I,J,bi,bj) = EvMPrFlux(I,J,bi,bj) 
     &                         +( EVAP(II,2,myThid)
     &                           -PRECNV(II,myThid)
     &                           -PRECLS(II,myThid)
     &                           )*conv_precip*cplTimeFraction
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO

      RETURN
      END
