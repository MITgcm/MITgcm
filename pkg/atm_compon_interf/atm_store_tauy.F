C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_tauy.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_TAUY( bi,bj,
     I                           myCurrentTime, 
     I                           myCurrentIter, 
     I                           myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_TAUY                                |
C     | o Routine for saving atmos. lower boundary momentum flux |
C     |   for export to coupling layer.                          |
C     |==========================================================|
C     | This version interfaces to the MITgcm AIMPHYS package.   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "AIM_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "AIM2DYN.h"
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
      INTEGER I,J,kLev,II
      _RL vStr_tmp
      _RL cplTimeFraction

C     o Accumulate meridional momentum flux that will be exported to the 
C       coupling layer. Momentum flux is in N/m^2 with same sign as the 
C       wind. Momentum flux is in COMMON /FLUXES/, com_physvar.h.
      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
      kLev     =  1
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
        tauYTime(bi,bj) = tauYTime(bi,bj) + cplTimeFraction
        DO J=1,sNy
         DO I=1,sNx
          IF ( hFacS(i,j,kLev,bi,bj) .NE. 0. ) THEN
           vStr_tmp = 
     &      ( aim_drag(i,j-1,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * vVel(i,j,kLev,bi,bj)
           tauY(I,J,bi,bj) = 
     &     tauY(I,J,bi,bj) + vStr_tmp*cplTimeFraction
          ENDIF
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO

      RETURN
      END
