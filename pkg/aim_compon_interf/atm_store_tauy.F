C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_store_tauy.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $  

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_TAUY( bi,bj,
     I                           myTime, 
     I                           myIter, 
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
c #include "AIM_FFIELDS.h"
#ifdef ALLOW_THSICE
# include "THSICE_PARAMS.h"
# include "THSICE_VARS.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"


C     == Routine arguments ==
C     bi,bj  - Tile index
C     myThid - Thread number for this instance of the routine
C     myIter - Current timestep number
C     myTime - Current model time
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     i,j,k - Loop counters
      INTEGER i,j,k
      _RL vStr_tmp
      _RL cplTimeFraction

C     o Accumulate meridional momentum flux that will be exported to the 
C       coupling layer. Momentum flux is in N/m^2 with same sign as the 
C       wind. Momentum flux is in COMMON /FLUXES/, com_physvar.h.
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       k = 1
       tauYTime(bi,bj) = tauYTime(bi,bj) + cplTimeFraction
#ifdef ALLOW_THSICE
      IF ( useThSIce .AND. stressReduction.GT. 0. _d 0 ) THEN
C--   Reduce wind stress applied to ocean where sea-ice is present
       DO j=1,sNy
         DO i=1,sNx
c         IF ( aim_landFr(i,j-1,bi,bj)*aim_landFr(i,j,bi,bj) .NE. 1. ) THEN
          IF ( hFacS(i,j,k,bi,bj) .NE. 0. ) THEN
           vStr_tmp =
     &      ( aim_drag(i,j-1,bi,bj)
     &       *(1. _d 0 - iceMask(i,j-1,bi,bj)*stressReduction)
     &      + aim_drag(i, j ,bi,bj)
     &       *(1. _d 0 - iceMask(i, j ,bi,bj)*stressReduction)
     &      )* 0.5 _d 0 * vVel(i,j,k,bi,bj)
           tauY(i,j,bi,bj) = tauY(i,j,bi,bj)
     &                     + vStr_tmp*cplTimeFraction
          ENDIF
         ENDDO
       ENDDO
      ELSE
#else  /*ALLOW_THSICE*/
      IF (.TRUE.) THEN
#endif  /*ALLOW_THSICE*/
       DO j=1,sNy
         DO i=1,sNx
c         IF ( aim_landFr(i,j-1,bi,bj)*aim_landFr(i,j,bi,bj) .NE. 1. ) THEN
          IF ( hFacS(i,j,k,bi,bj) .NE. 0. ) THEN
           vStr_tmp = 
     &      ( aim_drag(i,j-1,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * vVel(i,j,k,bi,bj)
           tauY(i,j,bi,bj) = tauY(i,j,bi,bj)
     &                     + vStr_tmp*cplTimeFraction
          ENDIF
         ENDDO
       ENDDO
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
