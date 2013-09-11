C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_aim_wndstr.F,v 1.1 2013/09/11 20:17:27 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AIM
# include "AIM_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ATM_STORE_AIM_WNDSTR
C     !INTERFACE:
      SUBROUTINE ATM_STORE_AIM_WNDSTR(
     I                     bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_AIM_WNDSTR
C     | o Routine for saving AIM surface wind-stress fields
C     |   for export to coupling layer.
C     *==========================================================*
C     | This version interfaces to the AIM package.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#ifdef ALLOW_AIM
# include "AIM_SIZE.h"
#else
# include "SIZE.h"
#endif

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_AIM
# include "AIM2DYN.h"
c #include "AIM_FFIELDS.h"
#endif
#ifdef ALLOW_THSICE
# include "THSICE_PARAMS.h"
# include "THSICE_VARS.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
C     ks        :: surface level index
      INTEGER i, j, ks
      _RL cplTimeFraction
      _RL uStr_tmp, vStr_tmp

C     o Accumulate momentum surface flux that will be exported to the
C       coupling layer. Momentum flux is in N/m^2 with same sign as the wind.
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       ks = 1

       tauXTime(bi,bj) = tauXTime(bi,bj) + cplTimeFraction
       tauYTime(bi,bj) = tauYTime(bi,bj) + cplTimeFraction
#ifdef ALLOW_THSICE
      IF ( useThSIce .AND. stressReduction.GT. 0. _d 0 ) THEN
C--   Reduce wind stress applied to ocean where sea-ice is present
       DO j=1,sNy
        DO i=1,sNx
c         IF ( aim_landFr(i-1,j,bi,bj)*aim_landFr(i,j,bi,bj) .NE. 1. ) THEN
          IF ( hFacW(i,j,ks,bi,bj) .NE. 0. ) THEN
           uStr_tmp =
     &      ( aim_drag(i-1,j,bi,bj)
     &       *(1. _d 0 - iceMask(i-1,j,bi,bj)*stressReduction)
     &      + aim_drag( i ,j,bi,bj)
     &       *(1. _d 0 - iceMask( i ,j,bi,bj)*stressReduction)
     &      )* 0.5 _d 0 * uVel(i,j,ks,bi,bj)
           tauX(i,j,bi,bj) = tauX(i,j,bi,bj)
     &                     + uStr_tmp*cplTimeFraction
          ENDIF
        ENDDO
       ENDDO
       DO j=1,sNy
        DO i=1,sNx
c         IF ( aim_landFr(i,j-1,bi,bj)*aim_landFr(i,j,bi,bj) .NE. 1. ) THEN
          IF ( hFacS(i,j,ks,bi,bj) .NE. 0. ) THEN
           vStr_tmp =
     &      ( aim_drag(i,j-1,bi,bj)
     &       *(1. _d 0 - iceMask(i,j-1,bi,bj)*stressReduction)
     &      + aim_drag(i, j ,bi,bj)
     &       *(1. _d 0 - iceMask(i, j ,bi,bj)*stressReduction)
     &      )* 0.5 _d 0 * vVel(i,j,ks,bi,bj)
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
c         IF ( aim_landFr(i-1,j,bi,bj)*aim_landFr(i,j,bi,bj) .NE. 1. ) THEN
          IF ( hFacW(i,j,ks,bi,bj) .NE. 0. ) THEN
           uStr_tmp =
     &      ( aim_drag(i-1,j,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * uVel(i,j,ks,bi,bj)
           tauX(i,j,bi,bj) = tauX(i,j,bi,bj)
     &                     + uStr_tmp*cplTimeFraction
          ENDIF
        ENDDO
       ENDDO
       DO j=1,sNy
        DO i=1,sNx
c         IF ( aim_landFr(i,j-1,bi,bj)*aim_landFr(i,j,bi,bj) .NE. 1. ) THEN
          IF ( hFacS(i,j,ks,bi,bj) .NE. 0. ) THEN
           vStr_tmp =
     &      ( aim_drag(i,j-1,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * vVel(i,j,ks,bi,bj)
           tauY(i,j,bi,bj) = tauY(i,j,bi,bj)
     &                     + vStr_tmp*cplTimeFraction
          ENDIF
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_AIM */

      RETURN
      END
