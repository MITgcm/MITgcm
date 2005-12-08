C $Header: /u/gcmpack/MITgcm/model/src/momentum_correction_step.F,v 1.3 2005/12/08 15:44:34 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: MOMENTUM_CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE MOMENTUM_CORRECTION_STEP(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MOMENTUM_CORRECTION_STEP
C     *==========================================================*
C     |1rst Part : Update U,V.
C     |
C     | The arrays used for time stepping are cycled.
C     | Momentum:
C     |           V(n) = Gv(n) - dt * grad Eta
C     |
C     |part1: update U,V
C     |  U*,V* (contained in gU,gV) have the surface        
C     |     pressure gradient term added and the result stored    
C     |     in U,V (contained in uVel, vVel)                      
C     |                                                           
C     |part2: Adjustments
C     |   o Filter  U,V (Shapiro Filter, Zonal_Filter)        
C     |   o Compute again Eta (exact volume conservation)     
C     |   o Compute vertical velocity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

#ifdef ALLOW_SHAP_FILT
#include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT
#include "ZONAL_FILT.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax
      INTEGER jMin,jMax
      INTEGER bi,bj
      INTEGER k,i,j

CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Set up work arrays that need valid initial values
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          phiSurfX(i,j)=0.
          phiSurfY(i,j)=0.
         ENDDO
        ENDDO

C       Loop range: Gradients of Eta are evaluated so valid
C       range is all but first row and column in overlaps.
        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy

C-      Calculate gradient of surface Potentiel
        CALL CALC_GRAD_PHI_SURF(
     I       bi,bj,iMin,iMax,jMin,jMax,
     I       etaN,
     O       phiSurfX,phiSurfY,
     I       myThid )

C--     Loop over all layers, top to bottom
        DO K=1,Nr

C-        Update velocity fields:  V(n) = V** - dt * grad Eta
          IF (momStepping)
     &      CALL CORRECTION_STEP(
     I         bi,bj,iMin,iMax,jMin,jMax,K,
     I         phiSurfX,phiSurfY,myTime,myThid )

#ifdef    ALLOW_OBCS
          IF (useOBCS) THEN
            CALL OBCS_APPLY_UV(bi,bj,K,uVel,vVel,myThid)
          ENDIF
#endif    /* ALLOW_OBCS */

C--     End DO K=1,Nr
        ENDDO

C--    End of 1rst bi,bj loop
       ENDDO
      ENDDO

C--- 2nd Part : Adjustment.

C--   Filter (and exchange)
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT) THEN
       IF ( .NOT.shap_filt_uvStar ) THEN
        CALL TIMER_START('SHAP_FILT_UV       [MOM_CORR_STEP]',myThid)
        CALL SHAP_FILT_APPLY_UV( uVel, vVel, myTime, myIter, myThid )
        CALL TIMER_STOP ('SHAP_FILT_UV       [MOM_CORR_STEP]',myThid)
       ENDIF
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT) THEN
       IF ( .NOT.zonal_filt_uvStar ) THEN
        CALL TIMER_START('ZONAL_FILT_UV      [MOM_CORR_STEP]',myThid)
        CALL ZONAL_FILT_APPLY_UV( uVel, vVel, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_UV      [MOM_CORR_STEP]',myThid)
       ENDIF
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
cph-test
CADJ STORE uvel, vvel  = comlev1, key = ikey_dynamics
# endif
#endif
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Integrate continuity vertically
C--     for vertical velocity and "etaN" (exact volume conservation) :
        CALL INTEGR_CONTINUITY( bi, bj, uVel, vVel,
     I                          myTime, myIter, myThid )

C--    End of 2nd bi,bj loop
       ENDDO
      ENDDO

      IF ( exactConserv .AND. implicDiv2Dflow .NE. 0. _d 0)
     & _EXCH_XY_R8( etaN , myThid )

      RETURN
      END
