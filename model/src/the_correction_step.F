C $Header: /u/gcmpack/MITgcm/model/src/Attic/the_correction_step.F,v 1.27 2004/11/23 20:20:19 mlosch Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_GCHEM
#include "GCHEM_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THE_CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE THE_CORRECTION_STEP(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THE_CORRECTION_STEP                            
C     *==========================================================*
C     |1rst Part : Update U,V,T,S.
C     |
C     | The arrays used for time stepping are cycled.
C     | Tracers:
C     |           T(n) = Gt(n)
C     | Momentum:
C     |           V(n) = Gv(n) - dt * grad Eta
C     |
C     |part1: update U,V,T,S                                      
C     |  U*,V* (contained in gU,gV) have the surface        
C     |     pressure gradient term added and the result stored    
C     |     in U,V (contained in uVel, vVel)                      
C     |  T* (contained in gT) is copied to T (theta)           
C     |  S* (contained in gS) is copied to S (salt)            
C     |                                                           
C     |part2: Adjustments & Diagnostics                                        
C     |   o Filter  U,V,T,S (Shapiro Filter, Zonal_Filter)        
C     |   o Convective Adjustment                                 
C     |   o Compute again Eta (exact volume conservation)     
C     |   o Compute vertical velocity
C     |   o Diagmnostic of state variables (Time average)         
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

C-        Update tracer fields:  T(n) = T**, Gt(n-1) = Gt(n)
          IF (tempStepping)
     &      CALL CYCLE_TRACER(
     I           bi,bj,iMin,iMax,jMin,jMax,K,
     U           theta,gT,gTNm1,
     I           myTime,myThid )
          IF (saltStepping)
     &      CALL CYCLE_TRACER(
     I           bi,bj,iMin,iMax,jMin,jMax,K,
     U           salt,gS,gSNm1,
     I           myTime,myThid )
#ifdef ALLOW_PTRACERS
C-        Update passive tracer fields:  T(n) = T**, Gt(n-1) = Gt(n)
          IF (usePTRACERS)
     &        CALL PTRACERS_CYCLE(bi,bj,k,myIter,myTime,myThid)
#endif /* ALLOW_PTRACERS */


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
C
C       Static stability is calculated and the tracers are
C       convective adjusted where statically unstable.

C--   Filter (and exchange)
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT) THEN
       IF ( .NOT.shap_filt_uvStar )
     &  CALL SHAP_FILT_APPLY_UV( uVel, vVel, myTime, myIter, myThid )

       IF ( .NOT.(staggerTimeStep .AND. shap_filt_TrStagg) )
     &  CALL SHAP_FILT_APPLY_TS( theta,salt, myTime, myIter, myThid )
      ENDIF
#endif 
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT) THEN
       IF ( .NOT.zonal_filt_uvStar )
     &  CALL ZONAL_FILT_APPLY_UV( uVel, vVel, myThid )

       IF ( .NOT.(staggerTimeStep .AND. zonal_filt_TrStagg) )
     &  CALL ZONAL_FILT_APPLY_TS( theta, salt, myThid )  

      ENDIF
#endif 

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Convectively adjust new fields to be statically stable
        iMin = 1
        iMax = sNx
        jMin = 1
        jMax = sNy
#ifdef ALLOW_OPPS
        IF ( useOPPS ) THEN
         CALL OPPS_INTERFACE(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime, myIter, myThid )
        ELSE
#endif /* ALLOW_OPPS */
         CALL CONVECTIVE_ADJUSTMENT(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime, myIter, myThid )
#ifdef ALLOW_OPPS
        ENDIF
#endif /* ALLOW_OPPS */

C--     Integrate continuity vertically
C--     for vertical velocity and "etaN" (exact volume conservation) :
        CALL INTEGR_CONTINUITY( bi, bj, uVel, vVel,
     I                          myTime, myIter, myThid )

#ifdef ALLOW_TIMEAVE
        IF (taveFreq.GT.0.) THEN 
          CALL TIMEAVE_STATVARS(myTime, myIter, bi, bj, myThid)
#ifdef ALLOW_PTRACERS
          IF ( usePTRACERS ) THEN 
           CALL PTRACERS_STATVARS(myTime, myIter, bi, bj, myThid)
          ENDIF
#endif /* ALLOW_PTRACERS */
         ENDIF
#endif /* ALLOW_TIMEAVE */

C--    End of 2nd bi,bj loop
       ENDDO
      ENDDO

      RETURN
      END
