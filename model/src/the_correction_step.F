C $Header: /u/gcmpack/MITgcm/model/src/Attic/the_correction_step.F,v 1.16 2001/12/11 14:58:46 jmc Exp $
C Tag $Name:  $

#include "CPP_OPTIONS.h"

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
C     |           T(n) = Gt(n-1)
C     |           Gt(n-1) = Gt(n)
C     | Momentum:
C     |           V(n) = Gv(n-1) - dt * grad Eta
C     |           Gv(n-1) = Gv(n)
C     |
C     |part1: update U,V,T,S                                      
C     |  U*,V* (contained in gUnm1,gVnm1) have the surface        
C     |     pressure gradient term added and the result stored    
C     |     in U,V (contained in uVel, vVel)                      
C     |  T* (contained in gTnm1) is copied to T (theta)           
C     |  S* (contained in gSnm1) is copied to S (salt)            
C     |                                                           
C     |part2: Adjustments.                                        
C     |   o Filter  U,V,T,S (Shapiro Filter, Zonal_Filter)        
C     |   o Convective Adjustment                                 
C     |   o Compute again Eta (exact volume conservation)         
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
#ifdef ALLOW_PASSIVE_TRACER
#include "TR1.h"
#endif

#ifdef ALLOW_SHAP_FILT
#include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT   
#include "ZONAL_FILT.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

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

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

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

#ifdef ALLOW_AUTODIFF_TAMC
          kkey = (ikey-1)*Nr + k
#endif

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
#ifdef ALLOW_PASSIVE_TRACER
          IF (tr1Stepping)
     &      CALL CYCLE_TRACER(
     I           bi,bj,iMin,iMax,jMin,jMax,K,
     U           Tr1,gTr1,gTr1Nm1,
     I           myTime,myThid )
#endif

#ifdef    ALLOW_OBCS
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uvel (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vvel (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1  (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */
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
        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy
        CALL CONVECTIVE_ADJUSTMENT(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       myTime, myIter, myThid )

#ifdef EXACT_CONSERV
        IF (exactConserv) THEN
C--     Compute again "eta" to satisfy exactly the total Volume Conservation :
          CALL CALC_EXACT_ETA( .TRUE., bi,bj, uVel,vVel,
     I                         myTime, myIter, myThid )
        ENDIF
#endif /* EXACT_CONSERV */ 

#ifdef ALLOW_TIMEAVE
        IF (taveFreq.GT.0.) THEN 
          CALL TIMEAVE_STATVARS(myTime, myIter, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_TIMEAVE */

C--    End of 2nd bi,bj loop
       ENDDO
      ENDDO

#ifdef EXACT_CONSERV
       IF (exactConserv .AND. implicDiv2Dflow .NE. 0. _d 0) 
     &    _EXCH_XY_R8(etaN, myThid ) 
#endif /* EXACT_CONSERV */ 

      RETURN
      END
