C $Header: /u/gcmpack/MITgcm/model/src/dynamics.F,v 1.75 2001/08/03 19:06:11 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE DYNAMICS(myTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE DYNAMICS                                      |
C     | o Controlling routine for the explicit part of the model |
C     |   dynamics.                                              |
C     |==========================================================|
C     | This routine evaluates the "dynamics" terms for each     |
C     | block of ocean in turn. Because the blocks of ocean have |
C     | overlap regions they are independent of one another.     |
C     | If terms involving lateral integrals are needed in this  |
C     | routine care will be needed. Similarly finite-difference |
C     | operations with stencils wider than the overlap region   |
C     | require special consideration.                           |
C     | Notes                                                    |
C     | =====                                                    |
C     | C*P* comments indicating place holders for which code is |
C     |      presently being developed.                          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_PASSIVE_TRACER
#include "TR1.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
# include "FFIELDS.h"
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
#endif

C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables
C     xA, yA                 - Per block temporaries holding face areas
C     uTrans, vTrans, rTrans - Per block temporaries holding flow 
C                              transport
C                              o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o rTrans: Vertical transport
C     maskUp                   o maskUp: land/water mask for W points
C     fVer[STUV]               o fVer: Vertical flux term - note fVer
C                                      is "pipelined" in the vertical
C                                      so we need an fVer for each
C                                      variable.
C     rhoK, rhoKM1   - Density at current level, and level above 
C     phiHyd         - Hydrostatic part of the potential phiHydi.
C                      In z coords phiHydiHyd is the hydrostatic 
C                      Potential (=pressure/rho0) anomaly
C                      In p coords phiHydiHyd is the geopotential 
C                      surface height anomaly.
C     phiSurfX, - gradient of Surface potentiel (Pressure/rho, ocean)
C     phiSurfY             or geopotentiel (atmos) in X and Y direction
C     KappaRT,       - Total diffusion in vertical for T and S.
C     KappaRS          (background + spatially varying, isopycnal term).
C     iMin, iMax     - Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj
C     k, kup,        - Index for layer above and below. kup and kDown
C     kDown, km1       are switched with layer to be the appropriate 
C                      index into fVerTerm.
C     tauAB - Adams-Bashforth timestepping weight: 0=forward ; 1/2=Adams-Bashf.
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerTr1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL phiHyd  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rhokm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhok    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRU (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tauAB

C This is currently used by IVDC and Diagnostics
      _RL ConvectCount (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, km1, kup, kDown

Cjmc : add for phiHyd output <- but not working if multi tile per CPU
c     CHARACTER*(MAX_LEN_MBUF) suff 
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE
Cjmc(end)
 
C---    The algorithm...
C
C       "Correction Step"
C       =================
C       Here we update the horizontal velocities with the surface
C       pressure such that the resulting flow is either consistent
C       with the free-surface evolution or the rigid-lid:
C         U[n] = U* + dt x d/dx P
C         V[n] = V* + dt x d/dy P
C
C       "Calculation of Gs"
C       ===================
C       This is where all the accelerations and tendencies (ie.
C       physics, parameterizations etc...) are calculated
C         rho = rho ( theta[n], salt[n] )
C         b   = b(rho, theta)
C         K31 = K31 ( rho )
C         Gu[n] = Gu( u[n], v[n], wVel, b, ... )
C         Gv[n] = Gv( u[n], v[n], wVel, b, ... )
C         Gt[n] = Gt( theta[n], u[n], v[n], wVel, K31, ... )
C         Gs[n] = Gs( salt[n], u[n], v[n], wVel, K31, ... )
C
C       "Time-stepping" or "Prediction"
C       ================================
C       The models variables are stepped forward with the appropriate
C       time-stepping scheme (currently we use Adams-Bashforth II)
C       - For momentum, the result is always *only* a "prediction"
C       in that the flow may be divergent and will be "corrected"
C       later with a surface pressure gradient.
C       - Normally for tracers the result is the new field at time
C       level [n+1} *BUT* in the case of implicit diffusion the result
C       is also *only* a prediction.
C       - We denote "predictors" with an asterisk (*).
C         U* = U[n] + dt x ( 3/2 Gu[n] - 1/2 Gu[n-1] )
C         V* = V[n] + dt x ( 3/2 Gv[n] - 1/2 Gv[n-1] )
C         theta[n+1] = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         salt[n+1] = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C       With implicit diffusion:
C         theta* = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         salt* = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         (1 + dt * K * d_zz) theta[n] = theta*
C         (1 + dt * K * d_zz) salt[n] = salt*
C---

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
Ccs-

C--     Start computation of dynamics
        iMin = 1-OLx+2
        iMax = sNx+OLx-1
        jMin = 1-OLy+2
        jMax = sNy+OLy-1

C--     Explicit part of the Surface Potentiel Gradient (add in TIMESTEP)
C       (note: this loop will be replaced by CALL CALC_GRAD_ETA)
        IF (implicSurfPress.NE.1.) THEN
          CALL CALC_GRAD_PHI_SURF(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I         etaN,
     O         phiSurfX,phiSurfY,
     I         myThid )                         
        ENDIF

C--     Start of dynamics loop
        DO k=1,Nr

C--       km1    Points to level above k (=k-1)
C--       kup    Cycles through 1,2 to point to layer above
C--       kDown  Cycles through 2,1 to point to current layer

          km1  = MAX(1,k-1)
          kup  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

C--      Integrate hydrostatic balance for phiHyd with BC of 
C        phiHyd(z=0)=0
C        distinguishe between Stagger and Non Stagger time stepping
         IF (staggerTimeStep) THEN
           CALL CALC_PHI_HYD(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        gTnm1, gSnm1,
     U        phiHyd,
     I        myThid )
         ELSE
           CALL CALC_PHI_HYD(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        theta, salt,
     U        phiHyd,
     I        myThid )
         ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRT(:,:,k)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE KappaRS(:,:,k)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  INCLUDE_CALC_DIFFUSIVITY_CALL
C--      Calculate the total vertical diffusivity
         CALL CALC_DIFFUSIVITY(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        maskUp,
     O        KappaRT,KappaRS,KappaRU,KappaRV,
     I        myThid)
#endif

C--      Calculate accelerations in the momentum equations (gU, gV, ...)
C        and step forward storing the result in gUnm1, gVnm1, etc...
         IF ( momStepping ) THEN
           CALL CALC_MOM_RHS(
     I         bi,bj,iMin,iMax,jMin,jMax,k,kup,kDown,
     I         phiHyd,KappaRU,KappaRV,
     U         fVerU, fVerV,
     I         myTime, myThid)
           CALL TIMESTEP(
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     I         phiHyd, phiSurfX, phiSurfY,
     I         myIter, myThid)

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           CALL OBCS_APPLY_UV( bi, bj, k, gUnm1, gVnm1, myThid )
         END IF
#endif   /* ALLOW_OBCS */

#ifdef   ALLOW_AUTODIFF_TAMC
#ifdef   INCLUDE_CD_CODE
         ELSE
           DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               guCD(i,j,k,bi,bj) = 0.0
               gvCD(i,j,k,bi,bj) = 0.0
             END DO
           END DO
#endif   /* INCLUDE_CD_CODE */
#endif   /* ALLOW_AUTODIFF_TAMC */
         ENDIF


C--     end of dynamics k loop (1:Nr)
        ENDDO



C--     Implicit viscosity
        IF (implicitViscosity.AND.momStepping) THEN
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 3
CADJ STORE gUNm1(:,:,:,bi,bj) = comlev1_bibj , key=ikey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRU,recip_HFacW,
     U         gUNm1,
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 4
CADJ STORE gVNm1(:,:,:,bi,bj) = comlev1_bibj , key=ikey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRV,recip_HFacS,
     U         gVNm1,
     I         myThid )

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           DO K=1,Nr
             CALL OBCS_APPLY_UV( bi, bj, k, gUnm1, gVnm1, myThid )
           ENDDO
         END IF
#endif   /* ALLOW_OBCS */

#ifdef    INCLUDE_CD_CODE
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 5
CADJ STORE vVelD(:,:,:,bi,bj) = comlev1_bibj , key=ikey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRU,recip_HFacW,
     U         vVelD,
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 6
CADJ STORE uVelD(:,:,:,bi,bj) = comlev1_bibj , key=ikey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRV,recip_HFacS,
     U         uVelD,
     I         myThid )
#endif    /* INCLUDE_CD_CODE */
C--     End If implicitViscosity.AND.momStepping
        ENDIF
 
Cjmc : add for phiHyd output <- but not working if multi tile per CPU
c       IF ( DIFFERENT_MULTIPLE(dumpFreq,myTime+deltaTClock,myTime) 
c    &  .AND. buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
c         WRITE(suff,'(I10.10)') myIter+1
c         CALL WRITE_FLD_XYZ_RL('PH.',suff,phiHyd,myIter+1,myThid) 
c       ENDIF
Cjmc(end)

#ifdef ALLOW_TIMEAVE
        IF (taveFreq.GT.0.) THEN 
          CALL TIMEAVE_CUMUL_1T(phiHydtave, phiHyd, Nr,
     I                              deltaTclock, bi, bj, myThid)
          IF (ivdc_kappa.NE.0.) THEN
            CALL TIMEAVE_CUMULATE(ConvectCountTave, ConvectCount, Nr,
     I                              deltaTclock, bi, bj, myThid)
          ENDIF
        ENDIF
#endif /* ALLOW_TIMEAVE */

       ENDDO
      ENDDO

#ifndef EXCLUDE_DEBUGMODE
      If (debugMode) THEN
       CALL DEBUG_STATS_RL(1,EtaN,'EtaN (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,uVel,'Uvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,vVel,'Vvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,wVel,'Wvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,theta,'Theta (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,salt,'Salt (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,Gu,'Gu (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,Gv,'Gv (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,Gt,'Gt (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,Gs,'Gs (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,GuNm1,'GuNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,GvNm1,'GvNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,GtNm1,'GtNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,GsNm1,'GsNm1 (DYNAMICS)',myThid)
      ENDIF
#endif

      RETURN
      END
