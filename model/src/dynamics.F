C $Header: /u/gcmpack/MITgcm/model/src/dynamics.F,v 1.119 2005/06/27 12:27:19 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DYNAMICS
C     !INTERFACE:
      SUBROUTINE DYNAMICS(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DYNAMICS                                       
C     | o Controlling routine for the explicit part of the model  
C     |   dynamics.                                               
C     *==========================================================*
C     | This routine evaluates the "dynamics" terms for each      
C     | block of ocean in turn. Because the blocks of ocean have  
C     | overlap regions they are independent of one another.      
C     | If terms involving lateral integrals are needed in this   
C     | routine care will be needed. Similarly finite-difference  
C     | operations with stencils wider than the overlap region    
C     | require special consideration.                            
C     | The algorithm...
C     |
C     | "Correction Step"
C     | =================
C     | Here we update the horizontal velocities with the surface
C     | pressure such that the resulting flow is either consistent
C     | with the free-surface evolution or the rigid-lid:
C     |   U[n] = U* + dt x d/dx P
C     |   V[n] = V* + dt x d/dy P
C     |
C     | "Calculation of Gs"
C     | ===================
C     | This is where all the accelerations and tendencies (ie.
C     | physics, parameterizations etc...) are calculated
C     |   rho = rho ( theta[n], salt[n] )
C     |   b   = b(rho, theta)
C     |   K31 = K31 ( rho )
C     |   Gu[n] = Gu( u[n], v[n], wVel, b, ... )
C     |   Gv[n] = Gv( u[n], v[n], wVel, b, ... )
C     |   Gt[n] = Gt( theta[n], u[n], v[n], wVel, K31, ... )
C     |   Gs[n] = Gs( salt[n], u[n], v[n], wVel, K31, ... )
C     |
C     | "Time-stepping" or "Prediction"
C     | ================================
C     | The models variables are stepped forward with the appropriate
C     | time-stepping scheme (currently we use Adams-Bashforth II)
C     | - For momentum, the result is always *only* a "prediction"
C     | in that the flow may be divergent and will be "corrected"
C     | later with a surface pressure gradient.
C     | - Normally for tracers the result is the new field at time
C     | level [n+1} *BUT* in the case of implicit diffusion the result
C     | is also *only* a prediction.
C     | - We denote "predictors" with an asterisk (*).
C     |   U* = U[n] + dt x ( 3/2 Gu[n] - 1/2 Gu[n-1] )
C     |   V* = V[n] + dt x ( 3/2 Gv[n] - 1/2 Gv[n-1] )
C     |   theta[n+1] = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   salt[n+1] = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     | With implicit diffusion:
C     |   theta* = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   salt* = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   (1 + dt * K * d_zz) theta[n] = theta*
C     |   (1 + dt * K * d_zz) salt[n] = salt*
C     |
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_CD_CODE
#include "CD_CODE_VARS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
# include "FFIELDS.h"
# include "EOS.h"
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !CALLING SEQUENCE:
C     DYNAMICS()
C      |
C      |-- CALC_GRAD_PHI_SURF
C      |
C      |-- CALC_VISCOSITY
C      |
C      |-- CALC_PHI_HYD  
C      |
C      |-- MOM_FLUXFORM  
C      |
C      |-- MOM_VECINV    
C      |
C      |-- TIMESTEP      
C      |
C      |-- OBCS_APPLY_UV 
C      |
C      |-- IMPLDIFF      
C      |
C      |-- OBCS_APPLY_UV
C      |
C      |-- CALL DEBUG_STATS_RL

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
C     fVer[UV]               o fVer: Vertical flux term - note fVer
C                                    is "pipelined" in the vertical
C                                    so we need an fVer for each
C                                    variable.
C     phiHydC    :: hydrostatic potential anomaly at cell center
C                   In z coords phiHyd is the hydrostatic potential
C                      (=pressure/rho0) anomaly
C                   In p coords phiHyd is the geopotential height anomaly.
C     phiHydF    :: hydrostatic potential anomaly at middle between 2 centers
C     dPhiHydX,Y :: Gradient (X & Y directions) of hydrostatic potential anom.
C     phiSurfX,  ::  gradient of Surface potential (Pressure/rho, ocean)
C     phiSurfY             or geopotential (atmos) in X and Y direction
C     guDissip   :: dissipation tendency (all explicit terms), u component
C     gvDissip   :: dissipation tendency (all explicit terms), v component
C     iMin, iMax     - Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj
C     k, kup,        - Index for layer above and below. kup and kDown
C     kDown, km1       are switched with layer to be the appropriate 
C                      index into fVerTerm.
      _RL fVerU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL phiHydF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiHydC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL guDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gvDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRU (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)

      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, km1, kp1, kup, kDown

#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

 
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
CEOP

C-- Call to routine for calculation of
C   Eliassen-Palm-flux-forced U-tendency,
C   if desired:
#ifdef INCLUDE_EP_FORCING_CODE
      CALL CALC_EP_FORCING(myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
CHPF$  INDEPENDENT, NEW (fVerU,fVerV
CHPF$&                  ,phiHydF
CHPF$&                  ,KappaRU,KappaRV
CHPF$&                  )
#endif /* ALLOW_AUTODIFF_TAMC */

       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          idynkey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           KappaRU(i,j,k) = 0. _d 0
           KappaRV(i,j,k) = 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
cph(
c--   need some re-initialisation here to break dependencies
cph)
           gu(i,j,k,bi,bj) = 0. _d 0
           gv(i,j,k,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fVerU  (i,j,1) = 0. _d 0
          fVerU  (i,j,2) = 0. _d 0
          fVerV  (i,j,1) = 0. _d 0
          fVerV  (i,j,2) = 0. _d 0
          phiHydF (i,j)  = 0. _d 0 
          phiHydC (i,j)  = 0. _d 0 
          dPhiHydX(i,j)  = 0. _d 0
          dPhiHydY(i,j)  = 0. _d 0 
          phiSurfX(i,j)  = 0. _d 0
          phiSurfY(i,j)  = 0. _d 0
          guDissip(i,j)  = 0. _d 0
          gvDissip(i,j)  = 0. _d 0
         ENDDO
        ENDDO

C--     Start computation of dynamics
        iMin = 0
        iMax = sNx+1
        jMin = 0
        jMax = sNy+1

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wvel (:,:,:,bi,bj) = 
CADJ &     comlev1_bibj, key = idynkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Explicit part of the Surface Potentiel Gradient (add in TIMESTEP)
C       (note: this loop will be replaced by CALL CALC_GRAD_ETA)
        IF (implicSurfPress.NE.1.) THEN
          CALL CALC_GRAD_PHI_SURF(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I         etaN,
     O         phiSurfX,phiSurfY,
     I         myThid )                         
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=idynkey, byte=isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=idynkey, byte=isbyte
#ifdef ALLOW_KPP
CADJ STORE KPPviscAz (:,:,:,bi,bj) 
CADJ &                 = comlev1_bibj, key=idynkey, byte=isbyte
#endif /* ALLOW_KPP */
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  INCLUDE_CALC_DIFFUSIVITY_CALL
C--      Calculate the total vertical diffusivity
        DO k=1,Nr
         CALL CALC_VISCOSITY(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     O        KappaRU,KappaRV,
     I        myThid)
       ENDDO
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRU(:,:,:) 
CADJ &                 = comlev1_bibj, key=idynkey, byte=isbyte
CADJ STORE KappaRV(:,:,:) 
CADJ &                 = comlev1_bibj, key=idynkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Start of dynamics loop
        DO k=1,Nr

C--       km1    Points to level above k (=k-1)
C--       kup    Cycles through 1,2 to point to layer above
C--       kDown  Cycles through 2,1 to point to current layer

          km1  = MAX(1,k-1)
          kp1  = MIN(k+1,Nr)
          kup  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

#ifdef ALLOW_AUTODIFF_TAMC 
         kkey = (idynkey-1)*Nr + k
c
CADJ STORE totphihyd (:,:,k,bi,bj) 
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE theta (:,:,k,bi,bj) 
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt  (:,:,k,bi,bj) 
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--      Integrate hydrostatic balance for phiHyd with BC of 
C        phiHyd(z=0)=0
         CALL CALC_PHI_HYD(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        theta, salt,
     U        phiHydF,
     O        phiHydC, dPhiHydX, dPhiHydY,
     I        myTime, myIter, myThid )

C--      Calculate accelerations in the momentum equations (gU, gV, ...)
C        and step forward storing the result in gU, gV, etc...
         IF ( momStepping ) THEN
#ifdef ALLOW_MOM_FLUXFORM
           IF (.NOT. vectorInvariantMomentum) CALL MOM_FLUXFORM(
     I         bi,bj,iMin,iMax,jMin,jMax,k,kup,kDown,
     I         dPhiHydX,dPhiHydY,KappaRU,KappaRV,
     U         fVerU, fVerV,
     I         myTime, myIter, myThid)
#endif
#ifdef ALLOW_MOM_VECINV
           IF (vectorInvariantMomentum) CALL MOM_VECINV(
     I         bi,bj,iMin,iMax,jMin,jMax,k,kup,kDown,
     I         dPhiHydX,dPhiHydY,KappaRU,KappaRV,
     U         fVerU, fVerV,
     O         guDissip, gvDissip,
     I         myTime, myIter, myThid)
#endif
           CALL TIMESTEP(
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     I         dPhiHydX,dPhiHydY, phiSurfX, phiSurfY,
     I         guDissip, gvDissip,
     I         myTime, myIter, myThid)

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
           IF (useOBCS) THEN
             CALL OBCS_APPLY_UV( bi, bj, k, gU, gV, myThid )
           ENDIF
#endif   /* ALLOW_OBCS */

         ENDIF


C--     end of dynamics k loop (1:Nr)
        ENDDO

C--     Implicit Vertical advection & viscosity
#ifdef INCLUDE_IMPLVERTADV_CODE
        IF ( momImplVertAdv ) THEN
          CALL MOM_U_IMPLICIT_R( kappaRU, 
     I                           bi, bj, myTime, myIter, myThid )
          CALL MOM_V_IMPLICIT_R( kappaRV,
     I                           bi, bj, myTime, myIter, myThid )
        ELSEIF ( implicitViscosity ) THEN
#else /* INCLUDE_IMPLVERTADV_CODE */
        IF     ( implicitViscosity ) THEN
#endif /* INCLUDE_IMPLVERTADV_CODE */
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRU(:,:,:) = comlev1_bibj , key=idynkey, byte=isbyte
CADJ STORE gU(:,:,:,bi,bj) = comlev1_bibj , key=idynkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         0, KappaRU,recip_HFacW,
     U         gU,
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRV(:,:,:) = comlev1_bibj , key=idynkey, byte=isbyte
CADJ STORE gV(:,:,:,bi,bj) = comlev1_bibj , key=idynkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         0, KappaRV,recip_HFacS,
     U         gV,
     I         myThid )
        ENDIF

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
        IF ( useOBCS .AND.(implicitViscosity.OR.momImplVertAdv) ) THEN
           DO K=1,Nr
             CALL OBCS_APPLY_UV( bi, bj, k, gU, gV, myThid )
           ENDDO
        ENDIF
#endif   /* ALLOW_OBCS */

#ifdef    ALLOW_CD_CODE
        IF (implicitViscosity.AND.useCDscheme) THEN
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE vVelD(:,:,:,bi,bj) = comlev1_bibj , key=idynkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         0, KappaRU,recip_HFacW,
     U         vVelD,
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE uVelD(:,:,:,bi,bj) = comlev1_bibj , key=idynkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         0, KappaRV,recip_HFacS,
     U         uVelD,
     I         myThid )
        ENDIF
#endif    /* ALLOW_CD_CODE */
C--     End implicit Vertical advection & viscosity
 
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_PRESCRIBE_EXCHANGES(myThid)
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

Cml(
C     In order to compare the variance of phiHydLow of a p/z-coordinate
C     run with etaH of a z/p-coordinate run the drift of phiHydLow
C     has to be removed by something like the following subroutine:
C      CALL REMOVE_MEAN_RL( 1, phiHydLow, maskH, maskH, rA, drF,
C     &                'phiHydLow', myThid )
Cml)

#ifdef ALLOW_DIAGNOSTICS
      IF ( usediagnostics ) THEN

       CALL DIAGNOSTICS_FILL(totPhihyd,'PHIHYD  ',0,Nr,0,1,1,myThid)

       IF ( DIAGNOSTICS_IS_ON('PHIHYDSQ',myThid) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k = 1,Nr
          DO j = 1,sNy
           DO i = 1,sNx
             tmpFld(i,j) = totPhihyd(i,j,k,bi,bj)*totPhihyd(i,j,k,bi,bj)
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(tmpFld,'PHIHYDSQ',k,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
        ENDDO
       ENDIF

       CALL DIAGNOSTICS_FILL(phiHydLow,'PHIBOT  ',0,1,0,1,1,myThid)

       IF ( DIAGNOSTICS_IS_ON('PHIBOTSQ',myThid) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy
           DO i = 1,sNx
             tmpFld(i,j) = phiHydLow(i,j,bi,bj)*phiHydLow(i,j,bi,bj)
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(tmpFld,'PHIBOTSQ',0,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDIF

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      
#ifdef ALLOW_DEBUG
      If ( debugLevel .GE. debLevB ) THEN
       CALL DEBUG_STATS_RL(1,EtaN,'EtaN (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,uVel,'Uvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,vVel,'Vvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,wVel,'Wvel (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,theta,'Theta (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,salt,'Salt (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gU,'Gu (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gV,'Gv (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gT,'Gt (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gS,'Gs (DYNAMICS)',myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
       CALL DEBUG_STATS_RL(Nr,guNm1,'GuNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gvNm1,'GvNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gtNm1,'GtNm1 (DYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gsNm1,'GsNm1 (DYNAMICS)',myThid)
#endif
      ENDIF
#endif

      RETURN
      END
