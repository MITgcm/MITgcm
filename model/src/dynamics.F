C $Header: /u/gcmpack/MITgcm/model/src/dynamics.F,v 1.15 1998/06/09 15:58:36 adcroft Exp $

#include "CPP_EEOPTIONS.h"

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

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CG2D.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
      _RL myTime
      INTEGER myIter

C     == Local variables
C     xA, yA                 - Per block temporaries holding face areas
C     uTrans, vTrans, wTrans - Per block temporaries holding flow transport
C     wVel                     o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o wTrans: Vertical transport
C                              o wVel:   Vertical velocity at upper and lower
C                                        cell faces.
C     maskC,maskUp             o maskC: land/water mask for tracer cells
C                              o maskUp: land/water mask for W points
C     aTerm, xTerm, cTerm    - Work arrays for holding separate terms in
C     mTerm, pTerm,            tendency equations.
C     fZon, fMer, fVer[STUV]   o aTerm: Advection term
C                              o xTerm: Mixing term
C                              o cTerm: Coriolis term
C                              o mTerm: Metric term
C                              o pTerm: Pressure term
C                              o fZon: Zonal flux term
C                              o fMer: Meridional flux term
C                              o fVer: Vertical flux term - note fVer
C                                      is "pipelined" in the vertical
C                                      so we need an fVer for each
C                                      variable.
C     iMin, iMax - Ranges and sub-block indices on which calculations
C     jMin, jMax   are applied.
C     bi, bj
C     k, kUp, kDown, kM1 - Index for layer above and below. kUp and kDown
C                          are switched with layer to be the appropriate index
C                          into fVerTerm
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL aTerm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL xTerm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cTerm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mTerm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pTerm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL pH    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL rhokm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhokp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhotmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL K13   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL K23   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL K33   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL KapGM (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaZT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)

      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, kM1, kUp, kDown

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
C         w = sum_z ( div. u[n] )
C         rho = rho ( theta[n], salt[n] )
C         K31 = K31 ( rho )
C         Gu[n] = Gu( u[n], v[n], w, rho, Ph, ... )
C         Gv[n] = Gv( u[n], v[n], w, rho, Ph, ... )
C         Gt[n] = Gt( theta[n], u[n], v[n], w, K31, ... )
C         Gs[n] = Gs( salt[n], u[n], v[n], w, K31, ... )
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

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        xA(i,j)      = 0. _d 0
        yA(i,j)      = 0. _d 0
        uTrans(i,j)  = 0. _d 0
        vTrans(i,j)  = 0. _d 0
        aTerm(i,j)   = 0. _d 0
        xTerm(i,j)   = 0. _d 0
        cTerm(i,j)   = 0. _d 0
        mTerm(i,j)   = 0. _d 0
        pTerm(i,j)   = 0. _d 0
        fZon(i,j)    = 0. _d 0
        fMer(i,j)    = 0. _d 0
        DO K=1,nZ
         pH (i,j,k)  = 0. _d 0
         K13(i,j,k) = 0. _d 0
         K23(i,j,k) = 0. _d 0
         K33(i,j,k) = 0. _d 0
         KappaZT(i,j,k) = 0. _d 0
        ENDDO
        rhokm1(i,j)  = 0. _d 0
        rhokp1(i,j)  = 0. _d 0
        rhotmp(i,j)  = 0. _d 0
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Set up work arrays that need valid initial values
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          wTrans(i,j)  = 0. _d 0
          wVel  (i,j,1) = 0. _d 0
          wVel  (i,j,2) = 0. _d 0
          fVerT(i,j,1) = 0. _d 0
          fVerT(i,j,2) = 0. _d 0
          fVerS(i,j,1) = 0. _d 0
          fVerS(i,j,2) = 0. _d 0
          fVerU(i,j,1) = 0. _d 0
          fVerU(i,j,2) = 0. _d 0
          fVerV(i,j,1) = 0. _d 0
          fVerV(i,j,2) = 0. _d 0
          pH(i,j,1) = 0. _d 0
          K13(i,j,1) = 0. _d 0
          K23(i,j,1) = 0. _d 0
          K33(i,j,1) = 0. _d 0
          KapGM(i,j) = 0. _d 0
         ENDDO
        ENDDO

        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy

C--     Calculate gradient of surface pressure
        CALL GRAD_PSURF(
     I       bi,bj,iMin,iMax,jMin,jMax,
     O       pSurfX,pSurfY,
     I       myThid)

C--     Update fields in top level according to tendency terms
        CALL CORRECTION_STEP(
     I       bi,bj,iMin,iMax,jMin,jMax,1,pSurfX,pSurfY,myThid)

C--     Density of 1st level (below W(1)) reference to level 1
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax, 1, 1, eosType,
     O     rhoKm1,
     I     myThid )
C--     Integrate hydrostatic balance for pH with BC of pH(z=0)=0
        CALL CALC_PH(
     I      bi,bj,iMin,iMax,jMin,jMax,1,rhoKm1,rhoKm1,
     U      pH,
     I      myThid )
        DO J=jMin,jMax
         DO I=iMin,iMax
          rhoKp1(I,J)=rhoKm1(I,J)
         ENDDO
        ENDDO

        DO K=2,Nz
C--     Update fields in Kth level according to tendency terms
        CALL CORRECTION_STEP(
     I       bi,bj,iMin,iMax,jMin,jMax,K,pSurfX,pSurfY,myThid)
C--     Density of K-1 level (above W(K)) reference to K-1 level
copt    CALL FIND_RHO(
copt I     bi, bj, iMin, iMax, jMin, jMax,  K-1, K-1, eosType,
copt O     rhoKm1,
copt I     myThid )
C       rhoKm1=rhoKp1
        DO J=jMin,jMax
         DO I=iMin,iMax
          rhoKm1(I,J)=rhoKp1(I,J)
         ENDDO
        ENDDO
C--     Density of K level (below W(K)) reference to K level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K, K, eosType,
     O     rhoKp1,
     I     myThid )
C--     Density of K-1 level (above W(K)) reference to K level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K-1, K, eosType,
     O     rhotmp,
     I     myThid )
C--     Calculate iso-neutral slopes for the GM/Redi parameterisation
        CALL CALC_ISOSLOPES(
     I            bi, bj, iMin, iMax, jMin, jMax, K,
     I            rhoKm1, rhoKp1, rhotmp,
     O            K13, K23, K33, KapGM,
     I            myThid )
C--     Calculate static stability and mix where convectively unstable
        CALL CONVECT(
     I      bi,bj,iMin,iMax,jMin,jMax,K,rhotmp,rhoKp1,
     I      myTime,myIter,myThid)
C--     Density of K-1 level (above W(K)) reference to K-1 level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K-1, K-1, eosType,
     O     rhoKm1,
     I     myThid )
C--     Density of K level (below W(K)) referenced to K level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K, K, eosType,
     O     rhoKp1,
     I     myThid )
C--     Integrate hydrostatic balance for pH with BC of pH(z=0)=0
        CALL CALC_PH(
     I      bi,bj,iMin,iMax,jMin,jMax,K,rhoKm1,rhoKp1,
     U      pH,
     I      myThid )

        ENDDO ! K

C--     Initial boundary condition on barotropic divergence integral
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          cg2d_b(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

        DO K = Nz, 1, -1
         kM1  =max(1,k-1)   ! Points to level above k (=k-1)
         kUp  =1+MOD(k+1,2) ! Cycles through 1,2 to point to layer above
         kDown=1+MOD(k,2)   ! Cycles through 2,1 to point to current layer
         iMin = 1-OLx+2
         iMax = sNx+OLx-1
         jMin = 1-OLy+2
         jMax = sNy+OLy-1

C--      Get temporary terms used by tendency routines
         CALL CALC_COMMON_FACTORS (
     I        bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     O        xA,yA,uTrans,vTrans,wTrans,wVel,maskC,maskUp,
     I        myThid)

C--      Calculate the total vertical diffusivity
         CALL CALC_DIFFUSIVITY(
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskC,maskUp,KapGM,K33,
     O        KappaZT,
     I        myThid)

C--      Calculate accelerations in the momentum equations
         IF ( momStepping ) THEN
          CALL CALC_MOM_RHS(
     I         bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I         xA,yA,uTrans,vTrans,wTrans,wVel,maskC,
     I         pH,
     U         aTerm,xTerm,cTerm,mTerm,pTerm,
     U         fZon, fMer, fVerU, fVerV,
     I         myThid)
         ENDIF

C--      Calculate active tracer tendencies
         IF ( tempStepping ) THEN
          CALL CALC_GT(
     I         bi,bj,iMin,iMax,jMin,jMax, k,kM1,kUp,kDown,
     I         xA,yA,uTrans,vTrans,wTrans,maskUp,
     I         K13,K23,KappaZT,KapGM,
     U         aTerm,xTerm,fZon,fMer,fVerT,
     I         myThid)
         ENDIF
Cdbg     CALL CALC_GS(
Cdbg I        bi,bj,iMin,iMax,jMin,jMax, k,kM1,kUp,kDown,
Cdbg I        xA,yA,uTrans,vTrans,wTrans,maskUp,
Cdbg I        K13,K23,K33,KapGM,
Cdbg U        aTerm,xTerm,fZon,fMer,fVerS,
Cdbg I        myThid)

C--      Prediction step (step forward all model variables)
         CALL TIMESTEP(
     I       bi,bj,iMin,iMax,jMin,jMax,K,
     I       myThid)

C--      Diagnose barotropic divergence of predicted fields
         CALL DIV_G(
     I       bi,bj,iMin,iMax,jMin,jMax,K,
     I       xA,yA,
     I       myThid)

        ENDDO ! K

C--     Implicit diffusion
        IF (implicitDiffusion) THEN
         CALL IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                  KappaZT,
     I                  myThid )
        ENDIF
 
       ENDDO
      ENDDO

      write(0,*) 'dynamics: pS ',minval(cg2d_x(1:sNx,1:sNy,:,:)),
     &                           maxval(cg2d_x(1:sNx,1:sNy,:,:))
      write(0,*) 'dynamics: U  ',minval(uVel(1:sNx,1:sNy,:,:,:)),
     &                           maxval(uVel(1:sNx,1:sNy,:,:,:))
      write(0,*) 'dynamics: V  ',minval(vVel(1:sNx,1:sNy,:,:,:)),
     &                           maxval(vVel(1:sNx,1:sNy,:,:,:))
cblk  write(0,*) 'dynamics: K13',minval(K13(1:sNx,1:sNy,:)),
cblk &                           maxval(K13(1:sNx,1:sNy,:))
cblk  write(0,*) 'dynamics: K23',minval(K23(1:sNx,1:sNy,:)),
cblk &                           maxval(K23(1:sNx,1:sNy,:))
cblk  write(0,*) 'dynamics: K33',minval(K33(1:sNx,1:sNy,:)),
cblk &                           maxval(K33(1:sNx,1:sNy,:))
      write(0,*) 'dynamics: gT ',minval(gT(1:sNx,1:sNy,:,:,:)),
     &                           maxval(gT(1:sNx,1:sNy,:,:,:))
      write(0,*) 'dynamics: T  ',minval(Theta(1:sNx,1:sNy,:,:,:)),
     &                           maxval(Theta(1:sNx,1:sNy,:,:,:))
cblk  write(0,*) 'dynamics: pH ',minval(pH/(Gravity*Rhonil)),
cblk &                           maxval(pH/(Gravity*Rhonil))

      RETURN
      END
