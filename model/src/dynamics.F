C $Header: /u/gcmpack/MITgcm/model/src/dynamics.F,v 1.8 1998/05/25 20:05:55 cnh Exp $

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
C                              o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o wTrans: Vertical transport
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
      _RL pSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL K13   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL K23   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL K33   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RL KapGM (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, kM1, kUp, kDown

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
        ENDDO
        rhokm1(i,j)  = 0. _d 0
        rhokp1(i,j)  = 0. _d 0
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--     Boundary condition on hydrostatic pressure is pH(z=0)=0
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          pH(i,j,1) = 0. _d 0
          K13(i,j,1) = 0. _d 0
          K23(i,j,1) = 0. _d 0
          K33(i,j,1) = 0. _d 0
          KapGM(i,j) = 0. _d 0
         ENDDO
        ENDDO

C--     Set up work arrays that need valid initial values
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          wTrans(i,j)  = 0. _d 0
          fVerT(i,j,1) = 0. _d 0
          fVerT(i,j,2) = 0. _d 0
          fVerS(i,j,1) = 0. _d 0
          fVerS(i,j,2) = 0. _d 0
          fVerU(i,j,1) = 0. _d 0
          fVerU(i,j,2) = 0. _d 0
          fVerV(i,j,1) = 0. _d 0
          fVerV(i,j,2) = 0. _d 0
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
        CALL TIMESTEP(
     I       bi,bj,iMin,iMax,jMin,jMax,1,pSurfX,pSurfY,myThid)

C--     Density of 1st level (below W(1)) reference to level 1
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax, 1, 1, 'LINEAR',
     O     rhoKm1,
     I     myThid )
C--     Integrate hydrostatic balance for pH with BC of pH(z=0)=0
        CALL CALC_PH(
     I      bi,bj,iMin,iMax,jMin,jMax,1,rhoKm1,rhoKm1,
     U      pH,
     I      myThid )

        DO K=2,Nz
C--     Update fields in Kth level according to tendency terms
        CALL TIMESTEP(
     I       bi,bj,iMin,iMax,jMin,jMax,K,pSurfX,pSurfY,myThid)
C--     Density of K-1 level (above W(K)) reference to K level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K-1, K, 'LINEAR',
     O     rhoKm1,
     I     myThid )
C--     Density of K level (below W(K)) reference to K level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K, K, 'LINEAR',
     O     rhoKp1,
     I     myThid )
C--     Calculate iso-neutral slopes for the GM/Redi parameterisation
        CALL CALC_ISOSLOPES(
     I            bi, bj, iMin, iMax, jMin, jMax, K,
     I            rhoKm1, rhoKp1,
     O            K13, K23, K33, KapGM,
     I            myThid )
C--     Calculate static stability and mix where convectively unstable
        CALL CONVECT(
     I      bi,bj,iMin,iMax,jMin,jMax,K,rhoKm1,rhoKp1,
     I      myTime,myIter,myThid)
C--     Density of K-1 level (above W(K)) reference to K-1 level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K-1, K-1, 'LINEAR',
     O     rhoKm1,
     I     myThid )
C--     Density of K level (below W(K)) referenced to K level
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax,  K, K, 'LINEAR',
     O     rhoKp1,
     I     myThid )
C--     Integrate hydrostatic balance for pH with BC of pH(z=0)=0
        CALL CALC_PH(
     I      bi,bj,iMin,iMax,jMin,jMax,K,rhoKm1,rhoKp1,
     U      pH,
     I      myThid )

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
     O        xA,yA,uTrans,vTrans,wTrans,maskC,maskUp,
     I        myThid)

C--      Calculate accelerations in the momentum equations
         CALL CALC_MOM_RHS(
     I        bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I        xA,yA,uTrans,vTrans,wTrans,maskC,
     I        pH,
     U        aTerm,xTerm,cTerm,mTerm,pTerm,
     U        fZon, fMer, fVerU, fVerV,
     I        myThid)

C--      Calculate active tracer tendencies
         CALL CALC_GT(
     I        bi,bj,iMin,iMax,jMin,jMax, k,kM1,kUp,kDown,
     I        xA,yA,uTrans,vTrans,wTrans,maskUp,
     I        K13,K23,K33,KapGM,
     U        aTerm,xTerm,fZon,fMer,fVerT,
     I        myThid)
Cdbg     CALL CALC_GS(
Cdbg I        bi,bj,iMin,iMax,jMin,jMax, k,kM1,kUp,kDown,
Cdbg I        xA,yA,uTrans,vTrans,wTrans,maskUp,
Cdbg I        K13,K23,K33,KapGM,
Cdbg U        aTerm,xTerm,fZon,fMer,fVerS,
Cdbg I        myThid)

        ENDDO
 
       ENDDO
      ENDDO

!dbg  write(0,*) 'dynamics: pS',minval(cg2d_x),maxval(cg2d_x)
!dbg  write(0,*) 'dynamics: U',minval(uVel(1:sNx,1:sNy,:,:,:)),
!dbg &                         maxval(uVel(1:sNx,1:sNy,:,:,:))
!dbg  write(0,*) 'dynamics: V',minval(vVel(1:sNx,1:sNy,:,:,:)),
!dbg &                         maxval(vVel(1:sNx,1:sNy,:,:,:))
!dbg  write(0,*) 'dynamics: gT',minval(gT(1:sNx,1:sNy,:,:,:)),
!dbg &                         maxval(gT(1:sNx,1:sNy,:,:,:))
!dbg  write(0,*) 'dynamics: T',minval(Theta(1:sNx,1:sNy,:,:,:)),
!dbg &                         maxval(Theta(1:sNx,1:sNy,:,:,:))
!dbg  write(0,*) 'dynamics: pH',minval(pH/(Gravity*Rhonil)),
!dbg &                          maxval(pH/(Gravity*Rhonil))

      RETURN
      END
