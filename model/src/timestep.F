C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.30 2003/02/09 02:00:50 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP
C     !INTERFACE:
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax, K,
     I             phiHyd, dPhiHydX,dPhiHydY, phiSurfX, phiSurfY,
     I                     myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TIMESTEP                                              
C     | o Step model fields forward in time                       
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     phiHyd     :: Hydrostatic Potential (ocean: pressure/rho) 
C                                         (atmos: geopotential)
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hydrostatic Potential
C     phiSurfX :: gradient of Surface potential (Pressure/rho, ocean)
C     phiSurfY ::          or geopotential (atmos) in X and Y direction
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RL     phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr) 
      _RL     dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05
      _RL phxFac,phyFac, psFac
      _RL     gUtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gVtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C     Adams-Bashforth timestepping weights
      IF (myIter .EQ. 0) THEN
       ab15=1.0
       ab05=0.0
      ELSE
       ab15=1.5+abeps
       ab05=-0.5-abeps
      ENDIF

C-- stagger time step: grad Phi_Hyp is not in gU,gV => add it in this S/R
      IF (staggerTimeStep) THEN
c     IF (.FALSE.) THEN
        phxFac = pfFacMom
        phyFac = pfFacMom
      ELSE
        phxFac = 0.
        phyFac = 0.
      ENDIF 

C-- explicit part of the surface potential gradient is added in this S/R
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Compute effective gU term (including Adams-Bashforth weights) :
      DO j=jMin,jMax
       DO i=iMin,iMax
        gUtmp(i,j) = ab15*gU(i,j,k,bi,bj)
     &             + ab05*gUNm1(i,j,k,bi,bj)  
#ifdef INCLUDE_CD_CODE
     &             + guCD(i,j,k,bi,bj)
#endif
       ENDDO
      ENDDO
      
#ifdef NONLIN_FRSURF
      IF (.NOT. vectorInvariantMomentum 
     &    .AND. nonlinFreeSurf.GT.1) THEN
       IF (select_rStar.GT.0) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gUtmp(i,j)/rStarExpW(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( k.EQ.ksurfW(i,j,bi,bj) ) THEN
           gUtmp(i,j) = gUtmp(i,j)
     &         *hFacW(i,j,k,bi,bj)/hFac_surfW(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif 

C     Step forward zonal velocity (store in Gu)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gUNm1(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) 
     &     +deltaTmom*( 
     &         gUtmp(i,j)
     &       - psFac*phiSurfX(i,j)
     &       - phxFac*dPhiHydX(i,j)
     &        )*_maskW(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (.FALSE.) THEN
c     IF (staggerTimeStep) THEN
C--   -grad Phi_Hyd has not been incorporated to gU and is added here:
        phxFac = pfFacMom*deltaTmom
        DO j=jMin,jMax
          DO i=iMin,iMax
            gUNm1(i,j,k,bi,bj)=gUNm1(i,j,k,bi,bj)
     &       - _recip_dxC(i,j,bi,bj)
     &         *(phiHyd(i,j,k)-phiHyd(i-1,j,k))*phxFac
     &         *_maskW(i,j,k,bi,bj)
          ENDDO
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Compute effective gV term (including Adams-Bashforth weights) :
      DO j=jMin,jMax
       DO i=iMin,iMax
        gVtmp(i,j) = ab15*gV(i,j,k,bi,bj)
     &             + ab05*gVNm1(i,j,k,bi,bj)  
#ifdef INCLUDE_CD_CODE
     &             + gvCD(i,j,k,bi,bj)
#endif
       ENDDO
      ENDDO
      
#ifdef NONLIN_FRSURF
      IF (.NOT. vectorInvariantMomentum 
     &    .AND. nonlinFreeSurf.GT.1) THEN
       IF (select_rStar.GT.0) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
           gVtmp(i,j) = gVtmp(i,j)/rStarExpS(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( k.EQ.ksurfS(i,j,bi,bj) ) THEN
           gVtmp(i,j) = gVtmp(i,j)
     &         *hFacS(i,j,k,bi,bj)/hFac_surfS(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif 

C     Step forward meridional velocity (store in Gv)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gVNm1(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &     +deltaTmom*(
     &         gVtmp(i,j)
     &       - psFac*phiSurfY(i,j)
     &       - phyFac*dPhiHydY(i,j)
     &        )*_maskS(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (.FALSE.) THEN
c     IF (staggerTimeStep) THEN
C--   -grad Phi_Hyd has not been incorporated to gV and is added here:
        phyFac = pfFacMom*deltaTmom
        DO j=jMin,jMax
          DO i=iMin,iMax
            gVNm1(i,j,k,bi,bj)=gVNm1(i,j,k,bi,bj)
     &       - _recip_dyC(i,j,bi,bj)
     &         *(phiHyd(i,j,k)-phiHyd(i,j-1,k))*phyFac
     &         *_maskS(i,j,k,bi,bj)
          ENDDO
        ENDDO
      ENDIF

      RETURN
      END
