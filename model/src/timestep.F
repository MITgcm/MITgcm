C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.27 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP
C     !INTERFACE:
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax, K,
     I                     phiHyd, phiSurfX, phiSurfY,
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
C     phiHyd    - Hydrostatic Potential (ocean: pressure/rho) 
C                                       (atmos: geopotentiel)
C     phiSurfX, - gradient of Surface potentiel (Pressure/rho, ocean)
C     phiSurfY             or geopotentiel (atmos) in X and Y direction
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RL     phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr) 
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
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( k.EQ.ksurfW(i,j,bi,bj) ) THEN
           gUtmp(i,j) = gUtmp(i,j)
     &         *hFacW(i,j,k,bi,bj)/hFac_surfW(i,j,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif 

C     Step forward zonal velocity (store in Gu)
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gUNm1(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) 
     &     +deltaTmom*( 
     &         gUtmp(i,j)
     &       - psFac*phiSurfX(i,j)
     &        )*_maskW(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (staggerTimeStep) THEN
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
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( k.EQ.ksurfS(i,j,bi,bj) ) THEN
           gVtmp(i,j) = gVtmp(i,j)
     &         *hFacS(i,j,k,bi,bj)/hFac_surfS(i,j,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif 

C     Step forward meridional velocity (store in Gv)
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gVNm1(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &     +deltaTmom*(
     &         gVtmp(i,j)
     &       - psFac*phiSurfY(i,j)
     &        )*_maskS(i,j,k,bi,bj)
        ENDDO
      ENDDO

      IF (staggerTimeStep) THEN
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
