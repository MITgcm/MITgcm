C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.32 2003/04/17 13:41:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP
C     !INTERFACE:
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax, k,
     I                     dPhiHydX,dPhiHydY, phiSurfX, phiSurfY,
     I                     myTime, myIter, myThid )
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
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hydrostatic Potential
C     phiSurfX :: gradient of Surface potential (Pressure/rho, ocean)
C     phiSurfY ::          or geopotential (atmos) in X and Y direction
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k
      _RL     dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      LOGICAL momForcing_In_AB
      INTEGER i,j
      _RL ab15,ab05
      _RL phxFac,phyFac, psFac
      _RL     gUtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gVtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef INCLUDE_CD_CODE
      _RL     guCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gvCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
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
        phxFac = pfFacMom
        phyFac = pfFacMom
      ELSE
        phxFac = 0.
        phyFac = 0.
      ENDIF 

C-- explicit part of the surface potential gradient is added in this S/R
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)

C-- including or excluding momentum forcing from Adams-Bashforth:
      momForcing_In_AB = forcing_In_AB
      momForcing_In_AB = .TRUE.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialize local arrays (not really necessary but safer)
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        gUtmp(i,j) = 0. _d 0
        gVtmp(i,j) = 0. _d 0
#ifdef INCLUDE_CD_CODE
        guCor(i,j) = 0. _d 0
        gvCor(i,j) = 0. _d 0
#endif
       ENDDO
      ENDDO

C--   Forcing term inside the Adams-Bashforth:
      IF (momForcing .AND. momForcing_In_AB) THEN
        CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
        CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
      ENDIF

C-    Compute effective gU,gV_[n+1/2] terms (including Adams-Bashforth weights)
C     and save gU,gV_[n] into guNm1,gvNm1 for the next time step.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gUtmp(i,j) = ab15*gU(i,j,k,bi,bj)
     &             + ab05*guNm1(i,j,k,bi,bj)  
        gVtmp(i,j) = ab15*gV(i,j,k,bi,bj)
     &             + ab05*gvNm1(i,j,k,bi,bj)  
        guNm1(i,j,k,bi,bj)= gU(i,j,k,bi,bj) 
        gvNm1(i,j,k,bi,bj)= gV(i,j,k,bi,bj)
        gU(i,j,k,bi,bj) = gUtmp(i,j)
        gV(i,j,k,bi,bj) = gVtmp(i,j)
       ENDDO
      ENDDO
      
C--   Forcing term outside the Adams-Bashforth:
C     (not recommanded with CD-scheme ON)
      IF (momForcing .AND. .NOT.momForcing_In_AB) THEN
        CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
        CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
       IF (useCDscheme) THEN
C-     for CD-scheme, compute gU,Vtmp = gU,V^n + forcing 
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gU(i,j,k,bi,bj)-gUtmp(i,j)
     &               + guNm1(i,j,k,bi,bj)
          gVtmp(i,j) = gV(i,j,k,bi,bj)-gVtmp(i,j)
     &               + gvNm1(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gU(i,j,k,bi,bj)
          gVtmp(i,j) = gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDIF
      ELSEIF ( useCDscheme) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = guNm1(i,j,k,bi,bj)
          gVtmp(i,j) = gvNm1(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#ifdef INCLUDE_CD_CODE
      IF (useCDscheme) THEN
C-     Step forward D-grid velocity using C-grid gU,Vtmp = gU,V^n + forcing
C      and return coriolis terms on C-grid (guCor,gvCor)
        CALL MOM_CDSCHEME(
     I                    bi,bj,k, dPhiHydX,dPhiHydY, gUtmp,gVtmp,
     O                    guCor,gvCor,
     I                    myTime, myIter, myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gU(i,j,k,bi,bj)
     &               + guCor(i,j)
          gVtmp(i,j) = gV(i,j,k,bi,bj)
     &               + gvCor(i,j)
         ENDDO
        ENDDO
      ENDIF
#endif /* INCLUDE_CD_CODE */

#ifdef NONLIN_FRSURF
      IF (.NOT. vectorInvariantMomentum 
     &    .AND. nonlinFreeSurf.GT.1) THEN
       IF (select_rStar.GT.0) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gUtmp(i,j)/rStarExpW(i,j,bi,bj)
           gVtmp(i,j) = gVtmp(i,j)/rStarExpS(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( k.EQ.ksurfW(i,j,bi,bj) ) THEN
           gUtmp(i,j) = gUtmp(i,j)
     &         *hFacW(i,j,k,bi,bj)/hFac_surfW(i,j,bi,bj)
          ENDIF
          IF ( k.EQ.ksurfS(i,j,bi,bj) ) THEN
           gVtmp(i,j) = gVtmp(i,j)
     &         *hFacS(i,j,k,bi,bj)/hFac_surfS(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* NONLIN_FRSURF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Step forward zonal velocity (store in Gu)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gU(i,j,k,bi,bj) = uVel(i,j,k,bi,bj) 
     &     +deltaTmom*( 
     &         gUtmp(i,j)
     &       - psFac*phiSurfX(i,j)
     &       - phxFac*dPhiHydX(i,j)
     &        )*_maskW(i,j,k,bi,bj)
        ENDDO
      ENDDO

C     Step forward meridional velocity (store in Gv)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gV(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &     +deltaTmom*(
     &         gVtmp(i,j)
     &       - psFac*phiSurfY(i,j)
     &       - phyFac*dPhiHydY(i,j)
     &        )*_maskS(i,j,k,bi,bj)
        ENDDO
      ENDDO

      RETURN
      END
