C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.40 2005/09/29 16:47:16 baylor Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP
C     !INTERFACE:
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax, k,
     I                     dPhiHydX,dPhiHydY, phiSurfX, phiSurfY,
     I                     guDissip, gvDissip,
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
C     guDissip :: dissipation tendency (all explicit terms), u component
C     gvDissip :: dissipation tendency (all explicit terms), v component

      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k
      _RL     dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     guDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gvDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      LOGICAL momForcing_In_AB
      LOGICAL momDissip_In_AB
      INTEGER i,j
      _RL ab15,ab05
      _RL phxFac,phyFac, psFac
      _RL     gUtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gVtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C    Allow diagnosis of external forcing
      _RL     gUext(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gVext(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_CD_CODE
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

C-- explicit part of the surface potential gradient is added in this S/R
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)

C--  factors for gradient (X & Y directions) of Hydrostatic Potential
      phxFac = pfFacMom
      phyFac = pfFacMom

C-- including or excluding momentum forcing from Adams-Bashforth:
      momForcing_In_AB = forcing_In_AB
      momForcing_In_AB = .TRUE.
      momDissip_In_AB  = .TRUE.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialize local arrays (not really necessary but safer)
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        gUtmp(i,j) = 0. _d 0
        gVtmp(i,j) = 0. _d 0
        gUext(i,j) = 0. _d 0
        gVext(i,j) = 0. _d 0
#ifdef ALLOW_CD_CODE
        guCor(i,j) = 0. _d 0
        gvCor(i,j) = 0. _d 0
#endif
       ENDDO
      ENDDO

      IF ( .NOT.staggerTimeStep ) THEN
C--   Synchronous time step: add grad Phi_Hyp to gU,gV before doing Adams-Bashforth
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) - phxFac*dPhiHydX(i,j)
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) - phyFac*dPhiHydY(i,j)
         ENDDO
        ENDDO
        phxFac = 0.
        phyFac = 0.
c     ELSE
C--   Stagger time step: grad Phi_Hyp will be added later
      ENDIF 

C--   Dissipation term inside the Adams-Bashforth:
      IF ( momViscosity .AND. momDissip_In_AB) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) + guDissip(i,j)
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) + gvDissip(i,j)
         ENDDO
        ENDDO
      ENDIF

C--   Forcing term inside the Adams-Bashforth:
      IF (momForcing .AND. momForcing_In_AB) THEN
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gUext(i,j) = gU(i,j,k,bi,bj)
           gVext(i,j) = gV(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
        CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gUext(i,j) = gU(i,j,k,bi,bj)-gUext(i,j)
           gVext(i,j) = gV(i,j,k,bi,bj)-gVext(i,j)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ENDIF

      IF (useCDscheme) THEN
C-     for CD-scheme, store gU,Vtmp = gU,V^n + forcing 
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gU(i,j,k,bi,bj)
          gVtmp(i,j) = gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

C-    Compute effective gU,gV_[n+1/2] terms (including Adams-Bashforth weights)
C     and save gU,gV_[n] into guNm1,gvNm1 for the next time step.
#ifdef ALLOW_ADAMSBASHFORTH_3
      CALL ADAMS_BASHFORTH3(
     I                        bi, bj, k,
     U                        gU, guNm,
     I                        myIter, myThid )
      CALL ADAMS_BASHFORTH3(
     I                        bi, bj, k,
     U                        gV, gvNm,
     I                        myIter, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
      CALL ADAMS_BASHFORTH2(
     I                        bi, bj, k,
     U                        gU, guNm1,
     I                        myIter, myThid )
      CALL ADAMS_BASHFORTH2(
     I                        bi, bj, k,
     U                        gV, gvNm1,
     I                        myIter, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      
C--   Forcing term outside the Adams-Bashforth:
C     (not recommended with CD-scheme ON)
      IF (momForcing .AND. .NOT.momForcing_In_AB) THEN
       IF (useCDscheme) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gUtmp(i,j) - gU(i,j,k,bi,bj)
          gVtmp(i,j) = gVtmp(i,j) - gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDIF
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUext(i,j) = gU(i,j,k,bi,bj)
          gVext(i,j) = gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
        CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUext(i,j) = gU(i,j,k,bi,bj)-gUext(i,j)
          gVext(i,j) = gV(i,j,k,bi,bj)-gVext(i,j)
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(gUext,'Um_Ext  ',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(gVext,'Vm_Ext  ',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-     for CD-scheme, compute gU,Vtmp = gU,V^n + forcing 
       IF (useCDscheme) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gUtmp(i,j) + gU(i,j,k,bi,bj)
          gVtmp(i,j) = gVtmp(i,j) + gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDIF
      ENDIF

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
C-     Step forward D-grid velocity using C-grid gU,Vtmp = gU,V^n + forcing
C      and return coriolis terms on C-grid (guCor,gvCor)
        CALL CD_CODE_SCHEME(
     I                  bi,bj,k, dPhiHydX,dPhiHydY, gUtmp,gVtmp,
     O                  guCor,gvCor,
     I                  myTime, myIter, myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gU(i,j,k,bi,bj)
     &               + guCor(i,j)
          gVtmp(i,j) = gV(i,j,k,bi,bj)
     &               + gvCor(i,j)
         ENDDO
        ENDDO
      ELSE
#endif /* ALLOW_CD_CODE */
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gU(i,j,k,bi,bj)
          gVtmp(i,j) = gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_CD_CODE
      ENDIF
#endif

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

C--   Dissipation term outside the Adams-Bashforth:
      IF ( momViscosity .AND. .NOT.momDissip_In_AB ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gUtmp(i,j) + guDissip(i,j)
          gVtmp(i,j) = gVtmp(i,j) + gvDissip(i,j)
         ENDDO
        ENDDO
      ENDIF

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

#ifdef ALLOW_DIAGNOSTICS
#ifdef ALLOW_CD_CODE
      IF ( useCDscheme .AND. useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL(guCor,'Um_Cori ',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(gvCor,'Vm_Cori ',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_CD_CODE */
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
