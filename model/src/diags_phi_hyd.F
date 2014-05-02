C $Header: /u/gcmpack/MITgcm/model/src/diags_phi_hyd.F,v 1.7 2014/05/02 16:57:43 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGS_PHI_HYD
C     !INTERFACE:
      SUBROUTINE DIAGS_PHI_HYD(
     I                       k, bi, bj, iMin,iMax, jMin,jMax,
     I                       phiHydC,
     I                       myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_PHI_HYD
C     | o Diagnose full hydrostatic Potential at cell center ;
C     |   used for output & with EOS funct. of P
C     *==========================================================*
C     | NOTE: For now, only contains the (total) Potential anomaly
C     |  since phiRef (for Atmos) is not available (not in common)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     k, bi,bj      :: level & tile indices
C     iMin,iMax,jMin,jMax :: Loop counters
C     phiHydC    :: hydrostatic potential anomaly at cell center
C                  (atmos: =Geopotential ; ocean-z: =Pressure/rho)
C     myTime :: Current time
C     myIter :: Current iteration number
C     myThid :: Instance number for this call of the routine.
      INTEGER k, bi,bj, iMin,iMax, jMin,jMax
      _RL phiHydC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL myTime
      INTEGER myIter, myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j        :: Loop counters
C     phiHydCstR :: total hydrostatic Potential (anomaly, for now),
C                   at fixed r-position, cell center level location.
      INTEGER i,j
#ifdef NONLIN_FRSURF
      _RL facP, dPhiRef
      _RL phiHydCstR(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* NONLIN_FRSURF */
CEOP

        DO j=jMin,jMax
         DO i=iMin,iMax
           totPhiHyd(i,j,k,bi,bj) = phiHydC(i,j)
     &            + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &            + phi0surf(i,j,bi,bj)
#ifdef NONLIN_FRSURF
           phiHydCstR(i,j) = totPhiHyd(i,j,k,bi,bj)
#endif /* NONLIN_FRSURF */
         ENDDO
        ENDDO

#ifdef NONLIN_FRSURF
c     IF (select_rStar.GE.2 .AND. nonlinFreeSurf.GE.4 ) THEN
      IF (select_rStar.GE.1 .AND. nonlinFreeSurf.GE.4 ) THEN
c# ifndef DISABLE_RSTAR_CODE
C-    Integral of b.dr = rStarFac * Integral of b.dr* :
       IF ( fluidIsAir ) THEN
C-     Consistent with Phi'= Integr[ theta'.dPi ] :
        DO j=jMin,jMax
         DO i=iMin,iMax
           facP = pStarFacK(i,j,bi,bj)
           dPhiRef = phiRef(2*k) - gravity*topoZ(i,j,bi,bj)
     &                           - phi0surf(i,j,bi,bj)
           totPhiHyd(i,j,k,bi,bj) =
     &              phiHydC(i,j)*facP
     &            + MAX( dPhiRef, 0. _d 0 )*( facP - 1. _d 0 )
     &            + phi0surf(i,j,bi,bj)
c          phiHydCstR(i,j) = phiHydCstR(i,j)
c    &            + phiHydC(i,j)*( facP - 1. _d 0 )
         ENDDO
        ENDDO
       ELSEIF ( usingPCoords ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
c    &     dPhiRef = phiRef(2*k) - gravity*topoZ(i,j,bi,bj)
c    &                           - phi0surf(i,j,bi,bj)
C--    assume PhiRef is just (ps0 - p)/rhoConst :
           dPhiRef =( Ro_surf(i,j,bi,bj)-rC(k) )*recip_rhoConst
           totPhiHyd(i,j,k,bi,bj) =
     &              phiHydC(i,j)*rStarFacC(i,j,bi,bj)
     &            + MAX( dPhiRef, 0. _d 0 )
     &                *( rStarFacC(i,j,bi,bj) - 1. _d 0 )
     &            + phi0surf(i,j,bi,bj)
c          totPhiHyd(i,j,k,bi,bj) = phiHydCstR(i,j)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
           dPhiRef =( Ro_surf(i,j,bi,bj)-rC(k) )*gravity
           totPhiHyd(i,j,k,bi,bj) =
     &              phiHydC(i,j)*rStarFacC(i,j,bi,bj)
     &            + MAX( dPhiRef, 0. _d 0 )
     &                *( rStarFacC(i,j,bi,bj) - 1. _d 0 )
     &            + phi0surf(i,j,bi,bj)
c          totPhiHyd(i,j,k,bi,bj) = phiHydCstR(i,j)
         ENDDO
        ENDDO
       ENDIF
#ifdef ALLOW_DIAGNOSTICS
C--    skip diagnostics if called from INI_PRESSURE
       IF ( useDiagnostics .AND. myIter.GE.0 ) THEN
        CALL DIAGNOSTICS_FILL(phiHydCstR,'PHIHYDcR',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

c# endif /* DISABLE_RSTAR_CODE */
      ENDIF
#endif /* NONLIN_FRSURF */

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
