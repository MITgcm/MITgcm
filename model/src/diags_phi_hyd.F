C $Header: /u/gcmpack/MITgcm/model/src/diags_phi_hyd.F,v 1.3 2011/06/10 20:54:54 jmc Exp $
C $Name:  $

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
C     i,j :: Loop counters
      INTEGER i,j
CEOP

#ifdef NONLIN_FRSURF
c     IF (select_rStar.GE.2 .AND. nonlinFreeSurf.GE.4 ) THEN
      IF (select_rStar.GE.1 .AND. nonlinFreeSurf.GE.4 ) THEN
c# ifndef DISABLE_RSTAR_CODE
C-    Integral of b.dr = rStarFac * Integral of b.dr* :
       IF ( buoyancyRelation .EQ. 'ATMOSPHERIC' ) THEN
C-     Consistent with Phi'= Integr[ theta'.dPi ] :
        DO j=jMin,jMax
         DO i=iMin,iMax
           totPhiHyd(i,j,k,bi,bj) =
     &              phiHydC(i,j)*rStarFacC(i,j,bi,bj)**atm_kappa
     &            + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &            + phi0surf(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
           totPhiHyd(i,j,k,bi,bj) =
     &              phiHydC(i,j)*rStarFacC(i,j,bi,bj)
     &            + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &            + phi0surf(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
c# endif /* DISABLE_RSTAR_CODE */
      ELSE
#else /* NONLIN_FRSURF */
      IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
        DO j=jMin,jMax
         DO i=iMin,iMax
           totPhiHyd(i,j,k,bi,bj) = phiHydC(i,j)
     &            + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &            + phi0surf(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
