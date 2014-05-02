C $Header: /u/gcmpack/MITgcm/model/src/diags_phi_rlow.F,v 1.11 2014/05/02 16:57:43 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGS_PHI_RLOW
C     !INTERFACE:
      SUBROUTINE DIAGS_PHI_RLOW(
     I                       k, bi, bj, iMin,iMax, jMin,jMax,
     I                       phiHydF, phiHydC, alphRho, tFld, sFld,
     I                       myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_PHI_RLOW
C     | o Diagnose Phi-Hydrostatic at r-lower boundary
C     |   = bottom pressure (ocean in z-coord) ;
C     |   = sea surface elevation (ocean in p-coord) ;
C     |   = height at the top of atmosphere (in p-coord) ;
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
C     bi,bj      :: tile index
C     iMin,iMax,jMin,jMax :: Loop counters
C     phiHydF    :: hydrostatic potential anomaly at middle between
C                   2 centers k & k+1 (interface k+1)
C     phiHydC    :: hydrostatic potential anomaly at cell center
C                  (atmos: =Geopotential ; ocean-z: =Pressure/rho)
C     alphRho    :: Density (z-coord) or specific volume (p-coord)
C     tFld       :: Potential temp.
C     sFld       :: Salinity
C     myTime     :: Current time
C     myIter     :: Current iteration number
C     myThid     :: my Thread Id number
      INTEGER k, bi,bj, iMin,iMax, jMin,jMax
      _RL phiHydF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiHydC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL alphRho(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
      _RL ddRloc, ratioRm, ratioRp
#ifdef NONLIN_FRSURF
      _RL facP, dPhiRef
#endif /* NONLIN_FRSURF */
CEOP

      IF ( usingZCoords ) THEN

C----- Compute bottom pressure deviation from gravity*rho0*H
C      Start from phiHyd at the (bottom) tracer point and add Del_h*g*rho_prime
C      with Del_h = distance from the bottom up to tracer point

C--    Initialise to zero (otherwise phi0surf accumulate over land)
       IF ( k.EQ.1 ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            phiHydLow(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
       ENDIF

       IF (integr_GeoPot.EQ.1) THEN
C  --  Finite Volume Form

         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( k .EQ. kLowC(i,j,bi,bj) ) THEN
             ddRloc = rC(k)-R_low(i,j,bi,bj)
             phiHydLow(i,j,bi,bj) = phiHydC(i,j)
     &            + ddRloc*gravity*alphRho(i,j)*recip_rhoConst
           ENDIF
          ENDDO
         ENDDO

       ELSE
C  --  Finite Difference Form

         ratioRm = oneRL
         ratioRp = oneRL
         IF (k.GT.1 ) ratioRm = halfRL*drC(k)/(rF(k)-rC(k))
         IF (k.LT.Nr) ratioRp = halfRL*drC(k+1)/(rC(k)-rF(k+1))

         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( k .EQ. kLowC(i,j,bi,bj) ) THEN
             ddRloc = rC(k)-R_low(i,j,bi,bj)
             phiHydLow(i,j,bi,bj) = phiHydC(i,j)
     &                  +( MIN(zeroRL,ddRloc)*ratioRm
     &                    +MAX(zeroRL,ddRloc)*ratioRp
     &                   )*gravity*alphRho(i,j)*recip_rhoConst
           ENDIF
          ENDDO
         ENDDO

C  --  end if integr_GeoPot = ...
       ENDIF

C  -- end usingZCoords
      ENDIF

      IF ( k.EQ.Nr ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C  --  last level (bottom): rescale (r*) and add surface contribution

       IF ( usingPCoords ) THEN
C  -- P coordinate : Phi(R_low) is simply at the top :
        DO j=jMin,jMax
         DO i=iMin,iMax
           phiHydLow(i,j,bi,bj) = phiHydF(i,j)
         ENDDO
        ENDDO
       ENDIF

#ifdef NONLIN_FRSURF
c      IF ( select_rStar.GE.2 .AND. nonlinFreeSurf.GE.4 ) THEN
       IF ( select_rStar.GE.1 .AND. nonlinFreeSurf.GE.4 ) THEN
C-    Integral of b.dr = rStarFac * Integral of b.dr* :
        IF ( fluidIsAir ) THEN
C-     Consistent with Phi'= Integr[ theta'.dPi ] :
         DO j=jMin,jMax
          DO i=iMin,iMax
           facP = pStarFacK(i,j,bi,bj)
           dPhiRef = phiRef(2*k+1) - gravity*topoZ(i,j,bi,bj)
     &                             - phi0surf(i,j,bi,bj)
           phiHydLow(i,j,bi,bj) =
     &              phiHydLow(i,j,bi,bj)*facP
     &            + MAX( dPhiRef, zeroRL )*( facP - 1. _d 0 )
     &            + phi0surf(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSEIF ( usingPCoords ) THEN
C- Note: dPhiRef*(rStarFacC -1) = 1/rho*PSo*((eta+PSo)/PSo -1) = Bo_surf*etaN
C     so that this expression is the same as before (same as in "ELSE" block below)
         DO j=jMin,jMax
          DO i=iMin,iMax
           dPhiRef = ( Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj) )
     &              *recip_rhoConst
           phiHydLow(i,j,bi,bj) =
     &              phiHydLow(i,j,bi,bj)*rStarFacC(i,j,bi,bj)
     &            + dPhiRef*( rStarFacC(i,j,bi,bj) - 1. _d 0 )
     &            + phi0surf(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
C- Note: dPhiRef*(rStarFacC -1) = g*H*((eta+H)/H -1) = Bo_surf*etaN
C     so that this expression is the same as before (same as in "ELSE" block below)
         DO j=jMin,jMax
          DO i=iMin,iMax
           dPhiRef = ( Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj) )
     &              *gravity
           phiHydLow(i,j,bi,bj) =
     &              phiHydLow(i,j,bi,bj)*rStarFacC(i,j,bi,bj)
     &            + dPhiRef*( rStarFacC(i,j,bi,bj) - 1. _d 0 )
     &            + phi0surf(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
       ELSE
#else /* NONLIN_FRSURF */
       IF ( .TRUE. ) THEN
#endif /* NONLIN_FRSURF */

         DO j=jMin,jMax
          DO i=iMin,iMax
           phiHydLow(i,j,bi,bj) = phiHydLow(i,j,bi,bj)
     &            + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &            + phi0surf(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C  -- end if k=Nr.
      ENDIF

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
