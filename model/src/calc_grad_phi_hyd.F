C $Header: /u/gcmpack/MITgcm/model/src/calc_grad_phi_hyd.F,v 1.2 2003/02/09 02:00:50 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GRAD_PHI_HYD
C     !INTERFACE:
      SUBROUTINE CALC_GRAD_PHI_HYD( 
     I                       k, bi, bj, iMin,iMax, jMin,jMax,
     I                       phiHyd, alphRho, tFld, sFld,
     O                       dPhiHydX, dPhiHydY,
     I                       myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_GRAD_PHI_HYD                                    
C     | o Calculate the gradient of Hydrostatic potential anomaly 
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj      :: tile index            
C     iMin,iMax,jMin,jMax :: Loop counters
C     phiHyd     :: Hydrostatic Potential anomaly 
C                  (atmos: =Geopotential ; ocean-z: =Pressure/rho)
C     alphRho    :: Density (z-coord) or specific volume (p-coord)
C     tFld       :: Potential temp.
C     sFld       :: Salinity 
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hyd. Potential
C     myTime :: Current time
C     myIter :: Current iteration number
C     myThid :: Instance number for this call of the routine.
      INTEGER k, bi,bj, iMin,iMax, jMin,jMax
      _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL alphRho(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL myTime
      INTEGER myIter, myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
      _RL varLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
CEOP

      DO j=jMin-1,jMax
       DO i=iMin-1,iMax
        varLoc(i,j) = phiHyd(i,j,k)+phi0surf(i,j,bi,bj)
c       varLoc(i,j) = phiHyd(i,j,k)
       ENDDO
      ENDDO

C-    Zonal gradient
      DO j=jMin,jMax
       DO i=iMin,iMax
        dPhiHydX(i,j) = _recip_dxC(i,j,bi,bj)
     &                *( varLoc(i,j)-varLoc(i-1,j) )
c    &              *( (phiHyd(i,j,k)-phiHyd(i-1,j,k)) 
c    &                +(phi0surf(i,j,bi,bj)-phi0surf(i-1,j,bi,bj))
c    &              *( (phiHyd(i,j,k)+phi0surf(i,j,bi,bj))
c    &                -(phiHyd(i-1,j,k)+phi0surf(i-1,j,bi,bj))
c    &               )
       ENDDO
      ENDDO

C-    Meridional gradient
      DO j=jMin,jMax
       DO i=iMin,iMax
        dPhiHydY(i,j) = _recip_dyC(i,j,bi,bj)
     &                *( varLoc(i,j)-varLoc(i,j-1) )
c    &              *( (phiHyd(i,j,k)-phiHyd(i,j-1,k)) 
c    &                +(phi0surf(i,j,bi,bj)-phi0surf(i,j-1,bi,bj))
c    &              *( (phiHyd(i,j,k)+phi0surf(i,j,bi,bj))
c    &                -(phiHyd(i,j-1,k)+phi0surf(i,j-1,bi,bj))
c    &               )
       ENDDO
      ENDDO

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
